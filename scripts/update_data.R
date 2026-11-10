#!/usr/bin/env Rscript
# =============================================================================
# Career Compass :: weekly data check + rebuild
# -----------------------------------------------------------------------------
# Sources (all public, U.S. Bureau of Labor Statistics):
#   OEWS "All data" file            -> pay percentiles + job counts for the U.S.,
#                                      every state/territory, every metro and
#                                      nonmetro area (~800 careers)
#   Employment Projections table 1.2 -> 10-year growth, yearly openings, and the
#                                      typical education / experience / training
#
# Each run:
#   1. Finds the newest OEWS release on bls.gov
#   2. Asks BLS whether either file changed since the last run
#   3. Only if something changed (or FORCE_REBUILD=true): downloads, cleans,
#      and writes data/careers.rds for the Shiny app
#   4. Always writes data/last_checked.json so the site can show "last checked"
#
# Run locally from the project folder:  Rscript scripts/update_data.R
# Force a rebuild:                      FORCE_REBUILD=true Rscript scripts/update_data.R
# =============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a)) || identical(a, "")) b else a

FORCE   <- tolower(Sys.getenv("FORCE_REBUILD", "false")) == "true"
CONTACT <- Sys.getenv("BLS_CONTACT", "")
if (!nzchar(CONTACT)) CONTACT <- "set-BLS_CONTACT-to-your-email@example.com"
# BLS blocks anonymous scripts. Identifying yourself with an email is what they ask for.
UA <- sprintf("CareerCompass/1.0 (%s)", CONTACT)

DATA_DIR   <- "data"
OUT_PATH   <- file.path(DATA_DIR, "careers.rds")
META_PATH  <- file.path(DATA_DIR, "meta.json")
CHECK_PATH <- file.path(DATA_DIR, "last_checked.json")
TMP        <- file.path(tempdir(), "bls")
dir.create(DATA_DIR, showWarnings = FALSE)
dir.create(TMP, showWarnings = FALSE, recursive = TRUE)

OEWS_TABLES <- "https://www.bls.gov/oes/tables.htm"
OEWS_ZIP    <- function(yy) sprintf("https://www.bls.gov/oes/special-requests/oesm%02dall.zip", yy)
PROJ_URL    <- "https://www.bls.gov/emp/ind-occ-matrix/occupation.xlsx"

# BLS caps the very highest wages. "#" in the file means "at or above" these.
TOPCODE_ANNUAL <- 239200
TOPCODE_HOURLY <- 115

# ---- HTTP helpers -----------------------------------------------------------
bls_req <- function(url) {
  request(url) |>
    req_user_agent(UA) |>
    req_headers(Accept = "*/*", `Accept-Language` = "en-US,en;q=0.9") |>
    req_timeout(900) |>
    req_retry(max_tries = 4, backoff = function(i) 15 * i) |>
    req_error(is_error = function(resp) FALSE)
}

is_html <- function(resp) grepl("text/html", resp_header(resp, "content-type") %||% "", fixed = TRUE)

head_info <- function(url) {
  resp <- tryCatch(bls_req(url) |> req_method("HEAD") |> req_perform(), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200 || is_html(resp)) return(NULL)
  list(fingerprint = paste(resp_header(resp, "last-modified") %||% "",
                           resp_header(resp, "etag") %||% "",
                           resp_header(resp, "content-length") %||% "", sep = "|"))
}

download <- function(url, dest) {
  resp <- bls_req(url) |> req_perform(path = dest)
  if (resp_status(resp) != 200) {
    stop("BLS returned HTTP ", resp_status(resp), " for ", url,
         ". A 403 usually means BLS wants a contact email: set the BLS_CONTACT secret.")
  }
  if (is_html(resp)) stop("BLS sent a web page instead of a data file for ", url)
  dest
}

md5 <- function(path) unname(tools::md5sum(path))

latest_oews_url <- function() {
  page <- tryCatch({
    resp <- bls_req(OEWS_TABLES) |> req_perform()
    if (resp_status(resp) == 200) resp_body_string(resp) else ""
  }, error = function(e) "")
  yys <- suppressWarnings(as.integer(str_match_all(page, "oesm(\\d{2})all\\.zip")[[1]][, 2]))
  yys <- yys[!is.na(yys)]
  if (length(yys)) return(OEWS_ZIP(max(yys)))
  # Fallback: probe the last few years directly
  this_yy <- as.integer(format(Sys.Date(), "%y"))
  for (yy in this_yy:(this_yy - 3)) if (!is.null(head_info(OEWS_ZIP(yy)))) return(OEWS_ZIP(yy))
  stop("Couldn't find an OEWS 'all data' file on bls.gov. If BLS returned 403, set the BLS_CONTACT secret.")
}

# ---- Parsing helpers ----------------------------------------------------------
num <- function(x) suppressWarnings(as.numeric(gsub("[,$[:space:]]", "", x)))
num_wage <- function(x, cap) {
  v <- num(x)
  v[!is.na(x) & grepl("#", x, fixed = TRUE)] <- cap
  v
}
clean_txt <- function(x) {
  x <- str_squish(str_replace_all(x, "[\u2018\u2019]", "'"))
  x[x %in% c("", "\u2014", "-", "NA")] <- NA_character_
  x
}

read_projections <- function(path) {
  sheets <- excel_sheets(path)
  # Find the sheet that has both education requirements and openings (table 1.2)
  pick_sheet <- NA_character_
  for (sh in sheets) {
    top <- suppressMessages(read_excel(path, sheet = sh, col_names = FALSE, n_max = 8, col_types = "text"))
    txt <- paste(unlist(top), collapse = " ")
    if (grepl("Typical education", txt, ignore.case = TRUE) && grepl("openings", txt, ignore.case = TRUE)) {
      pick_sheet <- sh; break
    }
  }
  if (is.na(pick_sheet)) stop("Couldn't find projections table 1.2 in ", basename(path))

  raw <- suppressMessages(read_excel(path, sheet = pick_sheet, col_names = FALSE, col_types = "text"))
  hdr <- which(apply(raw, 1, function(r) any(grepl("code", r, ignore.case = TRUE)) &&
                                         any(grepl("title", r, ignore.case = TRUE))))[1]
  if (is.na(hdr)) stop("Couldn't find the header row in sheet ", pick_sheet)
  nms <- str_squish(as.character(unlist(raw[hdr, ])))
  nms[is.na(nms) | nms == ""] <- paste0("x", which(is.na(nms) | nms == ""))
  df <- raw[-seq_len(hdr), ]
  names(df) <- make.unique(nms)

  pick <- function(pattern, n = 1) {
    hit <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    if (length(hit) < n) NA_character_ else hit[n]
  }
  col <- list(
    title      = pick("title"),
    soc        = pick("code"),
    type       = pick("occupation type"),
    emp_base   = pick("^employment,? \\d{4}", 1),
    emp_proj   = pick("^employment,? \\d{4}", 2),
    chg_pct    = pick("change, percent|percent change"),
    openings   = pick("openings"),
    education  = pick("typical education"),
    experience = pick("work experience"),
    training   = pick("on-the-job training")
  )
  must <- c("soc", "emp_base", "emp_proj", "chg_pct", "openings", "education")
  missing <- must[is.na(unlist(col[must]))]
  if (length(missing)) stop("Projections table is missing columns: ", paste(missing, collapse = ", "),
                            "\nColumns found: ", paste(names(df), collapse = " | "))
  getc <- function(k) if (is.na(col[[k]])) rep(NA_character_, nrow(df)) else df[[col[[k]]]]

  out <- tibble(
    soc          = str_squish(getc("soc")),
    occ_type     = getc("type"),
    emp_base     = num(getc("emp_base")) * 1000,   # table is in thousands
    emp_proj     = num(getc("emp_proj")) * 1000,
    growth_pct   = num(getc("chg_pct")),
    openings_nat = num(getc("openings")) * 1000,   # thousands per year
    education    = clean_txt(getc("education")),
    experience   = clean_txt(getc("experience")),
    training     = clean_txt(getc("training"))
  ) |> filter(!is.na(soc))

  yrs <- str_extract(c(col$emp_base, col$emp_proj), "\\d{4}")
  list(data = out, period = paste(yrs, collapse = "\u20132"), sheet = pick_sheet)
}

read_oews <- function(zip_path) {
  inside <- unzip(zip_path, list = TRUE)$Name
  xl <- inside[grepl("\\.xlsx$", inside, ignore.case = TRUE) & grepl("all_data", inside, ignore.case = TRUE)][1]
  if (is.na(xl)) xl <- inside[grepl("\\.xlsx$", inside, ignore.case = TRUE)][1]
  if (is.na(xl)) stop("No spreadsheet inside ", basename(zip_path))
  unzip(zip_path, files = xl, exdir = TMP, overwrite = TRUE)
  df <- read_excel(file.path(TMP, xl), col_types = "text")
  names(df) <- tolower(names(df))
  df
}

# ---- Build the app dataset -------------------------------------------------------
build <- function(oews_zip, proj_xlsx, oews_url) {
  message("Reading projections...")
  pj <- read_projections(proj_xlsx)
  avg_growth <- pj$data$growth_pct[pj$data$soc == "00-0000"][1]
  proj <- pj$data |>
    filter(str_detect(soc, "^\\d{2}-\\d{4}$"), soc != "00-0000",
           is.na(occ_type) | str_detect(tolower(occ_type), "line")) |>
    distinct(soc, .keep_all = TRUE)
  message("  ", nrow(proj), " detailed occupations, period ", pj$period, ", all-jobs growth ", avg_growth, "%")

  message("Reading OEWS (big file, give it a minute)...")
  raw <- read_oews(oews_zip)
  need <- c("area", "area_title", "area_type", "occ_code", "occ_title", "o_group", "tot_emp", "a_median")
  miss <- setdiff(need, names(raw))
  if (length(miss)) stop("OEWS file is missing columns: ", paste(miss, collapse = ", "))
  for (nm in c("prim_state", "i_group", "own_code", "jobs_1000", "loc_quotient", "h_median", "a_mean",
               "a_pct10", "a_pct25", "a_pct75", "a_pct90", "hourly")) {
    if (!nm %in% names(raw)) raw[[nm]] <- NA_character_
  }

  o <- raw |>
    mutate(area_type = as.integer(num(area_type))) |>
    filter(area_type %in% c(1L, 2L, 3L, 4L, 6L),
           is.na(i_group) | tolower(i_group) == "cross-industry")
  if (any(o$own_code == "1235", na.rm = TRUE)) o <- o |> filter(is.na(own_code) | own_code == "1235")
  rm(raw); invisible(gc())

  st_lookup <- c(setNames(state.abb, state.name), "District of Columbia" = "DC", "Puerto Rico" = "PR",
                 "Guam" = "GU", "Virgin Islands" = "VI", "U.S. Virgin Islands" = "VI")

  areas <- o |>
    distinct(area, .keep_all = TRUE) |>
    transmute(
      area, area_type,
      area_title = str_squish(area_title),
      prim_state = na_if(str_squish(prim_state), ""),
      kind = case_when(area_type == 1L ~ "Nation",
                       area_type %in% c(2L, 3L) ~ "State",
                       area_type == 4L ~ "Metro area",
                       TRUE ~ "Nonmetro area")
    ) |>
    mutate(
      state_abbr = ifelse(kind == "State", coalesce(prim_state, unname(st_lookup[area_title])), NA_character_),
      from_title = str_match(area_title, ",\\s*([A-Z]{2}(?:-[A-Z]{2})*)\\s*$")[, 2],
      states = case_when(kind == "State" ~ state_abbr,
                         kind == "Metro area" & !is.na(from_title) ~ str_replace_all(from_title, "-", "|"),
                         TRUE ~ prim_state)
    ) |>
    select(-from_title)

  all_med <- o |>
    filter(occ_code == "00-0000") |>
    transmute(area, all_median = as.integer(round(num_wage(a_median, TOPCODE_ANNUAL)))) |>
    distinct(area, .keep_all = TRUE)
  areas <- left_join(areas, all_med, by = "area")

  aw <- function(x) as.integer(round(num_wage(x, TOPCODE_ANNUAL)))
  wages <- o |>
    filter(tolower(o_group) == "detailed") |>
    transmute(
      area, soc = occ_code,
      emp       = as.integer(round(num(tot_emp))),
      jobs_1000 = round(num(jobs_1000), 2),
      lq        = round(num(loc_quotient), 2),
      a_p10 = aw(a_pct10), a_p25 = aw(a_pct25), a_median = aw(a_median),
      a_p75 = aw(a_pct75), a_p90 = aw(a_pct90), a_mean = aw(a_mean),
      h_median  = round(num_wage(h_median, TOPCODE_HOURLY), 2),
      hourly_only = toupper(coalesce(hourly, "")) %in% c("TRUE", "T", "1")
    ) |>
    filter(!(is.na(emp) & is.na(a_median) & is.na(h_median))) |>
    distinct(area, soc, .keep_all = TRUE)

  nat_code <- areas$area[areas$kind == "Nation"][1]
  occ <- o |>
    filter(area == nat_code, tolower(o_group) == "detailed") |>
    distinct(occ_code, .keep_all = TRUE) |>
    transmute(soc = occ_code, title = str_squish(occ_title), field = substr(occ_code, 1, 2)) |>
    left_join(wages |> filter(area == nat_code) |> select(soc, nat_emp = emp, nat_median = a_median), by = "soc") |>
    left_join(proj |> select(soc, growth_pct, openings_nat, education, experience, training), by = "soc")

  message(sprintf("  %d areas (%d states, %d metros, %d nonmetro), %d careers, %d wage rows",
                  nrow(areas), sum(areas$kind == "State"), sum(areas$kind == "Metro area"),
                  sum(areas$kind == "Nonmetro area"), nrow(occ), nrow(wages)))
  message(sprintf("  Projections matched for %d of %d careers", sum(!is.na(occ$growth_pct)), nrow(occ)))

  yy <- as.integer(str_match(oews_url, "oesm(\\d{2})all")[, 2])
  meta <- list(
    oews_year = 2000L + yy,
    oews_label = paste("May", 2000L + yy),
    proj_period = pj$period,
    avg_growth = avg_growth,
    topcode_annual = TOPCODE_ANNUAL,
    built_at = format(Sys.time(), "%Y-%m-%d", tz = "America/New_York")
  )
  saveRDS(list(occ = occ, areas = areas, wages = wages, meta = meta), OUT_PATH, compress = "xz")
  message("Wrote ", OUT_PATH, " (", round(file.size(OUT_PATH) / 1e6, 1), " MB)")
  meta
}

# ---- Main ---------------------------------------------------------------------
message("Checking BLS for updates: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z", tz = "America/New_York"))
old_meta <- if (file.exists(META_PATH)) fromJSON(META_PATH, simplifyVector = FALSE) else list()
old_src  <- old_meta$sources
if (is.null(old_src)) old_src <- list()

urls <- list(oews = latest_oews_url(), projections = PROJ_URL)
message("Newest OEWS file: ", basename(urls$oews))

files <- list(); new_src <- list(); changed <- FALSE
for (nm in names(urls)) {
  old <- old_src[[nm]]
  h <- head_info(urls[[nm]])
  fp <- if (is.null(h)) "" else h$fingerprint
  same_headers <- !is.null(old) && identical(old$url, urls[[nm]]) &&
    nzchar(gsub("|", "", fp, fixed = TRUE)) && identical(old$fingerprint, fp)
  if (same_headers && file.exists(OUT_PATH) && !FORCE) {
    message(nm, ": unchanged")
    new_src[[nm]] <- old
    next
  }
  dest <- file.path(TMP, basename(urls[[nm]]))
  message(nm, ": downloading ", urls[[nm]])
  download(urls[[nm]], dest)
  files[[nm]] <- dest
  hash <- md5(dest)
  new_src[[nm]] <- list(url = urls[[nm]], fingerprint = fp, md5 = hash,
                        downloaded_at = format(Sys.time(), "%Y-%m-%d", tz = "America/New_York"))
  if (is.null(old) || !identical(old$md5, hash)) {
    changed <- TRUE
    message(nm, ": NEW DATA")
  } else {
    message(nm, ": same contents as last time")
  }
}
if (!file.exists(OUT_PATH)) changed <- TRUE

write_json(list(checked_at = format(Sys.time(), "%b %d, %Y", tz = "America/New_York"),
                new_data = changed || FORCE,
                oews_file = basename(urls$oews)),
           CHECK_PATH, auto_unbox = TRUE, pretty = TRUE)

if (!changed && !FORCE) {
  message("No new BLS data this week. Nothing to rebuild.")
  quit(save = "no", status = 0)
}

# Need both files to rebuild; grab whichever we skipped
for (nm in names(urls)) {
  if (is.null(files[[nm]])) {
    dest <- file.path(TMP, basename(urls[[nm]]))
    message(nm, ": downloading for rebuild")
    download(urls[[nm]], dest)
    files[[nm]] <- dest
    new_src[[nm]]$md5 <- md5(dest)
  }
}

meta <- build(files$oews, files$projections, urls$oews)
write_json(c(meta, list(sources = new_src)), META_PATH, auto_unbox = TRUE, pretty = TRUE)
message("Done. Rebuilt with ", meta$oews_label, " wages and ", meta$proj_period, " projections.")
