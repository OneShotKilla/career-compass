# =============================================================================
# Career Compass
# Helps students and young adults find in-demand careers where they live.
#
# Data (all public, U.S. Bureau of Labor Statistics):
#   - OEWS: pay percentiles + job counts for the U.S., every state, and every
#     metro / nonmetro area
#   - Employment Projections: 10-year growth, yearly openings, and the typical
#     education / experience / training path for each career
#
# data/careers.rds is rebuilt by scripts/update_data.R, which GitHub Actions
# runs every Monday. Run that script once before running this app locally.
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(reactable)
library(htmltools)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ---- Data -------------------------------------------------------------------
if (!file.exists("data/careers.rds")) {
  stop("data/careers.rds not found. Run scripts/update_data.R first (or the GitHub Action).")
}
DAT        <- readRDS("data/careers.rds")
META       <- DAT$meta
AREAS      <- DAT$areas
WAGES_BY_AREA <- split(DAT$wages, DAT$wages$area)
NAT_CODE   <- AREAS$area[AREAS$kind == "Nation"][1]
AVG_GROWTH <- META$avg_growth %||% 3.5
TOP_A      <- META$topcode_annual %||% 239200
LAST_CHECKED <- tryCatch(fromJSON("data/last_checked.json")$checked_at, error = function(e) NA)

STATES <- AREAS[AREAS$kind == "State" & !is.na(AREAS$state_abbr), ]
STATES <- STATES[order(STATES$area_title), ]
STATE_CHOICES <- setNames(STATES$state_abbr, STATES$area_title)
DEFAULT_STATE <- if ("NY" %in% STATES$state_abbr) "NY" else STATES$state_abbr[1]

# ---- Fields (BLS major occupation groups, renamed for humans) ----------------
FIELDS <- data.frame(
  code = c("11", "13", "15", "17", "19", "21", "23", "25", "27", "29", "31",
           "33", "35", "37", "39", "41", "43", "45", "47", "49", "51", "53"),
  name = c("Management", "Business & Finance", "Computers & Tech",
           "Engineering & Architecture", "Science & Research",
           "Community & Social Services", "Law & Legal Services",
           "Education & Libraries", "Arts, Media & Sports",
           "Healthcare: Clinical & Technical", "Healthcare Support",
           "Law Enforcement, Fire & Security", "Food & Hospitality",
           "Building & Grounds", "Personal Care & Services", "Sales",
           "Office & Admin Support", "Farming, Fishing & Forestry",
           "Construction & Trades", "Installation, Maintenance & Repair",
           "Manufacturing & Production", "Transportation & Logistics"),
  emoji = c("\U0001F4CA", "\U0001F4BC", "\U0001F4BB", "\U0001F4D0", "\U0001F52C",
            "\U0001F91D", "\u2696\uFE0F", "\U0001F393", "\U0001F3A8", "\U0001FA7A",
            "\U0001FA79", "\U0001F693", "\U0001F373", "\U0001F9F9", "\U0001F487",
            "\U0001F6CD\uFE0F", "\U0001F5C2\uFE0F", "\U0001F33E", "\U0001F3D7\uFE0F",
            "\U0001F527", "\U0001F3ED", "\U0001F69A"),
  blurb = c("Lead teams, projects and organizations",
            "Money, markets, HR and operations",
            "Software, data, IT and cybersecurity",
            "Design and build the physical world",
            "Labs, field research and analysis",
            "Counseling, social work and ministry",
            "Lawyers, paralegals and court staff",
            "Teaching from preschool to college",
            "Design, media, performance and sports",
            "Nurses, doctors, therapists, technologists",
            "Aides, assistants and patient care",
            "Police, fire, corrections and security",
            "Cooks, chefs, servers and hospitality",
            "Facilities, cleaning and landscaping",
            "Childcare, fitness, beauty, recreation",
            "Retail, sales reps and real estate",
            "Office, records and customer service",
            "Agriculture, fishing and forests",
            "Electricians, carpenters, plumbers and more",
            "Mechanics, technicians and repairers",
            "Machinists, welders, assemblers",
            "Drivers, pilots, logistics and moving"),
  stringsAsFactors = FALSE
)
FIELD_NAME  <- setNames(FIELDS$name, FIELDS$code)
FIELD_EMOJI <- setNames(FIELDS$emoji, FIELDS$code)
FIELD_CHOICES <- c("All fields" = "all", setNames(FIELDS$code, paste(FIELDS$emoji, FIELDS$name)))

OCC <- DAT$occ[DAT$occ$field %in% FIELDS$code, ]

# ---- Education & training language ------------------------------------------
EDU_LEVELS <- c("No formal educational credential", "High school diploma or equivalent",
                "Some college, no degree", "Postsecondary nondegree award",
                "Associate's degree", "Bachelor's degree", "Master's degree",
                "Doctoral or professional degree")
EDU_SHORT <- c("No diploma needed", "High school diploma", "Some college", "Certificate",
               "Associate's", "Bachelor's", "Master's", "Doctorate / professional")
EDU_EXPLAIN <- c(
  "You can start without a diploma. Employers train you.",
  "A high school diploma or GED gets you in the door.",
  "Some college courses help, but no degree is required.",
  "A certificate from a community college or trade school, often a year or less.",
  "A 2-year degree, usually from a community college.",
  "A 4-year college degree.",
  "A graduate degree, 1 to 2 years after a bachelor's.",
  "A doctoral or professional degree (MD, JD, PhD, PharmD), 3+ years after a bachelor's.")
EXP_EXPLAIN <- c(
  "None" = "No prior work experience required.",
  "Less than 5 years" = "Usually needs some related work experience first (under 5 years).",
  "5 years or more" = "Usually needs 5+ years of related experience, so plan a stepping-stone job.")
TRAIN_EXPLAIN <- c(
  "None" = "No formal training after you're hired.",
  "Short-term on-the-job training" = "About a month or less of training on the job.",
  "Moderate-term on-the-job training" = "1 to 12 months of training on the job.",
  "Long-term on-the-job training" = "More than a year of training on the job.",
  "Apprenticeship" = "A paid apprenticeship: you earn while you learn, usually 1 to 6 years.",
  "Internship/residency" = "A supervised internship or residency after school.")

# ---- Helpers ----------------------------------------------------------------
fmt_d   <- function(x) ifelse(is.na(x), "\u2014", paste0("$", formatC(round(x), format = "d", big.mark = ",")))
fmt_n   <- function(x) ifelse(is.na(x), "\u2014", formatC(round(x), format = "d", big.mark = ","))
fmt_k   <- function(x) ifelse(is.na(x), "\u2014", ifelse(x >= TOP_A, paste0("$", round(TOP_A / 1000), "k+"), paste0("$", round(x / 1000), "k")))
fmt_pay <- function(x) ifelse(!is.na(x) & x >= TOP_A, paste0(fmt_d(TOP_A), "+"), fmt_d(x))
edu_short <- function(x) { i <- match(x, EDU_LEVELS); ifelse(is.na(i), "\u2014", EDU_SHORT[i]) }
explain <- function(map, x) { if (is.null(x) || is.na(x)) return("Not reported by BLS."); v <- unname(map[x]); if (is.na(v)) x else v }
short_name <- function(t) sub(",.*$", "", t)
growth_label <- function(x) if (is.na(x)) "growth n/a" else sprintf("%+.0f%% growth", x)
growth_chip <- function(x) {
  if (is.na(x)) return("\u2014")
  cls <- if (x >= 1.5 * AVG_GROWTH) "chip up-strong" else if (x > 0) "chip up" else "chip down"
  span(class = cls, sprintf("%+.1f%%", x))
}
score_bar <- function(v) {
  div(class = "score", div(class = "score-track", div(class = "score-fill", style = sprintf("width:%s%%", v))), span(class = "score-n", v))
}
js_click <- function(input_id, value) {
  sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", input_id, value)
}

state_area <- function(abbr) STATES$area[STATES$state_abbr == abbr][1]

area_choices <- function(abbr) {
  st <- STATES[STATES$state_abbr == abbr, ]
  hit <- grepl(paste0("(^|\\|)", abbr, "(\\||$)"), AREAS$states)
  metros <- AREAS[hit & AREAS$kind == "Metro area", ]
  metros <- metros[order(metros$area_title), ]
  rural  <- AREAS[hit & AREAS$kind == "Nonmetro area", ]
  rural  <- rural[order(rural$area_title), ]
  ch <- list()
  ch[["Statewide"]] <- as.list(setNames(st$area, paste("All of", st$area_title)))
  if (nrow(metros)) ch[["Cities & metro areas"]] <- as.list(setNames(metros$area, metros$area_title))
  if (nrow(rural))  ch[["Rural & nonmetro areas"]] <- as.list(setNames(rural$area, rural$area_title))
  ch[["Whole country"]] <- as.list(setNames(NAT_CODE, "United States"))
  ch
}

# Joins local pay/jobs with national projections and scores each career.
build_area <- function(code) {
  w <- WAGES_BY_AREA[[code]]
  if (is.null(w) || nrow(w) == 0) return(NULL)
  w |>
    inner_join(OCC, by = "soc") |>
    mutate(
      pay = a_median,
      local_openings = ifelse(is.na(openings_nat) | is.na(nat_emp) | nat_emp == 0 | is.na(emp),
                              NA_real_, openings_nat * emp / nat_emp),
      in_demand = !is.na(growth_pct) & (growth_pct >= 1.5 * AVG_GROWTH | coalesce(openings_nat, 0) >= 100000),
      score = round(100 * (0.5 * coalesce(percent_rank(growth_pct), 0) +
                           0.3 * coalesce(percent_rank(local_openings), 0) +
                           0.2 * coalesce(percent_rank(pmin(lq, 3)), 0))),
      edu_rank = match(education, EDU_LEVELS)
    )
}

# ---- Look -------------------------------------------------------------------
theme <- bs_theme(
  version = 5,
  bg = "#FBFAF7", fg = "#1C1B2E",
  primary = "#2E3192", secondary = "#6B6880",
  base_font = font_google("Inter"),
  heading_font = font_google("Nunito", wght = c(700, 800)),
  "border-radius" = "0.75rem"
)

CSS <- "
:root { --ink:#1C1B2E; --muted:#6B6880; --line:#E7E3DA; --card:#FFFFFF; --hot:#FF7A1A; --hot-bg:#FFF1E6; --indigo:#2E3192; }
.navbar .brand { font-family: Nunito, sans-serif; font-weight: 800; letter-spacing: -0.01em; }
.loc-bar { background: var(--card); border-bottom: 1px solid var(--line); padding: 0.9rem 0 0.2rem; }
.loc-bar .form-label { font-size: 0.8rem; font-weight: 700; color: var(--muted); text-transform: uppercase; letter-spacing: 0.05em; }
.loc-hint { font-size: 0.85rem; color: var(--muted); padding-top: 2rem; }
.hero { padding: 3rem 0 1.25rem; }
.hero .eyebrow { color: var(--muted); font-weight: 700; text-transform: uppercase; letter-spacing: 0.08em; font-size: 0.8rem; }
.hero h1 { font-weight: 800; font-size: clamp(2.1rem, 4.5vw, 3.6rem); line-height: 1.05; letter-spacing: -0.02em; margin: 0.5rem 0 1rem; max-width: 18ch; }
.hero .hl { color: var(--indigo); }
.hero .lede { font-size: 1.15rem; color: var(--muted); max-width: 60ch; }
.quick { display: flex; flex-wrap: wrap; gap: 0.6rem; margin: 0.5rem 0 2.5rem; }
.quick .btn { border-radius: 999px; font-weight: 600; padding: 0.5rem 1.1rem; }
.section-h { font-weight: 800; margin: 2.2rem 0 0.3rem; }
.section-sub { color: var(--muted); margin-bottom: 1rem; }
.pick-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(230px, 1fr)); gap: 0.9rem; }
.pick-card, .field-card { display: block; text-decoration: none; color: inherit; background: var(--card); border: 1px solid var(--line); border-radius: 14px; padding: 1rem 1.1rem; transition: transform .15s ease, box-shadow .15s ease, border-color .15s ease; }
.pick-card:hover, .field-card:hover { transform: translateY(-2px); box-shadow: 0 10px 28px rgba(28,27,46,.08); border-color: var(--indigo); color: inherit; }
.pick-title { font-weight: 700; font-size: 1.05rem; line-height: 1.25; margin: 0.25rem 0 0.6rem; }
.pick-pay { font-family: Nunito, sans-serif; font-weight: 800; font-size: 1.6rem; color: var(--indigo); }
.pick-meta { color: var(--muted); font-size: 0.85rem; }
.field-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(240px, 1fr)); gap: 0.9rem; margin-bottom: 4rem; }
.field-card.is-empty { opacity: 0.45; }
.field-top { display: flex; justify-content: space-between; align-items: center; }
.field-emoji { font-size: 1.9rem; }
.field-name { font-weight: 800; font-family: Nunito, sans-serif; font-size: 1.1rem; margin-top: 0.4rem; }
.field-blurb { color: var(--muted); font-size: 0.9rem; min-height: 2.6em; }
.field-stats { font-size: 0.85rem; font-weight: 600; margin-top: 0.5rem; }
.badge-demand { background: var(--hot-bg); color: #B4480A; border-radius: 999px; padding: 0.15rem 0.6rem; font-size: 0.75rem; font-weight: 700; white-space: nowrap; }
.badge-loc { background: #EEF0FB; color: var(--indigo); border-radius: 999px; padding: 0.15rem 0.6rem; font-size: 0.8rem; font-weight: 600; }
.car-title { font-weight: 700; }
.car-sub { color: var(--muted); font-size: 0.82rem; display: flex; gap: 0.5rem; align-items: center; flex-wrap: wrap; }
.num { font-variant-numeric: tabular-nums; font-weight: 700; }
.chip { border-radius: 999px; padding: 0.1rem 0.5rem; font-weight: 700; font-size: 0.85rem; font-variant-numeric: tabular-nums; }
.chip.up-strong { background: var(--hot-bg); color: #B4480A; }
.chip.up { background: #EAF6EE; color: #1E7A43; }
.chip.down { background: #F3F2F0; color: var(--muted); }
.score { display: flex; align-items: center; gap: 0.5rem; }
.score-track { flex: 1; height: 8px; background: #EFEDE7; border-radius: 99px; overflow: hidden; }
.score-fill { height: 100%; background: linear-gradient(90deg, var(--indigo), var(--hot)); border-radius: 99px; }
.score-n { font-size: 0.8rem; color: var(--muted); width: 2ch; text-align: right; }
.results-head { display: flex; justify-content: space-between; align-items: baseline; margin: 0.5rem 0 1rem; }
.results-head h3 { font-weight: 800; margin: 0; }
.career-modal h2 { font-weight: 800; letter-spacing: -0.01em; margin: 0.2rem 0 0.6rem; }
.career-modal h4 { font-weight: 800; margin: 1.8rem 0 0.8rem; }
.cm-badges { display: flex; gap: 0.5rem; flex-wrap: wrap; }
.ladder { display: grid; grid-template-columns: repeat(5, 1fr); gap: 0.5rem; }
.rung { background: #F5F4F0; border-radius: 12px; padding: 0.8rem; text-align: center; }
.rung.mid { background: var(--indigo); color: #fff; }
.rung-v { font-family: Nunito, sans-serif; font-weight: 800; font-size: 1.25rem; }
.rung-l { font-weight: 600; font-size: 0.85rem; }
.rung-p { font-size: 0.75rem; opacity: 0.7; }
.note { color: var(--muted); font-size: 0.85rem; margin-top: 0.6rem; }
.cmp { width: 100%; margin-top: 1rem; font-variant-numeric: tabular-nums; }
.cmp th { color: var(--muted); font-size: 0.75rem; text-transform: uppercase; letter-spacing: 0.05em; font-weight: 700; padding: 0.4rem; }
.cmp td { padding: 0.5rem 0.4rem; border-top: 1px solid var(--line); }
.facts { display: grid; grid-template-columns: repeat(3, 1fr); gap: 0.6rem; }
.fact { border: 1px solid var(--line); border-radius: 12px; padding: 0.8rem 1rem; }
.fact-v { font-family: Nunito, sans-serif; font-weight: 800; font-size: 1.5rem; }
.fact-l { color: var(--muted); font-size: 0.85rem; }
.steps { display: grid; grid-template-columns: repeat(3, 1fr); gap: 0.6rem; }
.step { border: 1px solid var(--line); border-radius: 12px; padding: 0.9rem 1rem; position: relative; }
.step-n { width: 1.8rem; height: 1.8rem; border-radius: 50%; background: var(--indigo); color: #fff; display: grid; place-items: center; font-weight: 800; margin-bottom: 0.5rem; }
.step-title { color: var(--muted); font-size: 0.8rem; text-transform: uppercase; letter-spacing: 0.05em; font-weight: 700; }
.step-val { font-weight: 700; margin: 0.15rem 0 0.35rem; }
.step-note { font-size: 0.88rem; color: var(--muted); }
.links { display: flex; flex-wrap: wrap; gap: 0.5rem; }
.links a { border: 1px solid var(--line); border-radius: 999px; padding: 0.4rem 0.9rem; text-decoration: none; font-weight: 600; font-size: 0.9rem; }
.links a:hover { border-color: var(--indigo); }
.rel { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 0.6rem; }
.about { max-width: 70ch; padding: 2.5rem 0 4rem; }
.about h2 { font-weight: 800; }
.about h4 { font-weight: 800; margin-top: 1.6rem; }
.checked { color: var(--muted); font-size: 0.8rem; }
@media (max-width: 768px) { .ladder, .facts, .steps { grid-template-columns: 1fr 1fr; } }
"

rt_theme <- reactableTheme(
  backgroundColor = "#FFFFFF",
  borderColor = "#EFEDE7",
  highlightColor = "#F4F3FB",
  cellPadding = "12px 14px",
  headerStyle = list(fontWeight = 700, color = "#6B6880", textTransform = "uppercase",
                     fontSize = "0.72rem", letterSpacing = "0.05em")
)

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  id = "main",
  title = tags$span(class = "brand", "\U0001F9ED Career Compass"),
  theme = theme,
  fillable = FALSE,
  header = tagList(
    tags$head(tags$style(HTML(CSS))),
    div(class = "loc-bar", div(class = "container-xl",
      layout_columns(
        col_widths = c(4, 5, 3),
        selectizeInput("state", "State", choices = STATE_CHOICES, selected = DEFAULT_STATE),
        selectizeInput("area", "City or area (type to search)",
                       choices = area_choices(DEFAULT_STATE), selected = state_area(DEFAULT_STATE)),
        div(class = "loc-hint", "BLS groups cities into metro areas. Pick the one your city belongs to.")
      )
    ))
  ),

  nav_panel("Explore", value = "explore", div(class = "container-xl",
    uiOutput("hero"),
    div(class = "quick",
      actionButton("qp_none", "No degree needed", class = "btn-outline-primary"),
      actionButton("qp_two",  "Certificate or 2-year degree", class = "btn-outline-primary"),
      actionButton("qp_four", "Bachelor's degree", class = "btn-outline-primary"),
      actionButton("qp_grad", "Grad or professional school", class = "btn-outline-primary")
    ),
    h3(class = "section-h", "Top picks here"),
    div(class = "section-sub", "In demand and paying above the typical local wage. Tap one for pay and the path in."),
    uiOutput("top_picks"),
    h3(class = "section-h", "Browse by field"),
    div(class = "section-sub", "Every field BLS tracks, with how many careers in each are hiring."),
    uiOutput("field_grid")
  )),

  nav_panel("Find careers", value = "careers", div(class = "container-xl", style = "padding-top:1.5rem; padding-bottom:3rem;",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput("field", "Field", choices = FIELD_CHOICES, selected = "all"),
        textInput("q", "Search careers", placeholder = "e.g. nurse, police, electrician"),
        input_switch("demand_only", "Only in-demand careers", value = FALSE),
        sliderInput("min_pay", "Minimum typical pay", min = 0, max = 200000, value = 0,
                    step = 5000, pre = "$", sep = ","),
        checkboxGroupInput("edu", "Education to start", choices = setNames(EDU_LEVELS, EDU_SHORT),
                           selected = EDU_LEVELS),
        radioButtons("sort", "Sort by", choices = c("Best outlook" = "score", "Highest pay" = "pay",
                                                    "Most jobs here" = "emp", "Fastest growth" = "growth"))
      ),
      uiOutput("results_head"),
      reactableOutput("table")
    )
  )),

  nav_panel("About the data", value = "about", div(class = "container-xl", div(class = "about",
    h2("Where the numbers come from"),
    p("Everything here comes from the U.S. Bureau of Labor Statistics, the federal agency that measures jobs and pay. Nothing is scraped from job boards or estimated by AI."),
    h4("Pay and jobs by place"),
    p(sprintf("Occupational Employment and Wage Statistics (OEWS), %s release. Covers about 800 careers in every state and every metro and rural area. Pay is for employees and doesn't include the self-employed.", META$oews_label)),
    h4("Growth, openings and the path in"),
    p(sprintf("BLS Employment Projections, %s. Growth and openings are national; local openings are estimated by scaling national openings by the share of the career's jobs located in your area. Education, experience and training are BLS's typical entry requirements.", META$proj_period)),
    h4("What \u201cin demand\u201d means"),
    p(sprintf("A career is flagged when BLS projects it to grow at least 1.5x faster than all jobs (all jobs: %+.1f%%) or to have 100,000+ openings a year nationally. The outlook score blends growth (50%%), estimated local openings (30%%) and how concentrated the job is here vs. nationally (20%%).", AVG_GROWTH)),
    h4("How fresh is it?"),
    p("A robot checks BLS every Monday morning. When BLS publishes new numbers (wages each spring, projections each fall), the site rebuilds itself automatically."),
    p(class = "checked", sprintf("Last checked: %s \u00b7 Data built: %s", LAST_CHECKED %||% "n/a", META$built_at %||% "n/a"))
  ))),

  nav_spacer(),
  nav_item(tags$span(class = "checked", sprintf("Pay: %s \u00b7 Checked %s", META$oews_label, LAST_CHECKED %||% "n/a")))
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  observeEvent(input$state, {
    updateSelectizeInput(session, "area", choices = area_choices(input$state),
                         selected = state_area(input$state))
  }, ignoreInit = TRUE)

  area_row <- reactive({ req(input$area); AREAS[match(input$area, AREAS$area), ] })
  area_df  <- reactive({ req(input$area); build_area(input$area) })

  # Explore ----
  output$hero <- renderUI({
    d <- area_df(); req(d); a <- area_row()
    div(class = "hero",
      div(class = "eyebrow", paste(a$kind, "\u00b7", META$oews_label, "pay data")),
      h1("Find a career that's hiring in ", span(class = "hl", short_name(a$area_title))),
      p(class = "lede", sprintf(
        "%s careers tracked here. %s of them are in demand, and the typical worker here earns %s a year.",
        fmt_n(nrow(d)), fmt_n(sum(d$in_demand, na.rm = TRUE)), fmt_pay(a$all_median)))
    )
  })

  output$top_picks <- renderUI({
    d <- area_df(); req(d); a <- area_row()
    picks <- d |> filter(in_demand, !is.na(pay), pay > coalesce(a$all_median, 0)) |>
      arrange(desc(score)) |> head(8)
    if (nrow(picks) == 0) picks <- d |> arrange(desc(score)) |> head(8)
    div(class = "pick-grid", lapply(seq_len(nrow(picks)), function(i) {
      pk <- picks[i, ]
      tags$a(class = "pick-card", href = "#", onclick = js_click("career_click", pk$soc),
        div(class = "car-sub", paste(FIELD_EMOJI[[pk$field]], FIELD_NAME[[pk$field]])),
        div(class = "pick-title", pk$title),
        div(class = "pick-pay", fmt_pay(pk$pay)),
        div(class = "pick-meta", paste(growth_label(pk$growth_pct), "\u00b7", edu_short(pk$education)))
      )
    }))
  })

  output$field_grid <- renderUI({
    d <- area_df(); req(d)
    s <- d |> group_by(field) |>
      summarise(n = n(), n_dem = sum(in_demand, na.rm = TRUE), med = median(pay, na.rm = TRUE), .groups = "drop")
    div(class = "field-grid", lapply(seq_len(nrow(FIELDS)), function(i) {
      f <- FIELDS[i, ]
      x <- s[s$field == f$code, ]
      n <- if (nrow(x)) x$n else 0
      tags$a(class = paste("field-card", if (n == 0) "is-empty"), href = "#",
        onclick = js_click("field_click", f$code),
        div(class = "field-top", span(class = "field-emoji", f$emoji),
            if (n > 0 && x$n_dem > 0) span(class = "badge-demand", paste(x$n_dem, "in demand"))),
        div(class = "field-name", f$name),
        div(class = "field-blurb", f$blurb),
        div(class = "field-stats", if (n > 0) paste0(n, " careers \u00b7 typical pay ", fmt_k(x$med)) else "No local data")
      )
    }))
  })

  observeEvent(input$field_click, {
    updateSelectInput(session, "field", selected = input$field_click)
    updateTextInput(session, "q", value = "")
    nav_select("main", "careers")
  })

  go_path <- function(levels) {
    updateSelectInput(session, "field", selected = "all")
    updateCheckboxGroupInput(session, "edu", selected = levels)
    update_switch("demand_only", value = TRUE)
    updateRadioButtons(session, "sort", selected = "score")
    nav_select("main", "careers")
  }
  observeEvent(input$qp_none, go_path(EDU_LEVELS[1:2]))
  observeEvent(input$qp_two,  go_path(EDU_LEVELS[3:5]))
  observeEvent(input$qp_four, go_path(EDU_LEVELS[6]))
  observeEvent(input$qp_grad, go_path(EDU_LEVELS[7:8]))

  # Find careers ----
  table_data <- reactive({
    d <- area_df(); req(d)
    if (!identical(input$field, "all")) d <- d[d$field == input$field, ]
    all_edu <- length(input$edu) == length(EDU_LEVELS)
    d <- d[d$education %in% input$edu | (is.na(d$education) & all_edu), ]
    if (input$min_pay > 0) d <- d[!is.na(d$pay) & d$pay >= input$min_pay, ]
    if (isTRUE(input$demand_only)) d <- d[d$in_demand, ]
    q <- trimws(input$q %||% "")
    if (nzchar(q)) d <- d[grepl(tolower(q), tolower(d$title), fixed = TRUE), ]
    switch(input$sort,
      pay    = arrange(d, desc(pay)),
      emp    = arrange(d, desc(emp)),
      growth = arrange(d, desc(growth_pct)),
      arrange(d, desc(score)))
  })

  output$results_head <- renderUI({
    d <- table_data(); a <- area_row()
    fld <- if (identical(input$field, "all")) "all fields" else FIELD_NAME[[input$field]]
    div(class = "results-head",
      h3(sprintf("%s careers \u00b7 %s", fmt_n(nrow(d)), fld)),
      span(class = "badge-loc", paste("\U0001F4CD", a$area_title)))
  })

  output$table <- renderReactable({
    d <- table_data()
    shiny::validate(need(nrow(d) > 0, "No careers match those filters here. Try loosening them."))
    show <- d |> transmute(title, field, in_demand, pay, hourly = h_median, a_p10, a_p90,
                           emp, growth_pct, local_openings, education, score)
    reactable(
      show,
      columns = list(
        title = colDef(name = "Career", minWidth = 260, cell = function(value, index) {
          r <- show[index, ]
          div(div(class = "car-title", value),
              div(class = "car-sub", paste(FIELD_EMOJI[[r$field]], FIELD_NAME[[r$field]]),
                  if (isTRUE(r$in_demand)) span(class = "badge-demand", "\U0001F525 In demand")))
        }),
        field = colDef(show = FALSE), in_demand = colDef(show = FALSE), hourly = colDef(show = FALSE),
        a_p10 = colDef(show = FALSE), a_p90 = colDef(show = FALSE),
        pay = colDef(name = "Typical pay", align = "right", minWidth = 130, cell = function(value, index) {
          r <- show[index, ]
          if (is.na(value) && !is.na(r$hourly)) return(div(class = "num", sprintf("$%.2f/hr", r$hourly)))
          div(div(class = "num", fmt_pay(value)),
              div(class = "car-sub", style = "justify-content:flex-end;", paste(fmt_k(r$a_p10), "to", fmt_k(r$a_p90))))
        }),
        emp = colDef(name = "Jobs here", align = "right", cell = function(value) fmt_n(value)),
        growth_pct = colDef(name = "10-yr growth (U.S.)", align = "right", cell = function(value) growth_chip(value)),
        local_openings = colDef(name = "Openings / yr (est.)", align = "right", cell = function(value) fmt_n(value)),
        education = colDef(name = "Education to start", minWidth = 150, cell = function(value) edu_short(value)),
        score = colDef(name = "Outlook", minWidth = 130, cell = function(value) score_bar(value))
      ),
      defaultPageSize = 20, showPageSizeOptions = TRUE, pageSizeOptions = c(20, 50, 100),
      highlight = TRUE, theme = rt_theme, rowStyle = list(cursor = "pointer"),
      onClick = JS("function(rowInfo) { Shiny.setInputValue('career_row', rowInfo.index + 1, {priority: 'event'}) }"),
      language = reactableLang(pageInfo = "{rowStart} to {rowEnd} of {rows} careers")
    )
  })

  # Career detail ----
  show_career <- function(s) {
    d <- area_df(); req(d)
    r <- d[d$soc == s, ]
    if (nrow(r) == 0) {
      showNotification("BLS doesn't publish data for that career in this area.", type = "warning")
      return(invisible(NULL))
    }
    r <- r[1, ]
    a <- area_row()
    st_code <- state_area(input$state)

    get_w <- function(code) {
      w <- WAGES_BY_AREA[[code]]
      if (is.null(w)) return(NULL)
      w <- w[w$soc == s, ]
      if (nrow(w)) w[1, ] else NULL
    }
    rows <- list(list(label = short_name(a$area_title), w = r))
    if (!identical(st_code, input$area)) {
      w <- get_w(st_code)
      if (!is.null(w)) rows[[length(rows) + 1]] <- list(label = STATES$area_title[STATES$area == st_code][1], w = w)
    }
    if (!identical(NAT_CODE, input$area)) {
      w <- get_w(NAT_CODE)
      if (!is.null(w)) rows[[length(rows) + 1]] <- list(label = "United States", w = w)
    }

    rung <- function(label, pct, v, mid = FALSE) {
      div(class = paste("rung", if (mid) "mid"),
          div(class = "rung-v", fmt_k(v)), div(class = "rung-l", label), div(class = "rung-p", pct))
    }
    ladder <- if (!is.na(r$a_median)) {
      div(class = "ladder",
        rung("Starting out", "10th percentile", r$a_p10),
        rung("Early career", "25th percentile", r$a_p25),
        rung("Typical", "Median", r$a_median, mid = TRUE),
        rung("Experienced", "75th percentile", r$a_p75),
        rung("Top earners", "90th percentile", r$a_p90))
    } else if (!is.na(r$h_median)) {
      p(tags$b(sprintf("Typical pay: $%.2f per hour.", r$h_median)), " BLS only reports hourly pay for this career.")
    } else {
      p("BLS doesn't publish pay for this career here.")
    }

    cmp <- tags$table(class = "cmp",
      tags$thead(tags$tr(tags$th(""), tags$th("Starting"), tags$th("Typical"), tags$th("Top"), tags$th("Jobs"))),
      tags$tbody(lapply(rows, function(x) tags$tr(
        tags$td(x$label), tags$td(fmt_k(x$w$a_p10)), tags$td(tags$b(fmt_k(x$w$a_median))),
        tags$td(fmt_k(x$w$a_p90)), tags$td(fmt_n(x$w$emp))))))

    growth_txt <- if (is.na(r$growth_pct)) "BLS has no national projection for this career." else
      sprintf("BLS projects %+.1f%% growth nationally over %s, vs %+.1f%% for all jobs.", r$growth_pct, META$proj_period, AVG_GROWTH)
    lq_txt <- if (is.na(r$lq)) "" else if (r$lq >= 1.2)
      sprintf("It's %.1fx more common here than nationwide, so local employers lean on it.", r$lq) else if (r$lq <= 0.8)
      sprintf("It's less common here than nationwide (%.1fx).", r$lq) else "It's about as common here as nationwide."

    city <- if (a$kind %in% c("Metro area", "Nonmetro area")) {
      paste0(sub("-.*$", "", short_name(a$area_title)), ", ", input$state)
    } else if (a$kind == "State") a$area_title else ""
    enc <- function(x) URLencode(x, reserved = TRUE)
    onet <- if (grepl("All Other", r$title)) paste0("https://www.onetonline.org/find/quick?s=", enc(r$title)) else
      paste0("https://www.onetonline.org/link/summary/", s, ".00")
    cos_profile <- sprintf("https://www.careeronestop.org/Toolkit/Careers/Occupations/occupation-profile.aspx?keyword=%s&onetcode=%s.00&location=%s",
                           enc(r$title), s, enc(input$state))
    cos_train <- sprintf("https://www.careeronestop.org/Toolkit/Training/find-local-training.aspx?keyword=%s&location=%s",
                         enc(r$title), enc(if (nzchar(city)) city else "United States"))

    rel <- d |>
      filter(field == r$field, soc != s, !is.na(pay), pay > coalesce(r$pay, 0),
             is.na(edu_rank) | edu_rank <= coalesce(r$edu_rank, 99L) + 1L) |>
      arrange(desc(score)) |> head(4)

    showModal(modalDialog(
      size = "xl", easyClose = TRUE, footer = modalButton("Close"),
      div(class = "career-modal",
        div(class = "car-sub", paste(FIELD_EMOJI[[r$field]], FIELD_NAME[[r$field]])),
        h2(r$title),
        div(class = "cm-badges",
            if (isTRUE(r$in_demand)) span(class = "badge-demand", "\U0001F525 In demand"),
            span(class = "badge-loc", paste("\U0001F4CD", a$area_title))),

        h4("What it pays"),
        ladder,
        p(class = "note", sprintf("Yearly pay, %s. The 10th percentile means 10%% of workers here earn less.", META$oews_label)),
        if (length(rows) > 1) cmp,

        h4("Is it hiring?"),
        div(class = "facts",
          div(class = "fact", div(class = "fact-v", fmt_n(r$emp)),
              div(class = "fact-l", sprintf("people work this job here (%s per 1,000 jobs)", ifelse(is.na(r$jobs_1000), "n/a", r$jobs_1000)))),
          div(class = "fact", div(class = "fact-v", fmt_n(r$local_openings)),
              div(class = "fact-l", sprintf("estimated openings a year here (%s nationally)", fmt_n(r$openings_nat)))),
          div(class = "fact", div(class = "fact-v", growth_chip(r$growth_pct)),
              div(class = "fact-l", paste("10-year growth,", META$proj_period)))),
        p(class = "note", growth_txt, " ", lq_txt),

        h4("How to get there"),
        div(class = "steps",
          div(class = "step", div(class = "step-n", "1"), div(class = "step-title", "Education"),
              div(class = "step-val", r$education %||% "Not reported"),
              div(class = "step-note", if (is.na(r$edu_rank)) "Not reported by BLS." else EDU_EXPLAIN[r$edu_rank])),
          div(class = "step", div(class = "step-n", "2"), div(class = "step-title", "Experience"),
              div(class = "step-val", r$experience %||% "Not reported"),
              div(class = "step-note", explain(EXP_EXPLAIN, r$experience))),
          div(class = "step", div(class = "step-n", "3"), div(class = "step-title", "On-the-job training"),
              div(class = "step-val", r$training %||% "Not reported"),
              div(class = "step-note", explain(TRAIN_EXPLAIN, r$training)))),

        h4("Next steps"),
        div(class = "links",
          tags$a(href = onet, target = "_blank", "Skills & daily tasks (O*NET) \u2197"),
          tags$a(href = cos_profile, target = "_blank", "Career profile (CareerOneStop) \u2197"),
          tags$a(href = cos_train, target = "_blank", "Find training near you \u2197"),
          if (grepl("Apprentice", r$training %||% "")) tags$a(href = "https://www.apprenticeship.gov/apprenticeship-job-finder", target = "_blank", "Find an apprenticeship \u2197")),

        if (nrow(rel)) tagList(
          h4("Level up in this field"),
          div(class = "rel", lapply(seq_len(nrow(rel)), function(i) {
            x <- rel[i, ]
            tags$a(class = "pick-card", href = "#", onclick = js_click("career_click", x$soc),
              div(class = "pick-title", x$title),
              div(class = "pick-pay", fmt_pay(x$pay)),
              div(class = "pick-meta", edu_short(x$education)))
          })))
      )
    ))
  }

  observeEvent(input$career_click, show_career(input$career_click))
  observeEvent(input$career_row, {
    d <- table_data(); i <- input$career_row
    if (i >= 1 && i <= nrow(d)) show_career(d$soc[i])
  })
}

shinyApp(ui, server)
