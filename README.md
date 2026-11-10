# Career Compass

A Shiny website that helps students and young adults find in-demand careers in
their state, city, or rural area: typical pay, pay range, local jobs, 10-year
growth, yearly openings, and the education / experience / training path in.

All data is public, from the U.S. Bureau of Labor Statistics (BLS):

| What | BLS program | How often BLS updates it |
|---|---|---|
| Pay + jobs for every state, metro and rural area | Occupational Employment & Wage Statistics (OEWS) | Once a year, spring |
| Growth, openings, education & training | Employment Projections, table 1.2 | Once a year, fall |

A GitHub Action checks BLS **every Monday at 6 AM Eastern**. If either file has
changed, it rebuilds the data, commits it, and the live site redeploys itself.

```
career-compass/
├── app.R                          # the website (Shiny + bslib + reactable)
├── scripts/update_data.R          # downloads + cleans BLS data -> data/careers.rds
├── data/                          # built data lives here (created by the script)
├── .github/workflows/update-data.yml   # the weekly robot
└── career-compass.Rproj
```

---

## Part 1: Run it on your own computer (15 min)

1. Install R packages (once):
   ```r
   install.packages(c("shiny","bslib","dplyr","reactable","htmltools","jsonlite",
                      "httr2","readxl","stringr","rsconnect","usethis","gitcreds"))
   ```
2. Open `career-compass.Rproj` in RStudio.
3. Build the data (downloads ~75 MB from BLS, takes a few minutes):
   ```r
   Sys.setenv(BLS_CONTACT = "you@example.com")   # BLS blocks anonymous scripts
   source("scripts/update_data.R")
   ```
4. Run the site:
   ```r
   shiny::runApp()
   ```

## Part 2: Put the code on GitHub (10 min)

1. Make a free account at github.com.
2. In RStudio, connect R to GitHub (once):
   ```r
   usethis::create_github_token()   # opens GitHub; click "Generate token", copy it
   gitcreds::gitcreds_set()         # paste the token
   ```
3. Turn the project into a GitHub repo:
   ```r
   usethis::use_git()      # say yes to commit + restart
   usethis::use_github()   # creates the repo on GitHub and pushes everything
   ```
4. On GitHub, open your repo, then **Settings > Secrets and variables > Actions >
   New repository secret**. Name: `BLS_CONTACT`, value: your email.
5. **Settings > Actions > General > Workflow permissions**: choose
   **Read and write permissions** and save.
6. **Actions** tab > **Weekly BLS data check** > **Run workflow** (tick "force")
   to test it. A green check means it worked and committed fresh data plus a
   `manifest.json`.

## Part 3: Publish the website (10 min, free)

GitHub Pages only serves static files, and Shiny needs R running on a server,
so the site itself is hosted on **Posit Connect Cloud** (free tier, made by the
RStudio people) and reads straight from your GitHub repo.

1. Go to connect.posit.cloud and sign in with GitHub.
2. Click **Publish > Shiny**, pick your `career-compass` repo, branch `main`,
   primary file `app.R`.
3. Turn on **Automatically publish on push**. That's the magic: every time the
   Monday robot commits new BLS data, the site redeploys by itself.
4. Click **Publish**. You'll get a public link to share.

> Alternative host: shinyapps.io (also free tier). Deploy with
> `rsconnect::deployApp()`; you'd then add a deploy step to the workflow.

---

## How it decides "in demand"

- **In demand badge**: BLS projects growth at least 1.5x faster than all jobs,
  or 100,000+ openings a year nationally (similar to BLS's own "bright outlook" idea).
- **Outlook score (0 to 100)**, ranked within the chosen area: 50% national
  growth, 30% estimated local openings, 20% local concentration (location quotient).
- **Local openings** = national openings x (local jobs / national jobs). It's an
  estimate, labeled as such in the app.

## Known limits (honest list)

- BLS reports pay by **metro area**, not individual city. Pick the metro your city belongs to.
- Growth projections are **national**. State-level projections exist at
  projectionscentral.org and would be a great v2 upgrade.
- Pay excludes self-employed workers. Very high wages are top-coded by BLS
  (shown as "$239,200+").
- If BLS ever renames a column, `update_data.R` stops with a clear error and
  GitHub emails you, so the live site never shows broken data.
- GitHub pauses scheduled workflows after 60 days with no repo activity. The
  robot commits a "last checked" stamp every week to stay awake.
