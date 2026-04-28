# Landscape Through Time — Going Dynamic

A practical guide for replacing local files with a real database, so your app is publishable and works for multiple users.

------------------------------------------------------------------------

## The Problem Right Now

Your app stores everything locally:

-   **Paintings data** → hardcoded CSV path (`/Users/jawvt/Documents/...BPaintings.csv`)
-   **User submissions** → local `.rds` files in `app_data/`
-   **Approved photos** → local `.rds` files in `app_data/`

This means the app only works on your computer, submissions disappear when the server restarts (on most hosting platforms), and the admin can't add new paintings without editing the CSV and redeploying.

------------------------------------------------------------------------

## The Solution: One Database for Everything

You need a single cloud database that stores both the paintings catalog AND user submissions. Here's the simplest path that will actually work for a college project.

### Recommended Stack

| Component | Choice | Why |
|----|----|----|
| **Database** | Supabase (free tier) | PostgreSQL database with a nice web dashboard, free for small projects, no credit card required |
| **R package** | `DBI` + `RPostgres` | Standard R packages for talking to PostgreSQL |
| **Image storage** | Supabase Storage (or Cloudinary free tier) | User-uploaded photos need to live somewhere that serves URLs |
| **Hosting** | shinyapps.io (free tier) | Easiest way to deploy R Shiny apps |

> **Why not Firebase, MongoDB, or SQLite?** Supabase gives you a real PostgreSQL database with a visual table editor (so the admin can add paintings through a web UI), built-in file storage, and it's free. SQLite won't work on shinyapps.io because the filesystem is read-only. Firebase/MongoDB require more setup and different R packages.

------------------------------------------------------------------------

## Step-by-Step Plan

### Step 1: Set Up Supabase (30 minutes)

1.  Go to [supabase.com](https://supabase.com) and create a free account
2.  Create a new project (pick a region close to your users)
3.  Once the project is ready, go to **Settings → Database** and copy your:
    -   **Host** (e.g., `db.xyzabc.supabase.co`)
    -   **Database password** (you set this when creating the project)
    -   **Port** (usually `5432`)

### Step 2: Create Your Tables

In the Supabase dashboard, go to the **SQL Editor** and run this:

``` sql
-- Paintings catalog (replaces BPaintings.csv)
CREATE TABLE paintings (
  id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  year INTEGER,
  latitude DOUBLE PRECISION,
  longitude DOUBLE PRECISION,
  context TEXT,
  image_url TEXT,
  artist TEXT
);

-- User submissions (replaces submissions.rds)
CREATE TABLE submissions (
  submission_id TEXT PRIMARY KEY,
  name TEXT,
  email TEXT,
  painting_id INTEGER REFERENCES paintings(id),
  photo_url TEXT,
  latitude DOUBLE PRECISION,
  longitude DOUBLE PRECISION,
  observations TEXT,
  submission_date DATE DEFAULT CURRENT_DATE,
  approval_status TEXT DEFAULT 'Pending'
);
```

Then import your existing CSV data. In the Supabase dashboard, go to **Table Editor → paintings → Insert → Import from CSV** and upload `BPaintings.csv`. It will match columns automatically.

### Step 3: Install R Packages

Add these to the top of your app:

``` r
library(DBI)
library(RPostgres)
```

Install them once with:

``` r
install.packages(c("DBI", "RPostgres"))
```

### Step 4: Replace Local File Code with Database Code

Here's what changes in your app. The structure stays the same — you're just swapping *where* data comes from.

#### Connection Setup (replaces your LocalDataDirectory section)

``` r
# ---- Database Connection ----
# Store these as environment variables, NOT in your code
# (see Step 6 for how to do this securely)
get_db_connection <- function() {
  dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("SUPABASE_HOST"),
    port     = 5432,
    dbname   = "postgres",
    user     = "postgres",
    password = Sys.getenv("SUPABASE_PASSWORD")
  )
}
```

#### Loading Paintings (replaces `read.csv(paintings_csv)`)

``` r
# Load paintings from database instead of CSV
load_paintings <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))  # always close the connection
  dbGetQuery(con, "SELECT * FROM paintings ORDER BY id")
}

paintings_data <- load_paintings()
```

#### Loading/Saving Submissions (replaces `load_data` and `save_data`)

``` r
load_submissions <- function(status = NULL) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  if (is.null(status)) {
    dbGetQuery(con, "SELECT * FROM submissions ORDER BY submission_date DESC")
  } else {
    dbGetQuery(con, "SELECT * FROM submissions WHERE approval_status = $1
               ORDER BY submission_date DESC", params = list(status))
  }
}

save_submission <- function(sub) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  dbExecute(con,
    "INSERT INTO submissions
     (submission_id, name, email, painting_id, photo_url,
      latitude, longitude, observations, approval_status)
     VALUES ($1,$2,$3,$4,$5,$6,$7,$8,'Pending')",
    params = list(
      sub$submission_id, sub$name, sub$email,
      sub$painting_id, sub$photo_url,
      sub$latitude, sub$longitude, sub$observations
    )
  )
}

update_submission_status <- function(submission_id, new_status) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  dbExecute(con,
    "UPDATE submissions SET approval_status = $1 WHERE submission_id = $2",
    params = list(new_status, submission_id)
  )
}

delete_submission <- function(submission_id) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  dbExecute(con,
    "DELETE FROM submissions WHERE submission_id = $1",
    params = list(submission_id)
  )
}
```

#### Server Changes

In your server function, the reactive values simplify:

``` r
rv <- reactiveValues(
  admin_auth = FALSE,
  submission_success = FALSE,
  submission_error = NULL,
  submissions = load_submissions(),
  approved = load_submissions(status = "Approved"),
  # ... rest stays the same
)
```

And your approve/reject handlers become:

``` r
observeEvent(input$approve_submission, {
  if (length(input$admin_table_rows_selected) > 0) {
    idx <- input$admin_table_rows_selected
    sid <- rv$submissions[idx, "submission_id"]
    update_submission_status(sid, "Approved")
    # Refresh from database
    rv$submissions <- load_submissions()
    rv$approved <- load_submissions(status = "Approved")
    showNotification("Approved!", type = "message")
  }
})
```

### Step 5: Handle Photo Uploads (Cloud Storage)

Right now you encode photos as base64 strings. This works but makes your database huge. Better approach:

**Option A: Supabase Storage (simplest)**

1.  In Supabase dashboard, go to **Storage** and create a bucket called `submissions`
2.  Set the bucket to **public** (so images can be displayed via URL)
3.  Use the Supabase REST API from R to upload files:

``` r
upload_photo <- function(file_path, file_name) {
  # Upload via Supabase Storage REST API
  url <- paste0(
    "https://", Sys.getenv("SUPABASE_PROJECT_ID"),
    ".supabase.co/storage/v1/object/submissions/", file_name
  )
  response <- httr::PUT(
    url,
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("SUPABASE_SERVICE_KEY")),
      `Content-Type` = "image/jpeg"
    ),
    body = httr::upload_file(file_path)
  )
  # Return the public URL
  paste0(
    "https://", Sys.getenv("SUPABASE_PROJECT_ID"),
    ".supabase.co/storage/v1/object/public/submissions/", file_name
  )
}
```

**Option B: Keep base64 for now** — It works fine for a small number of submissions. You can always migrate later.

### Step 6: Secure Your Credentials

**Never put passwords directly in your R code.** Use environment variables instead.

Create a file called `.Renviron` in your project folder:

```         
SUPABASE_HOST=db.xyzabc.supabase.co
SUPABASE_PASSWORD=your-database-password
SUPABASE_PROJECT_ID=xyzabc
SUPABASE_SERVICE_KEY=your-service-role-key
```

Add `.Renviron` to your `.gitignore` so it never gets pushed to GitHub.

When deploying to shinyapps.io, you set these same variables in the shinyapps.io dashboard under your app's settings.

### Step 7: Admin Can Now Add Paintings Without You

This is the big win. Once paintings live in Supabase:

-   The admin goes to the Supabase dashboard → Table Editor → paintings
-   Clicks "Insert row"
-   Fills in title, year, coordinates, image URL, etc.
-   Clicks Save

Next time anyone loads the app, the new painting appears automatically. No CSV editing, no redeployment.

> **Future upgrade:** You could build an "Add Painting" form directly into your Admin tab so the admin never needs to touch Supabase. But the dashboard works great as a starting point.

------------------------------------------------------------------------

## Deployment to shinyapps.io

1.  Install: `install.packages("rsconnect")`
2.  Connect your account: go to shinyapps.io → Account → Tokens → copy the token command
3.  Deploy:

``` r
rsconnect::deployApp(
  appDir = "path/to/your/app",
  appName = "landscape-through-time"
)
```

4.  Set your environment variables in the shinyapps.io dashboard (Applications → Your App → Settings → Environment Variables)

------------------------------------------------------------------------

## What Changes vs. What Stays the Same

| Part of your app | Changes? | What to do |
|----|----|----|
| All your CSS | **No change** | Stays exactly as-is |
| UI layout (page_navbar, tabs, etc.) | **No change** | Stays exactly as-is |
| JavaScript (lightbox, tilt, etc.) | **No change** | Stays exactly as-is |
| `paintings_csv` and `read.csv()` | **Replace** | Use `load_paintings()` function above |
| `load_data()` / `save_data()` with .rds | **Replace** | Use database functions above |
| `SUBMISSIONS_FILE` / `APPROVED_FILE` | **Remove** | No more local files |
| `LocalDataDirectory` setup | **Remove** | No more local folders |
| Submission handler (form validation) | **Small tweak** | Same logic, just call `save_submission()` instead of `rbind` + `saveRDS` |
| Admin approve/reject/delete | **Small tweak** | Call `update_submission_status()` / `delete_submission()` then refresh |
| Map, gallery, comparisons rendering | **No change** | They already read from `paintings_data` and `rv$approved` — those just come from the database now |

------------------------------------------------------------------------

## Summary of Services You'll Need (All Free Tier)

1.  **Supabase** — database + image storage (free: 500MB database, 1GB storage)
2.  **shinyapps.io** — hosting your app (free: 25 active hours/month, 5 apps)
3.  **GitHub** — version control for your code (free, and you should be using it already)

------------------------------------------------------------------------

## Suggested Order of Work

1.  Set up Supabase and create tables (Day 1)
2.  Import your CSV into the paintings table (Day 1)
3.  Replace `read.csv` with `load_paintings()` and test locally (Day 1)
4.  Replace submission save/load with database functions and test (Day 2)
5.  Replace approve/reject/delete with database functions and test (Day 2)
6.  Deploy to shinyapps.io (Day 3)
7.  (Optional) Add photo cloud storage (Day 4)
8.  (Optional) Build "Add Painting" admin form (Day 4+)

Each step works independently — you can test after each one. Don't try to do everything at once.
