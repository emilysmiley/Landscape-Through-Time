############################################
#
# Landscape Through Time
# Alex, Ben, Emily
# Code started 2/18/26
#
# Interactive application where users are able
# to view historical art located in desired regions,
# and have the ability to find the physical locations
# of the paintings using an interactive map-based design.
#
############################################


# ===========================================================================
# LIBRARIES
# ===========================================================================

library(DBI)
library(RPostgres)
library(shiny)
library(bslib)
library(leaflet)
library(htmltools)
library(DT)
library(shinyjs)
library(maps)
library(httr)

# ===========================================================================
# DATABASE CONNECTION
# ===========================================================================

readRenviron(".Renviron")

# --- Supabase Storage ----------------------------------------------------
SUPABASE_URL      <- Sys.getenv("SUPABASE_URL")
SUPABASE_ANON_KEY <- Sys.getenv("SUPABASE_ANON_KEY")
STORAGE_BUCKET    <- "submissions"

upload_to_storage <- function(file_path, submission_id, ext = "jpg") {
  if (SUPABASE_URL == "" || SUPABASE_ANON_KEY == "") {
    stop("SUPABASE_URL and SUPABASE_ANON_KEY must be set in .Renviron for image uploads.")
  }
  
  filename <- paste0(submission_id, ".", ext)
  content_type <- if (ext == "png") "image/png" else "image/jpeg"
  upload_url <- paste0(SUPABASE_URL, "/storage/v1/object/", STORAGE_BUCKET, "/", filename)
  
  raw_bytes <- readBin(file_path, "raw", file.info(file_path)$size)
  
  res <- POST(
    upload_url,
    add_headers(
      `Authorization` = paste("Bearer", SUPABASE_ANON_KEY),
      `Content-Type`  = content_type,
      `x-upsert`      = "true"
    ),
    body = raw_bytes
  )
  
  if (status_code(res) >= 400) {
    stop("Storage upload failed (", status_code(res), "): ",
         content(res, "text", encoding = "UTF-8"))
  }
  
  paste0(SUPABASE_URL, "/storage/v1/object/public/", STORAGE_BUCKET, "/", filename)
}

get_db_con <- function() {
  dbConnect(
    Postgres(),
    host       = Sys.getenv("SUPABASE_HOST"),
    port       = as.integer(Sys.getenv("SUPABASE_PORT", "5432")),
    dbname     = Sys.getenv("SUPABASE_DB"),
    user       = Sys.getenv("SUPABASE_USER"),
    password   = Sys.getenv("SUPABASE_PASSWORD"),
    gssencmode = "disable"
  )
}

db_load_submissions <- function() {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  result <- dbGetQuery(con, "SELECT * FROM submissions ORDER BY submission_date DESC")
  if (nrow(result) == 0) {
    return(data.frame(
      submission_id = character(), name = character(), email = character(),
      painting_id = integer(), photo_url = character(),
      latitude = numeric(), longitude = numeric(),
      observations = character(), submission_date = character(),
      approval_status = character(),
      submission_type = character(),
      painting_title = character(),
      artist_name = character(),
      painting_year = character(),
      painting_context = character(),
      state = character(),
      region = character(),
      location_notes = character(),
      museum_name = character(),
      museum_latitude = numeric(),
      museum_longitude = numeric(),
      museum_image_url = character(),
      stringsAsFactors = FALSE
    ))
  }
  result
}

db_insert_submission <- function(sub_df) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
            "INSERT INTO submissions (submission_id, name, email, painting_id, photo_url,
     latitude, longitude, observations, submission_date, approval_status,
     submission_type, painting_title, artist_name, painting_year, painting_context,
     state, region, location_notes,
     museum_name, museum_latitude, museum_longitude, museum_image_url)
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22)",
            params = list(
              sub_df$submission_id,
              sub_df$name,
              sub_df$email,
              sub_df$painting_id,
              sub_df$photo_url,
              sub_df$latitude,
              sub_df$longitude,
              sub_df$observations,
              sub_df$submission_date,
              sub_df$approval_status,
              sub_df$submission_type,
              sub_df$painting_title,
              sub_df$artist_name,
              sub_df$painting_year,
              sub_df$painting_context,
              sub_df$state,
              sub_df$region,
              sub_df$location_notes,
              sub_df$museum_name,
              sub_df$museum_latitude,
              sub_df$museum_longitude,
              sub_df$museum_image_url
            )
  )
}

db_update_status <- function(submission_id, new_status) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
            "UPDATE submissions SET approval_status = $1 WHERE submission_id = $2",
            params = list(new_status, submission_id)
  )
}

db_delete_submission <- function(submission_id) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
            "DELETE FROM submissions WHERE submission_id = $1",
            params = list(submission_id)
  )
}

db_promote_to_painting <- function(sub_row) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  
  has_museum <- !is.na(sub_row$museum_name) && sub_row$museum_name != ""
  museum_name_val <- if (has_museum) as.character(sub_row$museum_name) else NA_character_
  museum_lat_val <- if (has_museum && !is.na(sub_row$museum_latitude)) as.numeric(sub_row$museum_latitude) else NA_real_
  museum_lng_val <- if (has_museum && !is.na(sub_row$museum_longitude)) as.numeric(sub_row$museum_longitude) else NA_real_
  museum_img_val <- if (has_museum && !is.na(sub_row$museum_image_url) && sub_row$museum_image_url != "") as.character(sub_row$museum_image_url) else NA_character_
  
  result <- dbGetQuery(con,
                       "INSERT INTO paintings (title, artist, \"year\", context, image_url, state, region, location_notes,
                                    museum_name, museum_latitude, museum_longitude, museum_image_url)
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
     RETURNING id",
                       params = list(
                         as.character(sub_row$painting_title),
                         as.character(sub_row$artist_name),
                         as.character(ifelse(is.na(sub_row$painting_year) || sub_row$painting_year == "", "", sub_row$painting_year)),
                         as.character(ifelse(is.na(sub_row$painting_context) || sub_row$painting_context == "", "", sub_row$painting_context)),
                         as.character(sub_row$photo_url),
                         as.character(ifelse(is.na(sub_row$state) || sub_row$state == "", NA_character_, sub_row$state)),
                         as.character(ifelse(is.na(sub_row$region) || sub_row$region == "", NA_character_, sub_row$region)),
                         as.character(ifelse(is.na(sub_row$location_notes) || sub_row$location_notes == "", NA_character_, sub_row$location_notes)),
                         museum_name_val,
                         museum_lat_val,
                         museum_lng_val,
                         museum_img_val
                       )
  )
  new_id <- result$id[1]
  dbExecute(con,
            "UPDATE submissions SET painting_id = $1 WHERE submission_id = $2",
            params = list(new_id, as.character(sub_row$submission_id))
  )
  new_id
}

db_update_painting_museum <- function(painting_id, museum_name, museum_lat, museum_lng, museum_img) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
            "UPDATE paintings SET museum_name = $1, museum_latitude = $2, museum_longitude = $3, museum_image_url = $4
             WHERE id = $5",
            params = list(
              as.character(museum_name),
              if (is.na(museum_lat)) NA_real_ else as.numeric(museum_lat),
              if (is.na(museum_lng)) NA_real_ else as.numeric(museum_lng),
              if (is.na(museum_img) || museum_img == "") NA_character_ else as.character(museum_img),
              as.integer(painting_id)
            )
  )
}

db_load_paintings <- function() {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "SELECT * FROM paintings ORDER BY id")
}


# ===========================================================================
# PAINTING DATA
# ===========================================================================

con <- get_db_con()
paintings_data <- dbGetQuery(con, "SELECT * FROM paintings ORDER BY id")
dbDisconnect(con)

# State center coordinates for map zoom
state_centers <- data.frame(
  state = state.name,
  lat = state.center$y,
  lng = state.center$x,
  stringsAsFactors = FALSE
)
# R's state.center places Alaska/Hawaii at inset-map positions, not real coords
state_centers[state_centers$state == "Alaska", c("lat", "lng")] <- c(64.2008, -152.4937)
state_centers[state_centers$state == "Hawaii", c("lat", "lng")] <- c(20.7984, -156.3319)


# ===========================================================================
# CUSTOM CSS
# ===========================================================================

app_css <- "
/* ==============================================
   GLASSMORPHISM THEME — Landscape Through Time

   TABLE OF CONTENTS:
   - CSS Variables
   - Reset & Base
   - Navbar Overrides
   - Tab Content Wrapper
   - Hero / Home Tab
   - Button Styles
   - Section Headers
   - Painting Cards
   - Map (Split Layout)
   - Form
   - Comparisons
   - Lightbox (Paintings)
   - Lightbox (Comparisons)
   - Contribute Landing Cards
   - Admin
   - Alerts
   - Responsive
   - Splash Screen
   - Search Bar
   - Shiny Specific Overrides
   - Light/Dark Mode Toggle
   - Light Mode Overrides
   - Mobile Tab Bar
   - Map Scroll Hint
   - Gallery Badges
   - Map Filter Dropdowns
   ============================================== */

/* --- CSS Variables ---------------------------------------- */
:root {
  --glass-bg: rgba(255, 255, 255, 0.12);
  --glass-bg-strong: rgba(255, 255, 255, 0.18);
  --glass-bg-light: rgba(255, 255, 255, 0.06);
  --glass-border: rgba(255, 255, 255, 0.25);
  --glass-border-subtle: rgba(255, 255, 255, 0.12);
  --glass-blur: 20px;
  --glass-blur-heavy: 40px;

  --terra: #E8976B;
  --terra-dark: #C4724E;
  --terra-light: #F2B896;
  --terra-glow: rgba(232, 151, 107, 0.4);
  --sage: #7FA88A;
  --sage-dark: #5F8868;
  --sage-light: #A0C4A8;
  --sage-glow: rgba(127, 168, 138, 0.35);
  --amber: #E2B94C;
  --amber-light: #EDD07A;
  --amber-glow: rgba(226, 185, 76, 0.3);

  --surface-dark: #0f1a14;
  --surface-dark-mid: #152420;
  --surface-card: rgba(255, 255, 255, 0.08);

  --text-primary: #FFFFFF;
  --text-secondary: rgba(255, 255, 255, 0.7);
  --text-muted: rgba(255, 255, 255, 0.45);

  --radius-sm: 12px;
  --radius-md: 20px;
  --radius-lg: 28px;
  --radius-xl: 36px;

  --shadow-glass: 0 8px 32px rgba(0, 0, 0, 0.25);
  --shadow-glass-lg: 0 16px 48px rgba(0, 0, 0, 0.3);
  --shadow-glow: 0 0 40px rgba(232, 151, 107, 0.15);
  --ease: cubic-bezier(0.4, 0, 0.2, 1);
}

/* --- Reset & Base ---------------------------------------- */
* { margin: 0; padding: 0; box-sizing: border-box; }
html { scroll-behavior: smooth; }

body {
  font-family: 'DM Sans', -apple-system, BlinkMacSystemFont, sans-serif;
  background: var(--surface-dark);
  color: var(--text-primary);
  line-height: 1.6;
  overflow-x: hidden;
}

/* --- Navbar Overrides ------------------------------------ */
.navbar {
  background: rgba(15, 26, 20, 0.6) !important;
  backdrop-filter: blur(var(--glass-blur-heavy));
  -webkit-backdrop-filter: blur(var(--glass-blur-heavy));
  border-bottom: 1px solid var(--glass-border-subtle) !important;
  padding: 0 !important;
  min-height: 64px;
  box-shadow: 0 4px 30px rgba(0, 0, 0, 0.3);
  position: sticky;
  top: 0;
  z-index: 999;
}

.navbar-nav .nav-link {
  font-family: 'DM Sans', sans-serif !important;
  font-weight: 600 !important;
  font-size: 13px !important;
  color: var(--text-secondary) !important;
  padding: 22px 24px !important;
  letter-spacing: 0.8px;
  text-transform: uppercase;
  transition: all 0.3s var(--ease);
  border-bottom: 2px solid transparent;
  margin-bottom: -1px;
  position: relative;
}

.navbar-nav .nav-link:hover {
  color: var(--text-primary) !important;
  background: rgba(255, 255, 255, 0.05);
}

.navbar-nav .nav-link.active,
.navbar-nav .nav-item.active .nav-link,
.navbar-nav .nav-link[aria-selected='true'] {
  color: var(--terra-light) !important;
  border-bottom-color: var(--terra) !important;
  background: rgba(232, 151, 107, 0.08);
}

.navbar-toggler { display: none !important; }
.navbar-toggler-icon { display: none !important; }

/* --- Tab Content Wrapper --------------------------------- */
.tab-content { background: var(--surface-dark); }

.tab-pane {
  animation: tabFadeIn 0.4s var(--ease);
  padding: 0 !important;
  margin: 0 !important;
}

@keyframes tabFadeIn {
  from { opacity: 0; transform: translateY(12px); }
  to { opacity: 1; transform: translateY(0); }
}

/* --- Hero / Home Tab ------------------------------------- */
.hero-banner {
  position: relative;
  background:
    linear-gradient(180deg, rgba(15,26,20,0.3) 0%, rgba(15,26,20,0.7) 100%),
    url('https://upload.wikimedia.org/wikipedia/commons/thumb/4/4a/Albert_Bierstadt_-_Among_the_Sierra_Nevada%2C_California_-_Google_Art_Project.jpg/2560px-Albert_Bierstadt_-_Among_the_Sierra_Nevada%2C_California_-_Google_Art_Project.jpg');
  background-size: cover;
  background-position: center;
  min-height: calc(100vh - 64px);
  display: flex;
  align-items: center;
  justify-content: center;
  overflow: hidden;
}

.hero-bg-pattern { position: absolute; inset: 0; }

.hero-glow {
  position: absolute;
  border-radius: 50%;
  filter: blur(100px);
  pointer-events: none;
}

.hero-glow-1 {
  background: var(--terra-glow);
  width: 600px; height: 600px;
  top: -100px; right: -50px;
  opacity: 0.5;
}

.hero-glow-2 {
  background: var(--sage-glow);
  width: 500px; height: 500px;
  bottom: -100px; left: -50px;
  opacity: 0.4;
}

.hero-glow-3 {
  background: var(--amber-glow);
  width: 300px; height: 300px;
  top: 50%; left: 50%;
  transform: translate(-50%, -50%);
  opacity: 0.3;
}

.hero-inner {
  position: relative;
  z-index: 2;
  text-align: center;
  max-width: 820px;
  padding: 60px 48px;
  background: var(--glass-bg);
  backdrop-filter: blur(var(--glass-blur-heavy));
  -webkit-backdrop-filter: blur(var(--glass-blur-heavy));
  border: 1px solid var(--glass-border);
  border-radius: var(--radius-xl);
  box-shadow: var(--shadow-glass), var(--shadow-glow);
}

.hero-badge {
  display: inline-block;
  background: rgba(232, 151, 107, 0.15);
  border: 1px solid rgba(232, 151, 107, 0.35);
  color: var(--terra-light);
  font-size: 11px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 2.5px;
  padding: 8px 24px;
  border-radius: 50px;
  margin-bottom: 28px;
  animation: fadeUp 0.8s var(--ease) 0.1s both;
}

.hero-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: clamp(38px, 6vw, 72px);
  font-weight: 400;
  color: var(--text-primary);
  line-height: 1.1;
  margin-bottom: 20px;
  animation: fadeUp 0.8s var(--ease) 0.25s both;
}

.hero-title span {
  background: linear-gradient(135deg, var(--terra-light), var(--amber));
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
}

.hero-subtitle {
  font-size: clamp(15px, 2vw, 18px);
  color: var(--text-secondary);
  max-width: 540px;
  margin: 0 auto 36px;
  line-height: 1.7;
  animation: fadeUp 0.8s var(--ease) 0.4s both;
}

.hero-actions {
  display: flex;
  gap: 16px;
  justify-content: center;
  flex-wrap: wrap;
  animation: fadeUp 0.8s var(--ease) 0.55s both;
}

@keyframes fadeUp {
  from { opacity: 0; transform: translateY(20px); }
  to { opacity: 1; transform: translateY(0); }
}

.stats-strip {
  display: flex;
  justify-content: center;
  gap: 12px;
  margin-top: 48px;
  animation: fadeUp 0.8s var(--ease) 0.7s both;
}

.stat-item {
  padding: 24px 16px;
  text-align: center;
  background: var(--glass-bg-light);
  border: 1px solid var(--glass-border-subtle);
  border-radius: var(--radius-md);
  transition: all 0.3s var(--ease);
}

.stat-item:hover {
  background: var(--glass-bg);
  border-color: var(--glass-border);
  transform: translateY(-2px);
}

.stat-value {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 36px;
  color: var(--amber);
  line-height: 1;
  margin-bottom: 4px;
}

.stat-label {
  font-size: 10px;
  text-transform: uppercase;
  letter-spacing: 2px;
  color: var(--text-muted);
  font-weight: 600;
}

/* --- Button Styles --------------------------------------- */
.btn-terra {
  background: linear-gradient(135deg, var(--terra) 0%, var(--terra-dark) 100%);
  color: var(--text-primary) !important;
  border: none;
  padding: 15px 36px;
  font-size: 14px;
  font-weight: 700;
  border-radius: 50px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  text-decoration: none;
  display: inline-flex;
  align-items: center;
  gap: 8px;
  box-shadow: 0 4px 20px var(--terra-glow);
}

.btn-terra:hover {
  transform: translateY(-2px) scale(1.03);
  box-shadow: 0 8px 30px var(--terra-glow);
}

.btn-terra:active { transform: translateY(0) scale(0.98); }

.btn-sage {
  background: var(--glass-bg);
  color: var(--sage-light) !important;
  border: 1px solid var(--sage);
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
  padding: 14px 34px;
  font-size: 14px;
  font-weight: 700;
  border-radius: 50px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
}

.btn-sage:hover {
  background: rgba(127, 168, 138, 0.15);
  border-color: var(--sage-light);
  transform: translateY(-2px);
  box-shadow: 0 4px 20px var(--sage-glow);
}

.btn-submit {
  background: linear-gradient(135deg, var(--terra) 0%, var(--terra-dark) 100%);
  color: var(--text-primary) !important;
  border: none;
  padding: 16px 40px;
  font-size: 15px;
  font-weight: 700;
  border-radius: var(--radius-md);
  cursor: pointer;
  transition: all 0.3s var(--ease);
  width: 100%;
  box-shadow: 0 4px 20px var(--terra-glow);
}

.btn-submit:hover {
  transform: translateY(-2px);
  box-shadow: 0 8px 30px var(--terra-glow);
}

/* --- Section Headers ------------------------------------- */
.section-header {
  text-align: center;
  padding: 80px 24px 48px;
}

.section-header h2 {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: clamp(30px, 5vw, 48px);
  color: var(--text-primary);
  margin-bottom: 12px;
  line-height: 1.15;
}

.section-header p {
  font-size: 16px;
  color: var(--text-secondary);
  max-width: 520px;
  margin: 0 auto;
  line-height: 1.6;
}

.section-header .accent-line {
  width: 80px;
  height: 3px;
  background: var(--terra);
  border-radius: 2px;
  margin: 20px auto 0;
  box-shadow: 0 0 20px var(--terra-glow);
}

/* --- Painting Cards -------------------------------------- */
.gallery-wrap {
  padding: 0 24px 80px;
  max-width: 1400px;
  margin: 0 auto;
}

.paintings-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(360px, 1fr));
  gap: 28px;
}

.painting-card {
  position: relative;
  cursor: pointer;
  border-radius: var(--radius-lg);
  overflow: hidden;
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur));
  -webkit-backdrop-filter: blur(var(--glass-blur));
  border: 1px solid var(--glass-border-subtle);
  box-shadow: var(--shadow-glass);
  transition: all 0.4s var(--ease);
  transform-style: preserve-3d;
  display: flex;
  flex-direction: column;
}

.painting-card:hover {
  transform: translateY(-8px);
  border-color: var(--glass-border);
  box-shadow: var(--shadow-glass), 0 0 30px rgba(232, 151, 107, 0.1);
}

.painting-card-img-wrap {
  position: relative;
  overflow: hidden;
  aspect-ratio: 16 / 10;
}

.painting-card-img-wrap::after {
  content: '';
  position: absolute;
  inset: 0;
  background: rgba(15,26,20,0);
  transition: background 0.4s;
}

.painting-card:hover .painting-card-img-wrap::after { background: rgba(15,26,20,0.15); }

.painting-image {
  width: 100%;
  height: 100%;
  object-fit: cover;
  display: block;
  transition: transform 0.6s var(--ease);
}

.painting-card:hover .painting-image { transform: scale(1.05); }

.painting-card-badge {
  position: absolute;
  top: 14px;
  right: 14px;
  background: rgba(15, 26, 20, 0.5);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  border: 1px solid var(--glass-border-subtle);
  color: var(--amber);
  font-size: 11px;
  font-weight: 700;
  padding: 5px 14px;
  border-radius: 20px;
  z-index: 2;
}

.location-status-badge {
  position: absolute;
  top: 14px;
  left: 14px;
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  font-size: 11px;
  font-weight: 700;
  padding: 5px 12px;
  border-radius: 20px;
  z-index: 2;
  letter-spacing: 0.3px;
  border: 1px solid;
}

.location-status-badge.discovered {
  background: rgba(127, 168, 138, 0.25);
  border-color: var(--sage);
  color: var(--sage-light);
}

.location-status-badge.undiscovered {
  background: rgba(255, 255, 255, 0.92);
  border-color: rgba(255, 255, 255, 0.6);
  color: #8B6F1A;
}

body.light-mode .location-status-badge.discovered {
  background: rgba(95, 136, 104, 0.15);
  color: #4A6F52;
}

body.light-mode .location-status-badge.undiscovered {
  background: rgba(255, 255, 255, 0.92);
  color: #8B6F1A;
  border-color: rgba(0, 0, 0, 0.08);
}

.painting-info {
  padding: 24px;
  flex: 1;
  display: flex;
  flex-direction: column;
}

.painting-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 20px;
  color: var(--text-primary);
  margin-bottom: 6px;
  line-height: 1.3;
}

.painting-metadata {
  color: var(--terra-light);
  font-size: 12px;
  font-weight: 600;
  margin-bottom: 10px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.painting-context {
  color: var(--text-secondary);
  font-size: 13px;
  line-height: 1.7;
}

.painting-card-footer {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-top: auto;
  padding-top: 14px;
  gap: 10px;
}

.painting-card-cta {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  color: var(--terra);
  font-weight: 700;
  font-size: 12px;
  text-transform: uppercase;
  letter-spacing: 1px;
  transition: gap 0.3s;
  cursor: pointer;
  white-space: nowrap;
}

.painting-card:hover .painting-card-cta { gap: 10px; }

.painting-card-overlay {
  position: absolute;
  bottom: 0;
  left: 0;
  right: 0;
  padding: 16px 20px;
  background: linear-gradient(to top, rgba(15, 26, 20, 0.85) 0%, rgba(15, 26, 20, 0.4) 60%, transparent 100%);
  z-index: 2;
  pointer-events: none;
}

.painting-card-overlay-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 18px;
  color: #FFFFFF;
  line-height: 1.3;
  margin-bottom: 2px;
  text-shadow: 0 1px 4px rgba(0, 0, 0, 0.5);
}

.painting-card-overlay-meta {
  color: rgba(255, 255, 255, 0.75);
  font-size: 12px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  text-shadow: 0 1px 4px rgba(0, 0, 0, 0.5);
}

/* --- Map (Split Layout) ---------------------------------- */
.map-split-layout {
  display: grid;
  grid-template-columns: 3fr 1fr;
  gap: 24px;
  padding: 0 24px 60px;
  max-width: 1400px;
  margin: 0 auto;
  align-items: start;
}

.map-container {
  border-radius: var(--radius-lg);
  overflow: hidden;
  box-shadow: var(--shadow-glass-lg);
  height: 650px;
  width: 100%;
  border: 1px solid var(--glass-border-subtle);
}

.leaflet-container {
  height: 100%;
  border-radius: var(--radius-lg);
}

.map-info-panel {
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur-heavy));
  -webkit-backdrop-filter: blur(var(--glass-blur-heavy));
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-glass);
  border: 1px solid var(--glass-border);
  padding: 32px;
  height: 650px;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
}

.map-info-placeholder {
  flex: 1;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  text-align: center;
  color: var(--text-secondary);
}

.map-info-placeholder .placeholder-icon {
  font-size: 48px;
  margin-bottom: 16px;
  opacity: 0.3;
}

.map-info-placeholder p {
  font-size: 15px;
  line-height: 1.6;
  max-width: 280px;
  color: var(--text-secondary);
}

.map-info-header {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 20px;
  padding-bottom: 16px;
  border-bottom: 1px solid var(--glass-border-subtle);
}

.map-info-dot {
  width: 14px;
  height: 14px;
  border-radius: 50%;
  flex-shrink: 0;
}

.map-info-dot.painting { background: var(--terra); box-shadow: 0 0 8px var(--terra-glow); }
.map-info-dot.submission { background: #60A5FA; box-shadow: 0 0 8px rgba(96,165,250,0.4); }
.map-info-dot.museum { background: #DC3545; box-shadow: 0 0 8px rgba(220,53,69,0.4); }

.map-info-type-label {
  font-size: 11px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  color: var(--text-muted);
}

.map-info-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 24px;
  color: var(--text-primary);
  margin-bottom: 8px;
  line-height: 1.3;
}

.map-info-meta {
  color: var(--terra-light);
  font-size: 13px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin-bottom: 20px;
}

.map-info-image {
  width: 100%;
  border-radius: var(--radius-sm);
  margin-bottom: 20px;
  box-shadow: var(--shadow-glass);
  max-height: 220px;
  object-fit: cover;
}

.map-info-context {
  color: var(--text-secondary);
  font-size: 14px;
  line-height: 1.7;
  margin-bottom: 20px;
}

.map-info-coords {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 12px;
  margin-top: auto;
}

.coord-box {
  background: var(--glass-bg-light);
  border: 1px solid var(--glass-border-subtle);
  border-radius: var(--radius-sm);
  padding: 12px 14px;
  text-align: center;
}

.coord-label {
  font-size: 10px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  color: var(--text-muted);
  margin-bottom: 4px;
}

.coord-value {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 18px;
  color: var(--text-primary);
}

.map-filter-bar {
  display: flex;
  gap: 8px;
  justify-content: center;
  margin-bottom: 24px;
}

.map-filter-btn {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  font-size: 13px;
  font-weight: 600;
  color: var(--text-secondary);
  padding: 8px 20px;
  border-radius: 50px;
  cursor: pointer;
  border: 1px solid var(--glass-border-subtle);
  background: var(--glass-bg-light);
  transition: all 0.3s var(--ease);
}

.map-filter-btn:hover {
  background: var(--glass-bg);
  border-color: var(--glass-border);
  color: var(--text-primary);
}

.map-filter-btn.active {
  background: var(--glass-bg-strong);
  border-color: var(--terra);
  color: var(--text-primary);
  box-shadow: 0 0 12px var(--terra-glow);
}

.legend-dot {
  width: 10px;
  height: 10px;
  border-radius: 50%;
  display: inline-block;
  flex-shrink: 0;
}

.legend-dot.red { background: var(--terra); }
.legend-dot.blue { background: #60A5FA; }

.locate-me-btn.tracking {
  border-color: #34D399 !important;
  color: #34D399 !important;
  background: rgba(52, 211, 153, 0.12) !important;
  box-shadow: 0 0 12px rgba(52, 211, 153, 0.3);
}

.artist-filter-wrap {
  display: inline-flex;
  align-items: center;
}

.artist-filter-wrap .shiny-input-container {
  margin-bottom: 0 !important;
  width: 180px !important;
}

.artist-filter-wrap .selectize-input {
  padding: 7px 14px !important;
  min-height: 0 !important;
  font-size: 13px !important;
  font-weight: 600 !important;
  border-radius: 50px !important;
  background: var(--glass-bg-light) !important;
  border: 1px solid var(--glass-border-subtle) !important;
  color: var(--text-secondary) !important;
  line-height: 1.4 !important;
}

.artist-filter-wrap .selectize-input.focus {
  border-color: var(--terra) !important;
  box-shadow: 0 0 12px var(--terra-glow) !important;
  background: var(--glass-bg-strong) !important;
  color: var(--text-primary) !important;
}

.artist-filter-wrap .selectize-dropdown {
  border-radius: var(--radius-sm) !important;
  margin-top: 4px !important;
}

.map-info-observations {
  background: var(--glass-bg-light);
  border-left: 3px solid #60A5FA;
  padding: 14px 16px;
  border-radius: 0 var(--radius-sm) var(--radius-sm) 0;
  margin-bottom: 16px;
  font-size: 14px;
  color: var(--text-secondary);
  line-height: 1.6;
  font-style: italic;
}

.map-info-submitter {
  font-size: 13px;
  color: var(--text-muted);
  margin-bottom: 16px;
}

.map-info-cta {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  background: var(--glass-bg-light);
  border: 1px solid var(--terra);
  color: var(--terra);
  font-weight: 700;
  font-size: 12px;
  text-transform: uppercase;
  letter-spacing: 1px;
  padding: 10px 20px;
  border-radius: 50px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  margin-bottom: 20px;
}

.map-info-cta:hover {
  background: rgba(232, 151, 107, 0.15);
  transform: translateY(-2px);
  box-shadow: 0 4px 16px var(--terra-glow);
}

.map-info-cta.museum {
  border-color: #DC3545;
  color: #E25563;
}

.map-info-cta.museum:hover {
  background: rgba(220, 53, 69, 0.15);
  box-shadow: 0 4px 16px rgba(220, 53, 69, 0.3);
}

.map-info-cta.travel {
  border-color: var(--sage);
  color: var(--sage-light);
}

.map-info-cta.travel:hover {
  background: rgba(127, 168, 138, 0.15);
  box-shadow: 0 4px 16px var(--sage-glow);
}

@media (max-width: 1024px) {
  .map-split-layout { grid-template-columns: 1fr; }
  .map-info-panel { height: auto; max-height: 500px; }
  .map-container { height: 500px; }
}

/* --- Form ------------------------------------------------ */
.form-wrap {
  padding: 0 24px 60px;
  max-width: 660px;
  margin: 0 auto;
}

.form-card {
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur-heavy));
  -webkit-backdrop-filter: blur(var(--glass-blur-heavy));
  padding: 48px;
  border-radius: var(--radius-lg);
  border: 1px solid var(--glass-border);
  box-shadow: var(--shadow-glass);
}

.form-card .form-group { margin-bottom: 24px; }

.form-card label,
.form-card .control-label {
  font-weight: 600;
  color: var(--text-secondary);
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  margin-bottom: 8px;
  display: block;
}

.form-card .form-control,
.form-card .shiny-input-container input[type='text'],
.form-card .shiny-input-container input[type='number'],
.form-card .shiny-input-container select,
.form-card .shiny-input-container textarea {
  width: 100%;
  padding: 14px 16px;
  border: 1px solid var(--glass-border-subtle);
  border-radius: var(--radius-sm);
  font-size: 15px;
  font-family: 'DM Sans', sans-serif;
  transition: all 0.3s var(--ease);
  background: rgba(255, 255, 255, 0.06);
  color: var(--text-primary);
}

.form-card .form-control:focus,
.form-card input:focus,
.form-card select:focus,
.form-card textarea:focus {
  outline: none;
  border-color: var(--sage);
  box-shadow: 0 0 0 4px var(--sage-glow);
  background: rgba(255, 255, 255, 0.1);
}

.upload-zone {
  border: 3px dashed var(--glass-border-subtle);
  border-radius: var(--radius-md);
  padding: 40px 24px;
  text-align: center;
  transition: all 0.3s var(--ease);
  background: var(--glass-bg-light);
  cursor: pointer;
  color: var(--text-secondary);
}

.upload-zone:hover {
  border-color: var(--sage);
  background: rgba(127, 168, 138, 0.08);
}

.upload-icon {
  font-size: 36px;
  margin-bottom: 8px;
}

/* --- Comparisons ----------------------------------------- */
.comparison-wrap {
  padding: 0 24px 60px;
  max-width: 1400px;
  margin: 0 auto;
}

.comparison-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
  gap: 24px;
}

.comparison-thumb {
  position: relative;
  aspect-ratio: 16 / 10;
  border-radius: var(--radius-md);
  overflow: hidden;
  cursor: pointer;
  border: 1px solid var(--glass-border-subtle);
  box-shadow: var(--shadow-glass);
  transition: all 0.4s var(--ease);
}

.comparison-thumb:hover {
  transform: translateY(-6px) scale(1.02);
  border-color: var(--glass-border);
  box-shadow: var(--shadow-glass), 0 0 30px rgba(232,151,107,0.1);
}

.comparison-thumb img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  transition: transform 0.5s var(--ease);
}

.comparison-thumb:hover img { transform: scale(1.06); }

.comparison-thumb-overlay {
  position: absolute;
  inset: 0;
  background: linear-gradient(to top, rgba(15,26,20,0.8) 0%, transparent 50%);
  display: flex;
  align-items: flex-end;
  padding: 20px;
  opacity: 0;
  transition: opacity 0.3s;
}

.comparison-thumb:hover .comparison-thumb-overlay { opacity: 1; }

.comparison-thumb-label {
  color: var(--text-primary);
  font-weight: 700;
  font-size: 13px;
  display: flex;
  align-items: center;
  gap: 6px;
  background: var(--glass-bg);
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
  padding: 6px 14px;
  border-radius: 20px;
  border: 1px solid var(--glass-border-subtle);
}

.comparison-thumb-submitter {
  position: absolute;
  top: 12px;
  left: 12px;
  z-index: 2;
  background: rgba(15, 26, 20, 0.55);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  border: 1px solid var(--glass-border-subtle);
  color: var(--text-primary);
  font-size: 11px;
  font-weight: 600;
  padding: 5px 12px;
  border-radius: 20px;
  letter-spacing: 0.3px;
}

.no-comparisons {
  text-align: center;
  padding: 80px 24px;
  color: var(--text-secondary);
  font-size: 18px;
}

.compare-filter-banner {
  display: flex;
  align-items: center;
  justify-content: space-between;
  flex-wrap: wrap;
  gap: 12px;
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur));
  -webkit-backdrop-filter: blur(var(--glass-blur));
  border: 1px solid var(--glass-border);
  border-radius: var(--radius-md);
  padding: 16px 24px;
  margin-bottom: 28px;
  max-width: 1400px;
  margin-left: auto;
  margin-right: auto;
}

.compare-filter-text {
  color: var(--text-secondary);
  font-size: 14px;
}

.compare-filter-text strong { color: var(--text-primary); }

.compare-filter-see-all {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  background: var(--glass-bg-light);
  border: 1px solid var(--terra);
  color: var(--terra);
  font-weight: 700;
  font-size: 12px;
  text-transform: uppercase;
  letter-spacing: 1px;
  padding: 8px 20px;
  border-radius: 50px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
}

.compare-filter-see-all:hover {
  background: rgba(232, 151, 107, 0.15);
  transform: translateY(-2px);
  box-shadow: 0 4px 16px var(--terra-glow);
}

/* --- Lightbox Close Button ------------------------------- */
.lightbox-close {
  position: absolute;
  top: 24px;
  right: 24px;
  border-radius: 50%;
  cursor: pointer;
  transition: all 0.3s;
  z-index: 10;
  color: var(--text-primary);
  font-size: 28px;
  width: 48px;
  height: 48px;
  display: flex;
  align-items: center;
  justify-content: center;
  background: var(--glass-bg);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  border: 1px solid var(--glass-border-subtle);
}

.lightbox-close:hover {
  transform: rotate(90deg) scale(1.1);
  background: var(--glass-bg-strong);
}

/* --- Comparison Lightbox (Side-by-side) ------------------ */
#comparison-lightbox {
  display: none;
  position: fixed;
  inset: 0;
  background: rgba(8, 12, 10, 0.97);
  z-index: 10001;
}

#comparison-lightbox.active { display: flex; }

.comparison-container {
  width: 100%;
  height: 100%;
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 4px;
  padding: 70px 32px;
}

.comparison-side {
  position: relative;
  overflow: hidden;
  background: #000;
  border-radius: var(--radius-sm);
  border: 1px solid var(--glass-border-subtle);
}

.comparison-side img {
  width: 100%;
  height: 100%;
  object-fit: contain;
  transition: transform 0.1s ease;
}

.comparison-label {
  position: absolute;
  top: 16px;
  left: 16px;
  background: var(--glass-bg-strong);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  padding: 8px 18px;
  border-radius: 20px;
  font-size: 13px;
  font-weight: 700;
  color: var(--amber);
  border: 1px solid var(--glass-border-subtle);
}

/* --- Contribute Landing Cards ---------------------------- */
.contribute-landing {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  gap: 24px;
  max-width: 760px;
  margin: 0 auto;
  padding: 0 24px 60px;
}

.contribute-type-card {
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur));
  -webkit-backdrop-filter: blur(var(--glass-blur));
  border: 1px solid var(--glass-border-subtle);
  border-radius: var(--radius-lg);
  padding: 40px 28px;
  text-align: center;
  cursor: pointer;
  transition: all 0.4s var(--ease);
  box-shadow: var(--shadow-glass);
}

.contribute-type-card:hover {
  transform: translateY(-8px);
  border-color: var(--glass-border);
  box-shadow: var(--shadow-glass), 0 0 30px rgba(232,151,107,0.1);
}

.contribute-type-icon {
  font-size: 48px;
  margin-bottom: 20px;
  display: block;
}

.contribute-type-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: 22px;
  color: var(--text-primary);
  margin-bottom: 10px;
}

.contribute-type-desc {
  color: var(--text-secondary);
  font-size: 14px;
  line-height: 1.6;
  margin-bottom: 20px;
}

.contribute-type-cta {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  color: var(--terra);
  font-weight: 700;
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 1px;
  transition: gap 0.3s;
}

.contribute-type-card:hover .contribute-type-cta { gap: 10px; }

.contribute-back-btn {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  background: var(--glass-bg-light);
  border: 1px solid var(--glass-border-subtle);
  color: var(--text-secondary);
  font-weight: 600;
  font-size: 13px;
  padding: 8px 20px;
  border-radius: 50px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  margin-bottom: 24px;
}

.contribute-back-btn:hover {
  background: var(--glass-bg);
  border-color: var(--glass-border);
  color: var(--text-primary);
}

/* --- Admin ----------------------------------------------- */
.admin-wrap {
  max-width: 1200px;
  margin: 0 auto;
  padding: 0 24px 60px;
}

.admin-login-card {
  max-width: 400px;
  margin: 0 auto;
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur-heavy));
  -webkit-backdrop-filter: blur(var(--glass-blur-heavy));
  padding: 48px;
  border-radius: var(--radius-lg);
  border: 1px solid var(--glass-border);
  box-shadow: var(--shadow-glass);
  text-align: center;
}

.admin-login-card h3 {
  font-family: 'DM Serif Display', Georgia, serif;
  color: var(--text-primary);
  font-size: 28px;
}

.admin-login-card p {
  color: var(--text-secondary);
  font-size: 14px;
}

.admin-login-card .form-control {
  padding: 14px 16px;
  border: 1px solid var(--glass-border-subtle);
  border-radius: var(--radius-sm);
  width: 100%;
  transition: all 0.3s;
  background: rgba(255, 255, 255, 0.06);
  color: var(--text-primary);
}

.admin-login-card .form-control:focus {
  border-color: var(--sage);
  box-shadow: 0 0 0 4px var(--sage-glow);
  outline: none;
  background: rgba(255, 255, 255, 0.1);
}

.admin-toolbar {
  display: flex;
  gap: 12px;
  flex-wrap: wrap;
  margin-bottom: 24px;
}

.admin-toolbar .btn {
  border-radius: var(--radius-sm);
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  cursor: pointer;
  transition: all 0.3s;
  border: 1px solid;
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
}

.admin-toolbar .btn-approve {
  background: rgba(127, 168, 138, 0.2);
  border-color: var(--sage);
  color: var(--sage-light);
}
.admin-toolbar .btn-approve:hover { background: rgba(127, 168, 138, 0.35); }

.admin-toolbar .btn-reject {
  background: rgba(232, 151, 107, 0.1);
  border-color: var(--terra);
  color: var(--terra-light);
}
.admin-toolbar .btn-reject:hover { background: rgba(232, 151, 107, 0.2); }

.admin-toolbar .btn-refresh {
  background: var(--glass-bg-light);
  border-color: var(--glass-border);
  color: var(--text-secondary);
}
.admin-toolbar .btn-refresh:hover { background: var(--glass-bg); }

.dataTables_wrapper {
  background: var(--glass-bg-strong);
  backdrop-filter: blur(var(--glass-blur));
  -webkit-backdrop-filter: blur(var(--glass-blur));
  border: 1px solid var(--glass-border-subtle);
  border-radius: var(--radius-md);
  padding: 24px;
  box-shadow: var(--shadow-glass);
  color: var(--text-primary);
}

table.dataTable thead th {
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1px;
  color: var(--text-secondary);
  border-bottom: 1px solid var(--glass-border-subtle) !important;
}

table.dataTable tbody td {
  color: var(--text-secondary);
  border-bottom: 1px solid var(--glass-border-subtle) !important;
}

table.dataTable tbody tr:hover { background: var(--glass-bg-light) !important; }
table.dataTable tbody tr.selected { background: rgba(232, 151, 107, 0.15) !important; }

.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter,
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate {
  color: var(--text-muted) !important;
}

.dataTables_wrapper .dataTables_filter input {
  background: rgba(255,255,255,0.06);
  border: 1px solid var(--glass-border-subtle);
  color: var(--text-primary);
  border-radius: var(--radius-sm);
  padding: 6px 12px;
}

.dataTables_wrapper .dataTables_paginate .paginate_button {
  color: var(--text-secondary) !important;
  border: 1px solid var(--glass-border-subtle) !important;
  background: var(--glass-bg-light) !important;
  border-radius: 6px !important;
}

.dataTables_wrapper .dataTables_paginate .paginate_button.current {
  background: var(--glass-bg-strong) !important;
  border-color: var(--terra) !important;
  color: var(--terra-light) !important;
}

/* --- Alerts ---------------------------------------------- */
.alert-success-custom {
  background: rgba(127, 168, 138, 0.15);
  border: 1px solid var(--sage);
  color: var(--sage-light);
  padding: 16px 20px;
  border-radius: var(--radius-sm);
  margin-bottom: 24px;
  font-weight: 600;
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
}

.alert-error-custom {
  background: rgba(232, 151, 107, 0.15);
  border: 1px solid var(--terra);
  color: var(--terra-light);
  padding: 16px 20px;
  border-radius: var(--radius-sm);
  margin-bottom: 24px;
  font-weight: 600;
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
}

/* --- Responsive ------------------------------------------ */
html, body {
  max-width: 100vw;
  overflow-x: hidden;
}

@supports (padding-bottom: env(safe-area-inset-bottom)) {
  .navbar { padding-left: env(safe-area-inset-left); padding-right: env(safe-area-inset-right); }
}

.map-info-panel,
.admin-wrap,
.gallery-wrap,
.comparison-wrap,
.form-wrap {
  -webkit-overflow-scrolling: touch;
}

/* Tablet (≤ 1024px) */
@media (max-width: 1024px) {
  .paintings-grid { grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); }
}

/* Phone (≤ 768px) */
@media (max-width: 768px) {
  .navbar { min-height: 56px; }
  .navbar-nav .nav-link { padding: 14px 18px !important; font-size: 12px !important; }
  .navbar-collapse { background: var(--surface-dark-mid); border-top: 1px solid var(--glass-border-subtle); padding: 8px 0; }
  .theme-toggle { margin: 8px 12px; }

  .section-header { padding: 32px 16px 24px; }
  .section-header h2 { font-size: 28px; }
  .section-header p { font-size: 14px; }

  .hero-banner { min-height: 100vh; min-height: 100svh; }
  .hero-inner { padding: 32px 20px; margin: 0 12px; border-radius: var(--radius-lg); }
  .hero-title { font-size: 36px; }
  .hero-subtitle { font-size: 14px; margin-bottom: 24px; }
  .hero-actions { flex-direction: column; align-items: center; }
  .hero-actions .btn-terra,
  .hero-actions .btn-sage { width: 100%; max-width: 280px; justify-content: center; }
  .stats-strip { gap: 8px; margin-top: 32px; }
  .stat-item { padding: 16px 10px; }
  .stat-value { font-size: 28px; }
  .stat-label { font-size: 9px; letter-spacing: 1.5px; }

  .gallery-wrap { padding: 0 12px 48px; }
  .paintings-grid { grid-template-columns: 1fr; gap: 20px; }
  .painting-card { border-radius: var(--radius-md); }
  .painting-info { padding: 16px; }
  .painting-title { font-size: 18px; }
  .painting-card-footer { flex-direction: column; align-items: flex-start; gap: 8px; }

  .map-filter-bar { flex-wrap: wrap; padding: 0 12px; gap: 6px; justify-content: center; }
  .map-filter-btn { font-size: 11px; padding: 6px 12px; gap: 5px; min-height: 36px; }
  .artist-filter-wrap .shiny-input-container { width: 140px !important; }
  .artist-filter-wrap .selectize-input { font-size: 11px !important; padding: 6px 10px !important; }

  .map-split-layout { grid-template-columns: 1fr; padding: 0 12px 40px; gap: 16px; }
  .map-container { height: 55vh; height: 55svh; min-height: 300px; max-height: 450px; border-radius: var(--radius-md); }
  .map-info-panel { height: auto; max-height: 400px; padding: 20px; border-radius: var(--radius-md); }
  .map-info-title { font-size: 20px; }
  .map-info-image { max-height: 180px; }
  .map-info-coords { gap: 8px; }
  .coord-value { font-size: 15px; }
  .map-info-cta { font-size: 11px; padding: 8px 14px; }

  .form-wrap { padding: 0 12px 40px; max-width: 100%; }
  .form-card { padding: 24px 16px; border-radius: var(--radius-md); }

  .contribute-landing { grid-template-columns: 1fr; padding: 0 12px 40px; gap: 16px; }
  .contribute-type-card { padding: 28px 20px; }
  .contribute-type-icon { font-size: 36px; margin-bottom: 14px; }
  .contribute-type-title { font-size: 18px; }

  .comparison-wrap { padding: 0 12px 48px; }
  .comparison-grid { grid-template-columns: 1fr; gap: 16px; }
  .compare-filter-banner { flex-direction: column; align-items: flex-start; padding: 12px 16px; gap: 8px; }

  .comparison-container { grid-template-columns: 1fr; padding: 60px 12px 100px; gap: 8px; }
  .comparison-side { border-radius: 8px; }
  .comparison-label { font-size: 11px; padding: 5px 12px; top: 8px; left: 8px; }
  .lightbox-close { top: 12px; right: 12px; width: 40px; height: 40px; font-size: 22px; }

  #museum-photo-lightbox { padding: 56px 12px 24px !important; }
  #museum-lb-prev, #museum-lb-next { width: 36px !important; height: 36px !important; font-size: 24px !important; }
  #museum-lb-prev { left: 8px !important; }
  #museum-lb-next { right: 8px !important; }
  #museum-lb-img { max-height: 55vh !important; }

  .admin-wrap { padding: 0 12px 40px; max-width: 100vw; overflow-x: hidden; }
  .admin-login-card { padding: 32px 20px; margin: 0; }
  .admin-toolbar { display: grid !important; grid-template-columns: 1fr 1fr !important; gap: 8px !important; width: 100% !important; }
  .admin-toolbar > * { min-width: 0 !important; width: 100% !important; box-sizing: border-box !important; }
  .admin-toolbar .btn { font-size: 11px !important; padding: 10px 6px !important; width: 100% !important; text-align: center !important; white-space: nowrap !important; overflow: hidden !important; text-overflow: ellipsis !important; }
  .admin-toolbar .artist-filter-wrap { grid-column: 1 / -1; }
  .admin-toolbar .artist-filter-wrap .shiny-input-container { width: 100% !important; }
  .dataTables_wrapper { padding: 10px; border-radius: var(--radius-sm); max-width: 100%; overflow: hidden; }
  .dataTables_wrapper table { display: block; overflow-x: auto; -webkit-overflow-scrolling: touch; white-space: nowrap; }
  .dataTables_wrapper .dataTables_filter,
  .dataTables_wrapper .dataTables_length { width: 100%; text-align: left !important; margin-bottom: 8px; }
  .dataTables_wrapper .dataTables_filter input { width: 100% !important; max-width: 100% !important; }

  .alert-success-custom,
  .alert-error-custom { font-size: 13px; padding: 12px 14px; }
}

/* Small phone (≤ 390px) */
@media (max-width: 390px) {
  .hero-title { font-size: 30px; }
  .hero-subtitle { font-size: 13px; }
  .hero-inner { padding: 24px 14px; margin: 0 8px; }
  .section-header h2 { font-size: 24px; }
  .map-filter-bar { gap: 4px; }
  .map-filter-btn { font-size: 10px; padding: 5px 8px; }
  .artist-filter-wrap .shiny-input-container { width: 120px !important; }
  .map-container { height: 50vh; height: 50svh; min-height: 260px; max-height: 380px; }
  .painting-title { font-size: 16px; }
  .painting-info { padding: 14px; }
  .contribute-type-card { padding: 24px 16px; }
  .contribute-type-title { font-size: 16px; }
  .splash-title { font-size: 32px; }
  .splash-tagline { font-size: 13px; }
}

/* --- Splash Screen Overlay ------------------------------- */
.splash-overlay {
  position: fixed;
  inset: 0;
  z-index: 99999;
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  background-color: #F3EDE4;
  background-image:
    linear-gradient(180deg, rgba(243,237,228,0.3) 0%, rgba(243,237,228,0.75) 100%),
    url('https://upload.wikimedia.org/wikipedia/commons/thumb/4/4a/Albert_Bierstadt_-_Among_the_Sierra_Nevada%2C_California_-_Google_Art_Project.jpg/2560px-Albert_Bierstadt_-_Among_the_Sierra_Nevada%2C_California_-_Google_Art_Project.jpg');
  background-size: cover;
  background-position: center;
  transition: opacity 0.8s ease, visibility 0.8s ease;
}

.splash-overlay.fade-out {
  opacity: 0;
  visibility: hidden;
  pointer-events: none;
}

.splash-orb {
  position: absolute;
  border-radius: 50%;
  filter: blur(80px);
  pointer-events: none;
  opacity: 0;
  animation: orbFloat 12s ease-in-out infinite;
}

.splash-orb-1 { width: 400px; height: 400px; background: rgba(194, 113, 79, 0.1); top: -5%; right: -5%; animation-delay: 0s; animation-duration: 14s; }
.splash-orb-2 { width: 350px; height: 350px; background: rgba(95, 136, 104, 0.08); bottom: -8%; left: -5%; animation-delay: 3s; animation-duration: 16s; }
.splash-orb-3 { width: 250px; height: 250px; background: rgba(184, 148, 42, 0.07); top: 40%; left: 30%; animation-delay: 6s; animation-duration: 18s; }
.splash-orb-4 { width: 300px; height: 300px; background: rgba(194, 113, 79, 0.05); bottom: 20%; right: 15%; animation-delay: 9s; animation-duration: 20s; }

@keyframes orbFloat {
  0%   { opacity: 0; transform: translate(0, 0) scale(0.8); }
  25%  { opacity: 1; }
  50%  { opacity: 1; transform: translate(30px, -20px) scale(1.1); }
  75%  { opacity: 1; }
  100% { opacity: 0; transform: translate(-20px, 15px) scale(0.8); }
}

.splash-inner {
  text-align: center;
  max-width: 720px;
  padding: 48px;
  animation: fadeUp 1s ease 0.2s both;
}

.splash-badge {
  display: inline-block;
  background: rgba(232, 151, 107, 0.15);
  border: 1px solid rgba(232, 151, 107, 0.35);
  color: var(--terra-light);
  font-size: 11px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 2.5px;
  padding: 8px 24px;
  border-radius: 50px;
  margin-bottom: 24px;
}

.splash-title {
  font-family: 'DM Serif Display', Georgia, serif;
  font-size: clamp(42px, 7vw, 80px);
  font-weight: 400;
  color: #2D2D2D;
  line-height: 1.08;
  margin-bottom: 16px;
  text-shadow: 0 2px 20px rgba(255,255,255,0.3);
}

.splash-title span {
  background: linear-gradient(135deg, var(--terra-light), var(--amber));
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
}

.splash-tagline {
  font-size: clamp(15px, 2vw, 19px);
  color: rgba(45, 45, 45, 0.7);
  max-width: 480px;
  margin: 0 auto 40px;
  line-height: 1.7;
}

.splash-nav {
  display: flex;
  gap: 14px;
  justify-content: center;
  flex-wrap: wrap;
  margin-bottom: 36px;
}

.splash-nav-btn {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  padding: 14px 30px;
  border-radius: 50px;
  font-size: 13px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.8px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  border: 1px solid;
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
}

.splash-nav-btn.primary {
  background: linear-gradient(135deg, var(--terra) 0%, var(--terra-dark) 100%);
  border-color: transparent;
  color: white;
  box-shadow: 0 4px 20px var(--terra-glow);
}

.splash-nav-btn.primary:hover {
  transform: translateY(-3px) scale(1.03);
  box-shadow: 0 8px 30px var(--terra-glow);
}

.splash-nav-btn.secondary {
  background: rgba(255,255,255,0.08);
  border-color: rgba(255,255,255,0.25);
  color: rgba(255,255,255,0.9);
}

.splash-nav-btn.secondary:hover {
  background: rgba(255,255,255,0.15);
  border-color: rgba(255,255,255,0.4);
  transform: translateY(-2px);
}

.splash-hint {
  font-size: 12px;
  color: rgba(45, 45, 45, 0.3);
  letter-spacing: 1px;
  text-transform: uppercase;
  animation: pulse 2s ease-in-out infinite;
}

@keyframes pulse {
  0%, 100% { opacity: 0.3; }
  50% { opacity: 0.6; }
}

@media (max-width: 768px) {
  .splash-inner { padding: 32px 16px; }
  .splash-nav { flex-direction: column; align-items: center; gap: 10px; }
  .splash-nav-btn { width: 100%; max-width: 260px; justify-content: center; }
  .splash-title { font-size: 40px; }
  .splash-tagline { font-size: 14px; margin-bottom: 28px; }
  .splash-orb { display: none; }
  .splash-badge { font-size: 10px; padding: 6px 18px; }
}

/* --- Search Bar ------------------------------------------ */
.tab-search-wrap {
  display: flex;
  justify-content: center;
  margin: 0 auto 32px;
  padding: 0 24px;
  position: relative;
  width: fit-content;
  max-width: 100%;
}

.tab-search-input {
  width: auto;
  min-width: 20ch;
  padding: 12px 18px 12px 44px;
  border: 1px solid var(--glass-border-subtle);
  border-radius: 50px;
  font-family: 'DM Sans', sans-serif;
  font-size: 14px;
  font-weight: 500;
  color: var(--text-primary);
  background: var(--glass-bg);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  transition: all 0.3s var(--ease);
  outline: none;
  -webkit-appearance: none;
  appearance: none;
}

.tab-search-input::placeholder { color: var(--text-muted); }

.tab-search-input:focus {
  border-color: var(--terra);
  box-shadow: 0 0 0 4px var(--terra-glow);
  background: var(--glass-bg-strong);
}

.tab-search-icon {
  position: absolute;
  left: 40px;
  top: 50%;
  transform: translateY(-50%);
  font-size: 16px;
  color: var(--text-muted);
  pointer-events: none;
  transition: color 0.3s;
}

.tab-search-input:focus + .tab-search-icon,
.tab-search-input:not(:placeholder-shown) + .tab-search-icon {
  color: var(--terra);
}

@media (max-width: 768px) {
  .tab-search-wrap { padding: 0 12px; margin-bottom: 20px; }
  .tab-search-input { font-size: 13px; padding: 10px 14px 10px 38px; }
  .tab-search-icon { left: 26px; font-size: 14px; }
}

/* --- Shiny Specific Overrides ---------------------------- */
.shiny-input-container { width: 100% !important; }

.selectize-input {
  border: 1px solid var(--glass-border-subtle) !important;
  border-radius: var(--radius-sm) !important;
  padding: 10px 14px !important;
  background: rgba(255, 255, 255, 0.06) !important;
  color: var(--text-primary) !important;
}

.selectize-input.focus {
  border-color: var(--sage) !important;
  box-shadow: 0 0 0 4px var(--sage-glow) !important;
  background: rgba(255, 255, 255, 0.1) !important;
}

.selectize-dropdown {
  background: var(--surface-dark-mid) !important;
  border: 1px solid var(--glass-border-subtle) !important;
  border-radius: var(--radius-sm) !important;
}

.selectize-dropdown .option { color: var(--text-secondary) !important; }
.selectize-dropdown .option.active { background: var(--glass-bg) !important; color: var(--text-primary) !important; }
.selectize-input .item { color: var(--text-primary) !important; }

.nav-pills .nav-link { color: var(--text-secondary) !important; }
.nav-pills .nav-link.active { background: var(--glass-bg) !important; color: var(--text-primary) !important; }

::-webkit-scrollbar { width: 8px; }
::-webkit-scrollbar-track { background: var(--surface-dark); }
::-webkit-scrollbar-thumb { background: rgba(255,255,255,0.15); border-radius: 4px; }
::-webkit-scrollbar-thumb:hover { background: rgba(255,255,255,0.25); }

/* --- Light/Dark Mode Toggle ------------------------------ */
.theme-toggle {
  background: var(--glass-bg);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  border: 1px solid var(--glass-border-subtle);
  color: var(--text-secondary);
  width: 40px;
  height: 40px;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  font-size: 18px;
  margin: 12px 16px 12px 0;
  flex-shrink: 0;
}

.theme-toggle:hover {
  background: var(--glass-bg-strong);
  border-color: var(--glass-border);
  color: var(--amber);
  transform: scale(1.1);
}

/* --- Light Mode Overrides -------------------------------- */
body.light-mode {
  --glass-bg: rgba(0, 0, 0, 0.04);
  --glass-bg-strong: rgba(0, 0, 0, 0.07);
  --glass-bg-light: rgba(0, 0, 0, 0.02);
  --glass-border: rgba(0, 0, 0, 0.12);
  --glass-border-subtle: rgba(0, 0, 0, 0.06);
  --surface-dark: #F3EDE4;
  --surface-dark-mid: #EAE2D8;
  --surface-card: rgba(0, 0, 0, 0.04);
  --text-primary: #2D2D2D;
  --text-secondary: rgba(45, 45, 45, 0.7);
  --text-muted: rgba(45, 45, 45, 0.45);
  --terra: #C2714F;
  --terra-dark: #A85D3F;
  --terra-light: #9E5A3C;
  --terra-glow: rgba(194, 113, 79, 0.25);
  --sage: #5F8868;
  --sage-dark: #4A6F52;
  --sage-light: #4A6F52;
  --sage-glow: rgba(95, 136, 104, 0.2);
  --amber: #B8942A;
  --amber-light: #A07E1F;
  --amber-glow: rgba(184, 148, 42, 0.2);
  --shadow-glass: 0 4px 20px rgba(0, 0, 0, 0.08);
  --shadow-glass-lg: 0 8px 32px rgba(0, 0, 0, 0.1);
  --shadow-glow: 0 0 30px rgba(194, 113, 79, 0.08);
}

body.light-mode .navbar {
  background: rgba(243, 237, 228, 0.85) !important;
  border-bottom-color: rgba(0, 0, 0, 0.08) !important;
  box-shadow: 0 2px 20px rgba(0, 0, 0, 0.06);
}

body.light-mode .hero-banner {
  background:
    linear-gradient(180deg, rgba(245,240,235,0.2) 0%, rgba(245,240,235,0.55) 100%),
    url('https://upload.wikimedia.org/wikipedia/commons/thumb/4/4a/Albert_Bierstadt_-_Among_the_Sierra_Nevada%2C_California_-_Google_Art_Project.jpg/2560px-Albert_Bierstadt_-_Among_the_Sierra_Nevada%2C_California_-_Google_Art_Project.jpg');
  background-size: cover;
  background-position: center;
}

body.light-mode .hero-inner { background: rgba(255, 255, 255, 0.65); border-color: rgba(0, 0, 0, 0.1); }
body.light-mode .painting-card-badge { background: rgba(255, 255, 255, 0.75); border-color: rgba(0, 0, 0, 0.08); }
body.light-mode .lightbox-close { background: rgba(255, 255, 255, 0.7); border-color: rgba(0, 0, 0, 0.1); color: var(--text-primary); }

body.light-mode .dataTables_wrapper .dataTables_filter input {
  background: rgba(0, 0, 0, 0.03);
  border-color: rgba(0, 0, 0, 0.1);
  color: var(--text-primary);
}

body.light-mode .dataTables_wrapper { background: rgba(255, 255, 255, 0.75) !important; border-color: rgba(0, 0, 0, 0.1) !important; color: #2D2D2D !important; }

body.light-mode table.dataTable thead th,
body.light-mode .dataTables_wrapper table.dataTable thead th { color: rgba(45, 45, 45, 0.65) !important; border-bottom-color: rgba(0, 0, 0, 0.1) !important; background: transparent !important; background-color: transparent !important; }

body.light-mode table.dataTable tbody td,
body.light-mode .dataTables_wrapper table.dataTable tbody td { color: #2D2D2D !important; border-bottom-color: rgba(0, 0, 0, 0.06) !important; }

body.light-mode table.dataTable tbody tr,
body.light-mode .dataTables_wrapper table.dataTable tbody tr { background: transparent !important; background-color: transparent !important; }

body.light-mode table.dataTable tbody tr:hover,
body.light-mode .dataTables_wrapper table.dataTable tbody tr:hover { background: rgba(0, 0, 0, 0.04) !important; background-color: rgba(0, 0, 0, 0.04) !important; }

body.light-mode table.dataTable tbody tr.selected,
body.light-mode .dataTables_wrapper table.dataTable tbody tr.selected { background: rgba(194, 113, 79, 0.12) !important; background-color: rgba(194, 113, 79, 0.12) !important; }

body.light-mode table.dataTable tbody tr.selected td { color: #2D2D2D !important; }

body.light-mode .dataTables_wrapper .dataTables_length,
body.light-mode .dataTables_wrapper .dataTables_filter,
body.light-mode .dataTables_wrapper .dataTables_info,
body.light-mode .dataTables_wrapper .dataTables_paginate { color: rgba(45, 45, 45, 0.55) !important; }

body.light-mode .dataTables_wrapper .dataTables_paginate .paginate_button { color: rgba(45, 45, 45, 0.65) !important; border-color: rgba(0, 0, 0, 0.08) !important; background: rgba(0, 0, 0, 0.02) !important; background-color: rgba(0, 0, 0, 0.02) !important; }
body.light-mode .dataTables_wrapper .dataTables_paginate .paginate_button:hover { color: #2D2D2D !important; background: rgba(0, 0, 0, 0.06) !important; background-color: rgba(0, 0, 0, 0.06) !important; border-color: rgba(0, 0, 0, 0.15) !important; }
body.light-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current { background: rgba(194, 113, 79, 0.12) !important; background-color: rgba(194, 113, 79, 0.12) !important; border-color: #C2714F !important; color: #C2714F !important; }

body.light-mode .dataTables_wrapper .dataTables_length select { background: rgba(0, 0, 0, 0.03) !important; border: 1px solid rgba(0, 0, 0, 0.1) !important; color: #2D2D2D !important; border-radius: 6px; }

body.light-mode .admin-toolbar .btn-approve { background: rgba(95, 136, 104, 0.12) !important; border-color: #5F8868 !important; color: #5F8868 !important; }
body.light-mode .admin-toolbar .btn-reject { background: rgba(194, 113, 79, 0.08) !important; border-color: #C2714F !important; color: #C2714F !important; }
body.light-mode .admin-toolbar .btn-refresh { background: rgba(0, 0, 0, 0.03) !important; border-color: rgba(0, 0, 0, 0.12) !important; color: rgba(45, 45, 45, 0.7) !important; }
body.light-mode .admin-toolbar .btn[style*='DC3545'] { background: rgba(220, 53, 69, 0.1) !important; border-color: #DC3545 !important; color: #DC3545 !important; }

body.light-mode .selectize-input { background: rgba(0, 0, 0, 0.03) !important; border-color: rgba(0, 0, 0, 0.1) !important; color: var(--text-primary) !important; }
body.light-mode .selectize-dropdown { background: #F3EDE4 !important; border-color: rgba(0, 0, 0, 0.1) !important; }

body.light-mode .form-card .form-control,
body.light-mode .form-card input,
body.light-mode .form-card select,
body.light-mode .form-card textarea { background: rgba(0, 0, 0, 0.03); color: var(--text-primary); }

body.light-mode .admin-login-card .form-control { background: rgba(0, 0, 0, 0.03); color: var(--text-primary); }

body.light-mode ::-webkit-scrollbar-track { background: #F3EDE4; }
body.light-mode ::-webkit-scrollbar-thumb { background: rgba(0, 0, 0, 0.15); }

body.light-mode .comparison-thumb-submitter { background: rgba(255, 255, 255, 0.75); border-color: rgba(0, 0, 0, 0.08); color: var(--amber); }

body.light-mode .mobile-tab-bar { background: rgba(243, 237, 228, 0.94); border-bottom-color: rgba(0, 0, 0, 0.08); }
body.light-mode .mobile-tab-bar .mob-tab { color: rgba(45, 45, 45, 0.4); }
body.light-mode .mobile-tab-bar .mob-tab.active { color: var(--terra); border-bottom-color: var(--terra); }

body.light-mode .form-card .form-control::placeholder,
body.light-mode .form-card input::placeholder,
body.light-mode .form-card textarea::placeholder { color: rgba(45, 45, 45, 0.4) !important; opacity: 1; }

body.light-mode .form-card .selectize-input .item { color: var(--text-primary) !important; }
body.light-mode .form-card .selectize-input input::placeholder { color: rgba(45, 45, 45, 0.4) !important; opacity: 1; }

/* --- Mobile Top Tab Bar ---------------------------------- */
.mobile-tab-bar { display: none; }

@media (max-width: 768px) {
  .navbar-toggler { display: none !important; }
  .navbar-collapse { display: none !important; }
  .theme-toggle { display: none !important; }
  .navbar { display: none !important; }

  .mobile-tab-bar {
    display: flex;
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    z-index: 998;
    background: rgba(15, 26, 20, 0.94);
    backdrop-filter: blur(var(--glass-blur-heavy));
    -webkit-backdrop-filter: blur(var(--glass-blur-heavy));
    border-bottom: 1px solid var(--glass-border-subtle);
    padding: 0;
    justify-content: stretch;
    align-items: stretch;
  }

  .mob-tab {
    display: flex;
    align-items: center;
    justify-content: center;
    flex: 1;
    color: var(--text-muted);
    font-size: 11px;
    font-weight: 700;
    letter-spacing: 0.6px;
    text-transform: uppercase;
    cursor: pointer;
    transition: color 0.2s, border-color 0.2s;
    -webkit-tap-highlight-color: transparent;
    text-decoration: none;
    border: none;
    background: none;
    outline: none;
    padding: 12px 4px;
    border-bottom: 2px solid transparent;
    white-space: nowrap;
  }

  .mob-tab.active {
    color: var(--terra);
    border-bottom-color: var(--terra);
  }

  .tab-content {
    padding-bottom: 0 !important;
    padding-top: 46px !important;
  }

  .navbar {
    min-height: 44px !important;
    justify-content: center !important;
  }
}

/* --- Map Info Panel Scroll Hint -------------------------- */
.map-scroll-hint { display: none; }

@media (max-width: 768px) {
  .map-scroll-hint {
    display: flex;
    justify-content: center;
    padding: 8px 0 0;
    animation: hintBounce 1.5s ease-in-out 3;
  }

  .map-scroll-hint .hint-arrow {
    display: flex;
    align-items: center;
    gap: 6px;
    font-size: 12px;
    font-weight: 600;
    color: var(--terra);
    letter-spacing: 0.5px;
    text-transform: uppercase;
    opacity: 0.8;
  }

  @keyframes hintBounce {
    0%, 100% { transform: translateY(0); }
    50% { transform: translateY(6px); }
  }
}

/* --- Gallery Badges -------------------------------------- */
.submission-count-badge {
  display: inline-flex;
  align-items: center;
  gap: 5px;
  background: var(--glass-bg);
  backdrop-filter: blur(10px);
  -webkit-backdrop-filter: blur(10px);
  border: 1px solid var(--glass-border-subtle);
  padding: 4px 12px;
  border-radius: 20px;
  font-size: 11px;
  font-weight: 700;
  color: var(--sage-light);
}

.museum-photo-badge {
  position: absolute;
  bottom: 14px;
  left: 14px;
  z-index: 2;
  display: inline-flex;
  align-items: center;
  gap: 5px;
  background: rgba(15, 26, 20, 0.55);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  border: 1px solid var(--amber-glow);
  color: var(--amber);
  font-size: 11px;
  font-weight: 700;
  padding: 6px 14px;
  border-radius: 20px;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  letter-spacing: 0.3px;
}

.museum-photo-badge:hover {
  background: rgba(226, 185, 76, 0.2);
  border-color: var(--amber);
  transform: translateY(-2px);
  box-shadow: 0 4px 16px var(--amber-glow);
}

.private-collection-badge {
  position: absolute;
  bottom: 14px;
  left: 14px;
  z-index: 2;
  display: inline-flex;
  align-items: center;
  gap: 5px;
  background: rgba(15, 26, 20, 0.55);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  border: 1px solid var(--glass-border-subtle);
  color: var(--text-muted);
  font-size: 11px;
  font-weight: 600;
  padding: 6px 14px;
  border-radius: 20px;
  letter-spacing: 0.3px;
  pointer-events: none;
}

body.light-mode .private-collection-badge {
  background: rgba(255, 255, 255, 0.75);
  border-color: rgba(0, 0, 0, 0.08);
  color: rgba(45, 45, 45, 0.5);
}

.map-info-private-notice {
  display: flex;
  align-items: center;
  gap: 8px;
  background: var(--glass-bg-light);
  border: 1px solid var(--glass-border-subtle);
  border-radius: var(--radius-sm);
  padding: 12px 16px;
  margin-bottom: 20px;
  font-size: 13px;
  color: var(--text-muted);
  font-style: italic;
}

/* --- Map Filter Dropdowns ------------------------- */
.artist-filter-wrap select {
  padding: 7px 14px;
  font-size: 13px;
  font-weight: 600;
  border-radius: 50px;
  background: var(--glass-bg-light);
  border: 1px solid var(--glass-border-subtle);
  color: var(--text-secondary);
  font-family: 'DM Sans', sans-serif;
  cursor: pointer;
  transition: all 0.3s var(--ease);
  appearance: none !important;
  -webkit-appearance: none !important;
  outline: none;
}

.artist-filter-wrap select:hover {
  background: var(--glass-bg);
  border-color: var(--glass-border);
  color: var(--text-primary);
}

.artist-filter-wrap select:focus {
  border-color: var(--terra);
  box-shadow: 0 0 12px var(--terra-glow);
  color: var(--text-primary);
}

.artist-filter-wrap select option {
  background: var(--surface-dark-mid);
  color: var(--text-primary);
}

body.light-mode .artist-filter-wrap select {
  background: var(--glass-bg-light);
  color: var(--text-secondary);
  border-color: var(--glass-border-subtle);
}

body.light-mode .artist-filter-wrap select option {
  background: var(--surface-dark-mid);
  color: var(--text-primary);
}
"


# ===========================================================================
# UI SECTION
# ===========================================================================

ui <- page_navbar(
  
  useShinyjs(),
  title = NULL,
  id = "main_tabs",
  
  theme = bs_theme(
    version = 5,
    bg = "#0f1a14",
    fg = "#FFFFFF",
    primary = "#E8976B",
    secondary = "#7FA88A",
    success = "#7FA88A",
    info = "#E2B94C",
    base_font = font_google("DM Sans"),
    heading_font = font_google("DM Serif Display")
  ),
  
  header = tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=DM+Serif+Display&display=swap", rel = "stylesheet"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1, viewport-fit=cover"),
    tags$style(HTML(app_css)),
    tags$script(HTML("document.addEventListener('DOMContentLoaded', function() { document.body.classList.add('light-mode'); });"))
  ),
  
  
  # -- TAB 1: GALLERY ------------------------------------------------------
  nav_panel(
    title = "Gallery",
    icon = icon("images"),
    
    tags$div(class = "section-header",
             tags$h2("The Collection"),
             tags$div(class = "accent-line")
    ),
    
    tags$div(class = "tab-search-wrap",
             tags$input(type = "text", id = "gallery_search", class = "tab-search-input",
                        size = "36",
                        placeholder = "Search by title or artist...",
                        oninput = "filterGalleryCards(this.value)"),
             tags$span(class = "tab-search-icon", HTML("&#128269;"))
    ),
    
    tags$div(class = "gallery-wrap",
             tags$div(id = "paintings-container", class = "paintings-grid",
                      uiOutput("painting_cards")
             )
    )
  ),
  
  
  # -- TAB 2: MAP ----------------------------------------------------------
  nav_panel(
    title = "Map",
    icon = icon("map-location-dot"),
    
    tags$div(class = "section-header",
             tags$h2("Explore Locations"),
             tags$p("Click a marker for details."),
             tags$div(class = "accent-line")
    ),
    
    tags$div(class = "map-filter-bar",
             tags$div(class = "map-filter-btn active", id = "map_filter_all",
                      onclick = "Shiny.setInputValue('set_map_filter', 'all');",
                      "All"
             ),
             tags$div(class = "map-filter-btn", id = "map_filter_submissions",
                      onclick = "Shiny.setInputValue('set_map_filter', 'submissions');",
                      tags$span(class = "legend-dot blue"),
                      "Current Photos"
             ),
             tags$div(class = "map-filter-btn", id = "map_filter_museums",
                      onclick = "Shiny.setInputValue('set_map_filter', 'museums');",
                      tags$span(class = "legend-dot", style = "background: #DC3545;"),
                      "Museums"
             ),
             tags$div(class = "map-filter-btn locate-me-btn", id = "locate_me_btn",
                      onclick = "startGeolocation();",
                      HTML("&#9678; My Location")
             ),
             tags$div(class = "artist-filter-wrap",
                      selectInput("map_artist_filter", NULL,
                                  choices = c("All Painters" = "", sort(unique(paintings_data$artist))),
                                  selected = "",
                                  width = "180px",
                                  selectize = FALSE)
             ),
             tags$div(class = "artist-filter-wrap",
                      selectInput("map_state_filter", NULL,
                                  choices = c("All States" = "", sort(unique(state.name))),
                                  selected = "",
                                  width = "160px",
                                  selectize = FALSE)
             )
    ),
    
    tags$div(class = "map-split-layout",
             tags$div(class = "map-container",
                      leafletOutput("main_map", height = "100%")
             ),
             tags$div(id = "map-scroll-hint", class = "map-scroll-hint", style = "display: none;",
                      tags$span(class = "hint-arrow", HTML("&#9660; Tap to see details below"))
             ),
             tags$div(id = "map-info-panel-el", class = "map-info-panel",
                      uiOutput("map_info_content")
             )
    )
  ),
  
  
  # -- TAB 3: CONTRIBUTE ----------------------------------------------------
  nav_panel(
    title = "Contribute",
    icon = icon("camera"),
    
    tags$div(class = "section-header",
             tags$h2("Contribute"),
             tags$p("Share landscape photos, museum visits, or upload historical paintings to expand the collection."),
             tags$div(class = "accent-line")
    ),
    
    tags$div(id = "contribute-landing", class = "contribute-landing",
             tags$div(class = "contribute-type-card",
                      onclick = "selectContributeType('landscape')",
                      tags$div(class = "contribute-type-title", "Landscape Photo"),
                      tags$div(class = "contribute-type-desc",
                               "Visit a painting's real-world location and photograph what it looks like today."
                      ),
                      tags$div(class = "contribute-type-cta", HTML("Get Started &rarr;"))
             ),
             tags$div(class = "contribute-type-card",
                      onclick = "selectContributeType('user_painting')",
                      tags$div(class = "contribute-type-title", "Upload a Painting"),
                      tags$div(class = "contribute-type-desc",
                               "Add a new historical landscape painting to grow the collection beyond Bierstadt."
                      ),
                      tags$div(class = "contribute-type-cta", HTML("Get Started &rarr;"))
             )
    ),
    
    tags$div(id = "contribute-form-wrap", class = "form-wrap", style = "display: none;",
             tags$div(class = "form-card",
                      tags$div(class = "contribute-back-btn",
                               onclick = "showContributeLanding()",
                               HTML("&larr; Back")
                      ),
                      tags$div(id = "contribute-form-type-label",
                               style = "font-family: 'DM Serif Display', Georgia, serif; font-size: 22px; color: var(--text-primary); margin-bottom: 24px;"
                      ),
                      uiOutput("submit_message"),
                      tags$div(style = "display: none;",
                               radioButtons("submit_type", NULL,
                                            choices = c("landscape" = "landscape", "museum_photo" = "museum_photo", "user_painting" = "user_painting"),
                                            selected = "landscape"
                               )
                      ),
                      tags$div(class = "form-group",
                               textInput("submit_name", "Your Name (optional)", placeholder = "Jane Doe")
                      ),
                      tags$div(class = "form-group",
                               textInput("submit_email", "Email (optional)", placeholder = "jane@university.edu")
                      ),
                      conditionalPanel(
                        condition = "input.submit_type !== 'user_painting'",
                        tags$div(class = "form-group",
                                 selectInput("submit_painting", "Which painting?",
                                             choices = c("Select a painting..." = "", setNames(paintings_data$id, paintings_data$title)))
                        )
                      ),
                      conditionalPanel(
                        condition = "input.submit_type === 'user_painting'",
                        tags$div(class = "form-group",
                                 textInput("submit_painting_title", "Painting Title", placeholder = "Storm in the Rocky Mountains")
                        ),
                        tags$div(class = "form-group",
                                 textInput("submit_artist_name", "Artist Name", placeholder = "Albert Bierstadt")
                        ),
                        tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
                                 tags$div(class = "form-group",
                                          textInput("submit_painting_year", "Year (optional)", placeholder = "1866")
                                 ),
                                 tags$div(class = "form-group",
                                          textInput("submit_painting_context", "Brief Description (optional)", placeholder = "Painted during his trip to the Rockies")
                                 )
                        ),
                        tags$div(class = "form-group",
                                 selectInput("submit_state", "State",
                                             choices = c("Select a state..." = "", sort(state.name), "Unknown" = "Unknown"),
                                             selected = "")
                        ),
                        tags$div(class = "form-group",
                                 textInput("submit_region", "Region / Landmark (optional)",
                                           placeholder = "Sierra Nevada, Yellowstone, Hudson River Valley")
                        ),
                        tags$div(class = "form-group",
                                 textInput("submit_location_notes", "Location Notes (optional)",
                                           placeholder = "Believed to be near...")
                        ),
                        tags$div(class = "form-group", style = "margin-top: 8px;",
                                 tags$label(style = "display: flex; align-items: center; gap: 10px; cursor: pointer; text-transform: none; letter-spacing: 0;",
                                            tags$input(type = "checkbox", id = "include_museum_info", style = "width: 18px; height: 18px; cursor: pointer;",
                                                       onchange = "document.getElementById('museum-info-fields').style.display = this.checked ? '' : 'none';"),
                                            tags$span(style = "color: var(--text-primary); font-weight: 600;",
                                                      HTML("Also add museum info for this painting?"))
                                 )
                        ),
                        tags$div(id = "museum-info-fields", style = "display: none; padding: 16px; background: var(--glass-bg-light); border-radius: var(--radius-sm); border: 1px solid var(--glass-border-subtle); margin-top: 8px;",
                                 tags$div(class = "form-group",
                                          textInput("submit_museum_name", "Museum / Collection Name",
                                                    placeholder = "Metropolitan Museum of Art")
                                 ),
                                 tags$div(class = "form-group",
                                          tags$div(style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 8px;",
                                                   tags$label("Museum GPS Coordinates", style = "margin-bottom: 0;"),
                                                   tags$button(
                                                     id = "use_museum_location_btn",
                                                     class = "map-filter-btn",
                                                     style = "padding: 6px 16px; font-size: 12px;",
                                                     onclick = "getMuseumLocation();",
                                                     HTML("&#9678; Use My Location")
                                                   )
                                          ),
                                          tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
                                                   tags$div(class = "form-group", style = "margin-bottom: 0;",
                                                            numericInput("submit_museum_lat", NULL, value = NA, step = 0.0001)
                                                   ),
                                                   tags$div(class = "form-group", style = "margin-bottom: 0;",
                                                            numericInput("submit_museum_lng", NULL, value = NA, step = 0.0001)
                                                   )
                                          ),
                                          tags$div(id = "museum_location_status", style = "font-size: 12px; color: var(--text-muted); margin-top: 6px;")
                                 )
                        )
                      ),
                      tags$div(class = "form-group",
                               tags$label("Upload Your Photo"),
                               tags$div(class = "upload-zone",
                                        tags$div(class = "upload-icon", HTML("&#128247;")),
                                        tags$p(style = "color: var(--text-secondary); font-size: 14px;", "Take a photo or choose from library"),
                                        tags$p(style = "color: var(--text-muted); font-size: 12px; margin-top: 4px;", "Max file size: 5MB (JPEG or PNG)"),
                                        fileInput("submit_photo", NULL, accept = c("image/png", "image/jpeg", "image/jpg"))
                               ),
                               tags$script(HTML("
                                 $(document).on('shiny:connected', function() {
                                   var fi = document.querySelector('#submit_photo');
                                   if (fi) { fi.setAttribute('capture', 'environment'); }
                                 });
                               "))
                      ),
                      conditionalPanel(
                        condition = "input.submit_type === 'landscape'",
                        tags$div(class = "form-group",
                                 tags$div(style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 8px;",
                                          tags$label("GPS Coordinates", style = "margin-bottom: 0;"),
                                          tags$button(
                                            id = "use_my_location_btn",
                                            class = "map-filter-btn",
                                            style = "padding: 6px 16px; font-size: 12px;",
                                            onclick = "getFormLocation();",
                                            HTML("&#9678; Use My Location")
                                          )
                                 ),
                                 tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
                                          tags$div(class = "form-group", style = "margin-bottom: 0;",
                                                   numericInput("submit_latitude", NULL, value = NA, step = 0.0001)
                                          ),
                                          tags$div(class = "form-group", style = "margin-bottom: 0;",
                                                   numericInput("submit_longitude", NULL, value = NA, step = 0.0001)
                                          )
                                 ),
                                 tags$div(id = "location_status", style = "font-size: 12px; color: var(--text-muted); margin-top: 6px;")
                        )
                      ),
                      tags$div(class = "form-group",
                               textAreaInput("submit_observations", "Observations (optional)", rows = 3,
                                             placeholder = "What did you notice about how the landscape has changed?")
                      ),
                      actionButton("submit_button", HTML("Submit &rarr;"), class = "btn-submit")
             )
    )
  ),
  
  
  # -- TAB 4: COMPARE ------------------------------------------------------
  nav_panel(
    title = "Compare",
    icon = icon("arrows-left-right"),
    
    tags$div(class = "section-header",
             tags$h2("Past vs Present"),
             tags$p("See how these landscapes have transformed over 150 years. Click to open side-by-side."),
             tags$div(class = "accent-line")
    ),
    
    tags$div(class = "tab-search-wrap",
             tags$input(type = "text", id = "compare_search", class = "tab-search-input",
                        size = "36",
                        placeholder = "Search by title or artist...",
                        oninput = "filterCompareCards(this.value)"),
             tags$span(class = "tab-search-icon", HTML("&#128269;"))
    ),
    
    tags$div(class = "comparison-wrap",
             uiOutput("comparison_gallery")
    )
  ),
  
  
  nav_spacer(),
  
  
  # -- TAB 5: ADMIN LOGIN ---------------------------------------------------
  nav_panel(
    title = "Admin Login",
    icon = icon("right-to-bracket"),
    
    tags$div(class = "section-header",
             tags$h2("Admin Login"),
             tags$p("Sign in to review and manage community submissions."),
             tags$div(class = "accent-line")
    ),
    
    tags$div(class = "admin-wrap",
             conditionalPanel(
               condition = "output.admin_authenticated == false",
               tags$div(class = "admin-login-card",
                        tags$h3("Sign In"),
                        tags$p("Enter admin credentials to manage submissions."),
                        passwordInput("admin_password", NULL, placeholder = "Password"),
                        actionButton("admin_login", "Sign In", class = "btn-submit")
               )
             ),
             conditionalPanel(
               condition = "output.admin_authenticated == true",
               tags$div(class = "admin-toolbar",
                        actionButton("refresh_admin", "Refresh", class = "btn btn-refresh"),
                        actionButton("approve_submission", "Approve Selected", class = "btn btn-approve"),
                        actionButton("reject_submission", "Reject Selected", class = "btn btn-reject"),
                        actionButton("delete_submission", "Delete Selected", class = "btn btn-reject",
                                     style = "background: #DC3545; border-color: #DC3545; color: white;"),
                        tags$div(class = "artist-filter-wrap",
                                 selectInput("admin_type_filter", NULL,
                                             choices = c("All Types" = "", "Landscape Photos" = "landscape", "Museum Photos" = "museum_photo", "User Paintings" = "user_painting", "Museum Info Updates" = "add_museum"),
                                             selected = "", width = "180px")
                        ),
                        tags$div(class = "artist-filter-wrap",
                                 selectInput("admin_status_filter", NULL,
                                             choices = c("All Statuses" = "", "Pending" = "Pending", "Approved" = "Approved", "Rejected" = "Rejected"),
                                             selected = "", width = "160px")
                        )
               ),
               DTOutput("admin_table")
             )
    )
  ),
  
  
  # -- FOOTER: LIGHTBOXES AND JAVASCRIPT ------------------------------------
  footer = tagList(
    
    # Mobile top tab bar
    tags$div(id = "mobile-tab-bar", class = "mobile-tab-bar",
             tags$button(class = "mob-tab active", `data-tab` = "Gallery", onclick = "mobileTabSwitch('Gallery')", "Gallery"),
             tags$button(class = "mob-tab", `data-tab` = "Map", onclick = "mobileTabSwitch('Map')", "Map"),
             tags$button(class = "mob-tab", `data-tab` = "Contribute", onclick = "mobileTabSwitch('Contribute')", "Contribute"),
             tags$button(class = "mob-tab", `data-tab` = "Compare", onclick = "mobileTabSwitch('Compare')", "Compare"),
             tags$button(class = "mob-tab", `data-tab` = "Admin Login", onclick = "mobileTabSwitch('Admin Login')", "Admin")
    ),
    
    # Splash screen overlay
    tags$div(id = "splash-overlay", class = "splash-overlay",
             onclick = "dismissSplash()",
             tags$div(class = "splash-orb splash-orb-1"),
             tags$div(class = "splash-orb splash-orb-2"),
             tags$div(class = "splash-orb splash-orb-3"),
             tags$div(class = "splash-orb splash-orb-4"),
             tags$div(class = "splash-inner",
                      tags$h1(class = "splash-title", HTML("Landscape<br>Through <span>Time</span>")),
                      tags$p(class = "splash-tagline", "Explore where painters set up their easels across America."),
                      tags$div(class = "splash-hint", "Click anywhere to enter")
             )
    ),
    
    # Comparison lightbox
    tags$div(id = "comparison-lightbox",
             tags$div(class = "lightbox-close", onclick = "closeComparisonLightbox()",
                      style = "position: fixed; top: 24px; right: 24px; z-index: 10002;", HTML("&times;")),
             tags$div(id = "comp-sidebyside", class = "comparison-container",
                      tags$div(class = "comparison-side",
                               tags$div(class = "comparison-label", "Historical"),
                               tags$img(id = "comp-historical", src = "", draggable = "false")
                      ),
                      tags$div(class = "comparison-side",
                               tags$div(class = "comparison-label", "Present Day"),
                               tags$img(id = "comp-modern", src = "", draggable = "false")
                      )
             )
    ),
    
    # Painting detail lightbox
    tags$div(id = "painting-detail-lightbox",
             style = "display:none; position:fixed; inset:0; background:var(--surface-dark); z-index:10001; overflow-y:auto; padding:60px 24px;",
             tags$div(class = "lightbox-close", onclick = "closePaintingDetail()",
                      style = "position:fixed; top:24px; right:24px; z-index:10002;", HTML("&times;")),
             tags$div(id = "painting-detail-content", style = "max-width:800px; margin:0 auto;")
    ),
    
    # Add Museum info modal
    tags$div(id = "add-museum-modal",
             style = "display:none; position:fixed; inset:0; background:rgba(8,12,10,0.85); z-index:10003; align-items:center; justify-content:center; padding:24px;",
             tags$div(style = "background:var(--surface-dark-mid); border:1px solid var(--glass-border); border-radius:var(--radius-lg); padding:32px; max-width:500px; width:100%; box-shadow:var(--shadow-glass-lg); max-height:90vh; overflow-y:auto;",
                      tags$div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:20px;",
                               tags$h3(id = "add-museum-title", style = "font-family:'DM Serif Display',Georgia,serif; font-size:24px; color:var(--text-primary); margin:0;", "Add Museum Info"),
                               tags$div(class = "lightbox-close", onclick = "closeAddMuseumModal()",
                                        style = "position:relative; top:auto; right:auto; width:36px; height:36px; font-size:22px;", HTML("&times;"))
                      ),
                      tags$p(id = "add-museum-subtitle", style = "color:var(--text-secondary); font-size:14px; margin-bottom:20px; line-height:1.5;",
                             "Help others find this painting by adding the museum or collection it's held at."),
                      tags$div(class = "form-group",
                               tags$label(style = "color:var(--text-secondary); font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:0.5px; margin-bottom:6px; display:block;", "Museum / Collection Name"),
                               tags$input(type = "text", id = "add_museum_name_input", class = "form-control",
                                          placeholder = "Metropolitan Museum of Art",
                                          style = "width:100%; padding:12px 14px; background:rgba(255,255,255,0.06); border:1px solid var(--glass-border-subtle); border-radius:var(--radius-sm); color:var(--text-primary); font-size:14px;")
                      ),
                      tags$div(class = "form-group", style = "margin-top:16px;",
                               tags$div(style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:6px;",
                                        tags$label(style = "color:var(--text-secondary); font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:0.5px; margin:0;", "GPS Coordinates"),
                                        tags$button(onclick = "getAddMuseumLocation();", class = "map-filter-btn",
                                                    style = "padding:6px 14px; font-size:11px;", HTML("&#9678; Use My Location"))
                               ),
                               tags$div(style = "display:grid; grid-template-columns:1fr 1fr; gap:10px;",
                                        tags$input(type = "number", id = "add_museum_lat_input", class = "form-control", step = "0.0001", placeholder = "Latitude",
                                                   style = "width:100%; padding:12px 14px; background:rgba(255,255,255,0.06); border:1px solid var(--glass-border-subtle); border-radius:var(--radius-sm); color:var(--text-primary); font-size:14px;"),
                                        tags$input(type = "number", id = "add_museum_lng_input", class = "form-control", step = "0.0001", placeholder = "Longitude",
                                                   style = "width:100%; padding:12px 14px; background:rgba(255,255,255,0.06); border:1px solid var(--glass-border-subtle); border-radius:var(--radius-sm); color:var(--text-primary); font-size:14px;")
                               ),
                               tags$div(id = "add_museum_loc_status", style = "font-size:11px; color:var(--text-muted); margin-top:6px;")
                      ),
                      tags$div(class = "form-group", style = "margin-top:16px;",
                               tags$label(style = "color:var(--text-secondary); font-size:12px; font-weight:600; text-transform:uppercase; letter-spacing:0.5px; margin-bottom:6px; display:block;", "Your Name (optional)"),
                               tags$input(type = "text", id = "add_museum_name_user_input", class = "form-control", placeholder = "Jane Doe",
                                          style = "width:100%; padding:12px 14px; background:rgba(255,255,255,0.06); border:1px solid var(--glass-border-subtle); border-radius:var(--radius-sm); color:var(--text-primary); font-size:14px;")
                      ),
                      tags$div(id = "add_museum_error", style = "color:var(--terra); font-size:13px; margin-top:12px; display:none;"),
                      tags$button(onclick = "submitAddMuseum();", class = "btn-submit",
                                  style = "width:100%; margin-top:20px;", HTML("Submit for Review &rarr;"))
             )
    ),
    
    # Museum photo lightbox
    tags$div(id = "museum-photo-lightbox",
             style = "display:none; position:fixed; inset:0; background:rgba(8,12,10,0.97); z-index:10001; align-items:center; justify-content:center; flex-direction:column; padding:60px 32px;",
             tags$div(class = "lightbox-close", onclick = "closeMuseumLightbox()",
                      style = "position:fixed; top:24px; right:24px; z-index:10002;", HTML("&times;")),
             tags$div(id = "museum-lb-prev", onclick = "museumLightboxNav(-1)",
                      style = "position:fixed; left:24px; top:50%; transform:translateY(-50%); z-index:10002; cursor:pointer; font-size:36px; color:var(--text-secondary); background:var(--glass-bg); backdrop-filter:blur(12px); -webkit-backdrop-filter:blur(12px); border:1px solid var(--glass-border-subtle); width:48px; height:48px; border-radius:50%; display:flex; align-items:center; justify-content:center; transition:all 0.3s;",
                      HTML("&#8249;")),
             tags$div(id = "museum-lb-next", onclick = "museumLightboxNav(1)",
                      style = "position:fixed; right:80px; top:50%; transform:translateY(-50%); z-index:10002; cursor:pointer; font-size:36px; color:var(--text-secondary); background:var(--glass-bg); backdrop-filter:blur(12px); -webkit-backdrop-filter:blur(12px); border:1px solid var(--glass-border-subtle); width:48px; height:48px; border-radius:50%; display:flex; align-items:center; justify-content:center; transition:all 0.3s;",
                      HTML("&#8250;")),
             tags$div(style = "text-align:center; max-width:900px; width:100%;",
                      tags$div(id = "museum-lb-label",
                               style = "background:var(--glass-bg-strong); backdrop-filter:blur(12px); -webkit-backdrop-filter:blur(12px); padding:8px 18px; border-radius:20px; font-size:13px; font-weight:700; color:var(--amber); border:1px solid var(--glass-border-subtle); display:inline-block; margin-bottom:20px;",
                               "Museum Photo"
                      ),
                      tags$img(id = "museum-lb-img", src = "", style = "max-width:100%; max-height:65vh; border-radius:var(--radius-md); box-shadow:var(--shadow-glass-lg); object-fit:contain;"),
                      tags$div(id = "museum-lb-info", style = "margin-top:16px; color:var(--text-secondary); font-size:14px;"),
                      tags$div(id = "museum-lb-counter", style = "margin-top:8px; color:var(--text-muted); font-size:12px; font-weight:600; letter-spacing:1px;")
             )
    ),
    
    # JavaScript
    tags$script(HTML(paste0("

      // -- SPLASH SCREEN ---------------------------------------------------
      window.dismissSplash = function() {
        var splash = document.getElementById('splash-overlay');
        if (!splash) return;
        splash.classList.add('fade-out');
        setTimeout(function() { splash.remove(); }, 800);
        // Navigate to Gallery as default landing tab
        setTimeout(function() {
          var tabLink = document.querySelector('a.nav-link[data-value=\"Gallery\"]');
          if (tabLink) tabLink.click();
        }, 200);
      };

      window.splashNavigate = function(tab) {
        dismissSplash();
        setTimeout(function() {
          var tabLink = document.querySelector('a.nav-link[data-value=\"' + tab + '\"]');
          if (tabLink) tabLink.click();
        }, 200);
      };

      // -- AUTO DARK MODE ON ADMIN TAB -----------------------------------
      function applyAdminTheme(tabVal) {
        if (tabVal === 'Admin Login') {
          document.body.classList.remove('light-mode');
        } else {
          if (!document.body.classList.contains('light-mode')) {
            document.body.classList.add('light-mode');
          }
        }
      }

      // Server-driven theme toggle (most reliable)
      Shiny.addCustomMessageHandler('setTheme', function(tab) {
        applyAdminTheme(tab);
      });

      // Fallback: also listen to Bootstrap tab events
      $(document).on('shown.bs.tab', function(e) {
        if (!e.target) return;
        var tabVal = e.target.getAttribute('data-value');
        if (tabVal) applyAdminTheme(tabVal);
      });

      var paintingsData = ", jsonlite::toJSON(paintings_data, auto_unbox = TRUE), ";

      Shiny.addCustomMessageHandler('updatePaintingsData', function(data) {
        paintingsData = JSON.parse(data);
      });

      Shiny.addCustomMessageHandler('showPaintingDetail', function(data) {
        var p = JSON.parse(data);
        if (p) renderPaintingDetail(p);
      });

      // -- COMPARISON LIGHTBOX ---------------------------------------------
      window.openComparisonLightbox = function(historicalUrl, modernUrl) {
        document.getElementById('comp-historical').src = historicalUrl;
        document.getElementById('comp-modern').src = modernUrl;
        document.getElementById('comparison-lightbox').classList.add('active');
        document.body.style.overflow = 'hidden';

        var sides = document.querySelectorAll('#comp-sidebyside .comparison-side img');
        sides.forEach(function(img) {
          img.addEventListener('wheel', function(e) {
            e.preventDefault();
            var current = parseFloat(img.style.transform.replace('scale(', '').replace(')', '') || 1);
            var delta = e.deltaY * -0.01;
            var scale = Math.max(1, Math.min(3, current + delta));
            sides.forEach(function(s) { s.style.transform = 'scale(' + scale + ')'; });
          });
        });
      };

      window.closeComparisonLightbox = function() {
        document.getElementById('comparison-lightbox').classList.remove('active');
        document.body.style.overflow = '';
        document.querySelectorAll('#comp-sidebyside .comparison-side img').forEach(function(img) {
          img.style.transform = '';
        });
      };

      // -- MUSEUM PHOTO LIGHTBOX -------------------------------------------
      var museumPhotos = [];
      var museumPhotoIdx = 0;
      var museumTitle = '';

      function museumLightboxUpdate() {
        if (museumPhotos.length === 0) return;
        var photo = museumPhotos[museumPhotoIdx];
        document.getElementById('museum-lb-img').src = photo.url;
        document.getElementById('museum-lb-info').innerHTML = '<strong>' + museumTitle + '</strong><br>Photographed by ' + photo.name;
        document.getElementById('museum-lb-counter').textContent = (museumPhotoIdx + 1) + ' / ' + museumPhotos.length;
        document.getElementById('museum-lb-prev').style.display = museumPhotos.length > 1 ? 'flex' : 'none';
        document.getElementById('museum-lb-next').style.display = museumPhotos.length > 1 ? 'flex' : 'none';
        document.getElementById('museum-lb-counter').style.display = museumPhotos.length > 1 ? 'block' : 'none';
      }

      window.openMuseumLightbox = function(title, photos) {
        museumTitle = title;
        museumPhotos = photos;
        museumPhotoIdx = 0;
        museumLightboxUpdate();
        var lb = document.getElementById('museum-photo-lightbox');
        lb.style.display = 'flex';
        document.body.style.overflow = 'hidden';
      };

      window.museumLightboxNav = function(dir) {
        if (museumPhotos.length <= 1) return;
        museumPhotoIdx = (museumPhotoIdx + dir + museumPhotos.length) % museumPhotos.length;
        museumLightboxUpdate();
      };

      window.closeMuseumLightbox = function() {
        document.getElementById('museum-photo-lightbox').style.display = 'none';
        document.body.style.overflow = '';
        museumPhotos = [];
        museumPhotoIdx = 0;
      };

      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') {
          closeComparisonLightbox();
          closeMuseumLightbox();
          closePaintingDetail();
        }
        if (document.getElementById('museum-photo-lightbox').style.display === 'flex') {
          if (e.key === 'ArrowLeft') museumLightboxNav(-1);
          if (e.key === 'ArrowRight') museumLightboxNav(1);
        }
      });

      // -- PAINTING DETAIL LIGHTBOX ----------------------------------------
      window.openPaintingDetail = function(id) {
        var p = null;
        for (var i = 0; i < paintingsData.length; i++) {
          if (paintingsData[i].id === id) { p = paintingsData[i]; break; }
        }
        if (!p) {
          Shiny.setInputValue('request_painting_detail', {id: id, t: Date.now()});
          return;
        }
        renderPaintingDetail(p);
      };

      window.renderPaintingDetail = function(p) {

        var state = p.state || '';
        var region = p.region || '';
        var locationNotes = p.location_notes || '';
        var locParts = [];
        if (state) locParts.push(state);
        if (region) locParts.push(region);
        var locText = locParts.join(' — ');

        var isDiscovered = (p.approved_count || 0) > 0;
        var hasMuseum = p.has_museum === true;
        var museumName = p.museum_name || '';
        var isPrivate = museumName && /private collection/i.test(museumName);

        // Status chip: discovered (green) or undiscovered (amber)
        var statusChip = '<div style=\"display:inline-flex; align-items:center; gap:6px; padding:6px 14px; border-radius:20px; font-size:12px; font-weight:700; letter-spacing:0.3px; border:1px solid; ' +
          (isDiscovered
            ? 'background:rgba(127,168,138,0.15); border-color:var(--sage); color:var(--sage-light);'
            : 'background:rgba(226,185,76,0.12); border-color:rgba(226,185,76,0.6); color:var(--amber);');

        var html = '<div style=\"text-align:center; margin-bottom:20px;\">' +
          '<img src=\"' + p.image_url + '\" alt=\"' + p.title + '\" style=\"max-width:100%; max-height:45vh; border-radius:var(--radius-md); box-shadow:var(--shadow-glass-lg); object-fit:contain;\">' +
          '</div>' +
          '<h2 style=\"font-family:DM Serif Display,Georgia,serif; font-size:30px; color:var(--text-primary); margin-bottom:4px; text-align:center;\">' + p.title + '</h2>' +
          '<div style=\"color:var(--terra-light); font-size:13px; font-weight:600; text-transform:uppercase; letter-spacing:0.8px; margin-bottom:18px; text-align:center;\">' + p.artist + (p.year ? ' &middot; ' + p.year : '') + '</div>' +
          '<div style=\"text-align:center; margin-bottom:22px;\">' + statusChip + '</div>';

        // Location details (compact)
        if (locText || locationNotes) {
          html += '<div style=\"background:var(--glass-bg-light); border:1px solid var(--glass-border-subtle); padding:14px 18px; border-radius:var(--radius-sm); margin-bottom:18px;\">';
          if (locText) {
            html += '<div style=\"font-size:11px; font-weight:700; text-transform:uppercase; letter-spacing:1px; color:var(--text-muted); margin-bottom:4px;\">Location</div>' +
                    '<div style=\"color:var(--text-primary); font-size:14px; margin-bottom:' + (locationNotes ? '8px' : '0') + ';\">&#128205; ' + locText + '</div>';
          }
          if (locationNotes) {
            html += '<div style=\"font-size:13px; color:var(--text-secondary); font-style:italic;\">' + locationNotes + '</div>';
          }
          html += '</div>';
        }

        // Museum info (compact)
        if (hasMuseum && !isPrivate) {
          var hasMuseumCoords = p.museum_latitude != null && !isNaN(parseFloat(p.museum_latitude)) &&
                                p.museum_longitude != null && !isNaN(parseFloat(p.museum_longitude));
          html += '<div style=\"background:var(--glass-bg-light); border:1px solid var(--glass-border-subtle); padding:14px 18px; border-radius:var(--radius-sm); margin-bottom:18px;\">' +
            '<div style=\"font-size:11px; font-weight:700; text-transform:uppercase; letter-spacing:1px; color:var(--text-muted); margin-bottom:4px;\">Currently Held At</div>' +
            '<div style=\"color:var(--text-primary); font-size:14px; margin-bottom:' + (hasMuseumCoords ? '10px' : '0') + ';\">&#127963; ' + museumName + '</div>';
          if (hasMuseumCoords) {
            html += '<div class=\"map-info-cta museum\" style=\"font-size:11px; padding:7px 14px; display:inline-flex;\" onclick=\"closePaintingDetail(); Shiny.setInputValue(&#39;go_to_museum&#39;, {id: ' + p.id + ', t: Date.now()});\">View on Map &rarr;</div>';
          }
          html += '</div>';
        } else if (isPrivate) {
          html += '<div style=\"background:var(--glass-bg-light); border:1px solid var(--glass-border-subtle); padding:14px 18px; border-radius:var(--radius-sm); margin-bottom:18px; color:var(--text-muted); font-size:13px; font-style:italic;\">' +
            '&#128274; Held in a private collection' +
            '</div>';
        }

        // Context (if present)
        if (p.context) {
          html += '<p style=\"color:var(--text-secondary); font-size:14px; line-height:1.7; margin-bottom:22px;\">' + p.context + '</p>';
        }

        // Action buttons — conditional based on state
        html += '<div style=\"display:flex; gap:10px; flex-wrap:wrap; justify-content:center;\">';

        if (isDiscovered) {
          html += '<div class=\"map-info-cta\" onclick=\"closePaintingDetail(); Shiny.setInputValue(&#39;go_compare_painting&#39;, {id: ' + p.id + ', t: Date.now()});\">View Comparisons &rarr;</div>';
          html += '<div class=\"map-info-cta travel\" onclick=\"closePaintingDetail(); Shiny.setInputValue(&#39;contribute_for_painting&#39;, {id: ' + p.id + ', t: Date.now()});\">&#43; Add Contemporary Photo</div>';
        } else {
          html += '<div class=\"map-info-cta\" onclick=\"closePaintingDetail(); Shiny.setInputValue(&#39;contribute_for_painting&#39;, {id: ' + p.id + ', t: Date.now()});\">&#43; Add Contemporary Photo</div>';
        }

        if (!hasMuseum) {
          html += '<div class=\"map-info-cta museum\" onclick=\"openAddMuseumModal(' + p.id + ', &#39;' + (p.title || '').replace(/\\x27/g, \"\\\\\\x27\") + '&#39;);\">&#127963; Add Museum Info</div>';
        }

        html += '</div>';

        document.getElementById('painting-detail-content').innerHTML = html;
        document.getElementById('painting-detail-lightbox').style.display = 'block';
        document.body.style.overflow = 'hidden';
      };

      window.closePaintingDetail = function() {
        document.getElementById('painting-detail-lightbox').style.display = 'none';
        document.body.style.overflow = '';
      };

      // -- ADD MUSEUM MODAL ------------------------------------------------
      var addMuseumPid = null;

      window.openAddMuseumModal = function(pid, title) {
        addMuseumPid = pid;
        document.getElementById('add-museum-title').textContent = 'Add Museum Info';
        document.getElementById('add-museum-subtitle').textContent = 'Where is \"' + title + '\" currently held? Your submission will be reviewed before the painting is updated.';
        document.getElementById('add_museum_name_input').value = '';
        document.getElementById('add_museum_lat_input').value = '';
        document.getElementById('add_museum_lng_input').value = '';
        document.getElementById('add_museum_name_user_input').value = '';
        document.getElementById('add_museum_loc_status').textContent = '';
        document.getElementById('add_museum_error').style.display = 'none';
        document.getElementById('add-museum-modal').style.display = 'flex';
        document.body.style.overflow = 'hidden';
      };

      window.closeAddMuseumModal = function() {
        document.getElementById('add-museum-modal').style.display = 'none';
        document.body.style.overflow = '';
        addMuseumPid = null;
      };

      window.getAddMuseumLocation = function() {
        var statusEl = document.getElementById('add_museum_loc_status');
        if (!navigator.geolocation) { statusEl.textContent = 'Geolocation not supported.'; return; }
        statusEl.textContent = 'Getting your location...';
        navigator.geolocation.getCurrentPosition(
          function(pos) {
            document.getElementById('add_museum_lat_input').value = pos.coords.latitude.toFixed(4);
            document.getElementById('add_museum_lng_input').value = pos.coords.longitude.toFixed(4);
            statusEl.textContent = 'Location set.';
          },
          function(err) { statusEl.textContent = err.code === 1 ? 'Access denied.' : 'Unable to get location.'; },
          { enableHighAccuracy: true, timeout: 15000 }
        );
      };

      window.submitAddMuseum = function() {
        var errEl = document.getElementById('add_museum_error');
        errEl.style.display = 'none';

        var name = document.getElementById('add_museum_name_input').value.trim();
        var lat = document.getElementById('add_museum_lat_input').value;
        var lng = document.getElementById('add_museum_lng_input').value;
        var submitter = document.getElementById('add_museum_name_user_input').value.trim();

        if (!name) {
          errEl.textContent = 'Please enter the museum name.';
          errEl.style.display = 'block';
          return;
        }

        Shiny.setInputValue('submit_add_museum', {
          pid: addMuseumPid,
          museum_name: name,
          museum_lat: lat ? parseFloat(lat) : null,
          museum_lng: lng ? parseFloat(lng) : null,
          submitter: submitter,
          t: Date.now()
        });

        closeAddMuseumModal();
        closePaintingDetail();
      };

      // -- 3D CARD TILT EFFECT ---------------------------------------------
      function initTilt() {
        document.querySelectorAll('.painting-card').forEach(function(card) {
          card.addEventListener('mousemove', function(e) {
            var rect = card.getBoundingClientRect();
            var x = e.clientX - rect.left;
            var y = e.clientY - rect.top;
            var rotX = (y - rect.height / 2) / 25;
            var rotY = (rect.width / 2 - x) / 25;
            card.style.transform = 'perspective(800px) rotateX(' + rotX + 'deg) rotateY(' + rotY + 'deg) translateY(-6px)';
          });
          card.addEventListener('mouseleave', function() {
            card.style.transform = '';
          });
        });
      }

      $(document).on('shiny:value', function(e) {
        if (e.name === 'painting_cards') {
          setTimeout(initTilt, 100);
        }
      });
      setTimeout(initTilt, 500);

      // -- GALLERY SEARCH FILTER -------------------------------------------
      window.filterGalleryCards = function(query) {
        var q = (query || '').toLowerCase().trim();
        var cards = document.querySelectorAll('.painting-card');
        cards.forEach(function(card) {
          var title = card.getAttribute('data-title') || '';
          var artist = card.getAttribute('data-artist') || '';
          if (!q || title.indexOf(q) !== -1 || artist.indexOf(q) !== -1) {
            card.style.display = '';
          } else {
            card.style.display = 'none';
          }
        });
      };

      // -- COMPARE SEARCH FILTER -------------------------------------------
      window.filterCompareCards = function(query) {
        var q = (query || '').toLowerCase().trim();
        var cards = document.querySelectorAll('.comparison-thumb');
        cards.forEach(function(card) {
          var painting = card.getAttribute('data-painting') || '';
          var artist = card.getAttribute('data-artist') || '';
          if (!q || painting.indexOf(q) !== -1 || artist.indexOf(q) !== -1) {
            card.style.display = '';
          } else {
            card.style.display = 'none';
          }
        });
      };

      $(document).on('shiny:value', function(e) {
        if (e.name === 'painting_cards') {
          var gi = document.getElementById('gallery_search');
          if (gi && gi.value) filterGalleryCards(gi.value);
        }
        if (e.name === 'comparison_gallery') {
          var ci = document.getElementById('compare_search');
          if (ci && ci.value) filterCompareCards(ci.value);
        }
      });

      // -- TAB SWITCHING FROM R --------------------------------------------
      Shiny.addCustomMessageHandler('switchTab', function(tab) {
        var tabLink = document.querySelector('a.nav-link[data-value=\"' + tab + '\"]');
        if (tabLink) tabLink.click();
      });

      // -- LIVE GEOLOCATION ------------------------------------------------
      var geoWatchId = null;

      window.startGeolocation = function() {
        var btn = document.getElementById('locate_me_btn');

        if (geoWatchId !== null) {
          navigator.geolocation.clearWatch(geoWatchId);
          geoWatchId = null;
          btn.classList.remove('tracking');
          Shiny.setInputValue('user_location', null);
          return;
        }

        if (!navigator.geolocation) {
          alert('Geolocation is not supported by your browser.');
          return;
        }

        btn.classList.add('tracking');

        geoWatchId = navigator.geolocation.watchPosition(
          function(pos) {
            Shiny.setInputValue('user_location', {
              lat: pos.coords.latitude,
              lng: pos.coords.longitude,
              acc: pos.coords.accuracy,
              t: Date.now()
            });
          },
          function(err) {
            console.warn('Geolocation error:', err.message);
            btn.classList.remove('tracking');
            geoWatchId = null;
            if (err.code === 1) {
              alert('Location access was denied. Please allow location access in your browser settings.');
            } else {
              alert('Unable to retrieve your location.');
            }
          },
          { enableHighAccuracy: true, maximumAge: 5000, timeout: 15000 }
        );
      };

      // -- CONTRIBUTE LANDING PAGE -----------------------------------------
      var typeLabels = {
        'landscape': 'Submit a Landscape Photo',
        'museum_photo': 'Submit a Museum Photo',
        'user_painting': 'Upload a Painting'
      };

      window.selectContributeType = function(type) {
        $('input[name=\"submit_type\"][value=\"' + type + '\"]').prop('checked', true).trigger('change');
        document.getElementById('contribute-form-type-label').textContent = typeLabels[type] || 'Submit';
        document.getElementById('contribute-landing').style.display = 'none';
        document.getElementById('contribute-form-wrap').style.display = '';
        window.scrollTo({ top: 0, behavior: 'smooth' });
      };

      window.showContributeLanding = function() {
        document.getElementById('contribute-landing').style.display = '';
        document.getElementById('contribute-form-wrap').style.display = 'none';
      };

      // -- FORM GEOLOCATION (one-shot) -------------------------------------
      window.getFormLocation = function() {
        var statusEl = document.getElementById('location_status');
        var btn = document.getElementById('use_my_location_btn');

        if (!navigator.geolocation) {
          if (statusEl) statusEl.textContent = 'Geolocation not supported by your browser.';
          return;
        }

        if (statusEl) statusEl.textContent = 'Getting your location...';
        btn.style.opacity = '0.5';
        btn.style.pointerEvents = 'none';

        navigator.geolocation.getCurrentPosition(
          function(pos) {
            Shiny.setInputValue('submit_latitude', pos.coords.latitude);
            Shiny.setInputValue('submit_longitude', pos.coords.longitude);
            $('#submit_latitude').val(pos.coords.latitude.toFixed(4));
            $('#submit_longitude').val(pos.coords.longitude.toFixed(4));
            if (statusEl) statusEl.textContent = 'Location set (' + pos.coords.latitude.toFixed(4) + ', ' + pos.coords.longitude.toFixed(4) + ')';
            btn.style.opacity = '1';
            btn.style.pointerEvents = 'auto';
          },
          function(err) {
            if (statusEl) statusEl.textContent = err.code === 1 ? 'Location access denied.' : 'Unable to get location.';
            btn.style.opacity = '1';
            btn.style.pointerEvents = 'auto';
          },
          { enableHighAccuracy: true, timeout: 15000 }
        );
      };

      window.getMuseumLocation = function() {
        var statusEl = document.getElementById('museum_location_status');
        var btn = document.getElementById('use_museum_location_btn');
        if (!navigator.geolocation) {
          if (statusEl) statusEl.textContent = 'Geolocation not supported.';
          return;
        }
        if (statusEl) statusEl.textContent = 'Getting your location...';
        btn.style.opacity = '0.5';
        btn.style.pointerEvents = 'none';
        navigator.geolocation.getCurrentPosition(
          function(pos) {
            Shiny.setInputValue('submit_museum_lat', pos.coords.latitude);
            Shiny.setInputValue('submit_museum_lng', pos.coords.longitude);
            $('#submit_museum_lat').val(pos.coords.latitude.toFixed(4));
            $('#submit_museum_lng').val(pos.coords.longitude.toFixed(4));
            if (statusEl) statusEl.textContent = 'Location set (' + pos.coords.latitude.toFixed(4) + ', ' + pos.coords.longitude.toFixed(4) + ')';
            btn.style.opacity = '1';
            btn.style.pointerEvents = 'auto';
          },
          function(err) {
            if (statusEl) statusEl.textContent = err.code === 1 ? 'Location access denied.' : 'Unable to get location.';
            btn.style.opacity = '1';
            btn.style.pointerEvents = 'auto';
          },
          { enableHighAccuracy: true, timeout: 15000 }
        );
      };

      // -- MOBILE TAB BAR --------------------------------------------------
      window.mobileTabSwitch = function(tab) {
        var tabLink = document.querySelector('a.nav-link[data-value=\"' + tab + '\"]');
        if (tabLink) tabLink.click();
        document.querySelectorAll('.mob-tab').forEach(function(btn) {
          btn.classList.toggle('active', btn.getAttribute('data-tab') === tab);
        });
      };

      $(document).on('shown.bs.tab', function(e) {
        if (!e.target) return;
        var tabVal = e.target.getAttribute('data-value');
        document.querySelectorAll('.mob-tab').forEach(function(btn) {
          btn.classList.toggle('active', btn.getAttribute('data-tab') === tabVal);
        });
      });

      // -- MAP INFO SCROLL HINT + AUTO-SCROLL ------------------------------
      Shiny.addCustomMessageHandler('scrollToInfoPanel', function(msg) {
        var isMobile = window.innerWidth <= 768;
        if (!isMobile) return;

        var hint = document.getElementById('map-scroll-hint');
        var panel = document.getElementById('map-info-panel-el');
        if (!panel) return;

        if (hint) {
          hint.style.display = '';
          hint.onclick = function() {
            panel.scrollIntoView({ behavior: 'smooth', block: 'start' });
            hint.style.display = 'none';
          };
          setTimeout(function() { hint.style.display = 'none'; }, 4000);
        }

        setTimeout(function() {
          panel.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
        }, 400);
      });

      // -- LEAFLET MAP FIX -------------------------------------------------
      $(document).on('shown.bs.tab', function(e) {
        if (e.target && e.target.getAttribute('data-value') === 'Map') {
          setTimeout(function() {
            window.dispatchEvent(new Event('resize'));
          }, 250);
        }
      });
    ")))
  )
)


# ===========================================================================
# SERVER SECTION
# ===========================================================================

server <- function(input, output, session) {
  
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
  
  # -- REACTIVE VALUES ------------------------------------------------------
  rv <- reactiveValues(
    admin_auth = FALSE,
    submission_success = FALSE,
    submission_error = NULL,
    submissions = db_load_submissions(),
    paintings_data = db_load_paintings(),
    approved_trigger = 0,
    selected_marker = NULL,
    selected_type = NULL,
    filter_painting_id = NULL,
    map_filter = "all"
  )
  
  current_basemap <- reactiveVal("minimal")
  
  
  # -- ADMIN DARK MODE (server-driven) ---------------------------------------
  observeEvent(input$main_tabs, {
    session$sendCustomMessage("setTheme", input$main_tabs)
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  # -- SYNC JS PAINTINGS DATA ON PAGE LOAD -----------------------------------
  enriched_paintings <- function() {
    pd <- isolate(rv$paintings_data)
    if (is.null(pd) || nrow(pd) == 0) return(pd)
    subs <- isolate(rv$submissions)
    approved <- subs[subs$approval_status == "Approved" &
                       (is.na(subs$submission_type) | subs$submission_type == "landscape"), ]
    counts <- if (nrow(approved) > 0) as.data.frame(table(approved$painting_id), stringsAsFactors = FALSE) else data.frame(Var1 = character(), Freq = integer())
    pd$approved_count <- sapply(pd$id, function(id) {
      m <- counts[counts$Var1 == as.character(id), "Freq"]
      if (length(m) > 0) m[1] else 0
    })
    pd$has_museum <- !is.na(pd$museum_name) & pd$museum_name != ""
    pd
  }
  
  session$onFlushed(function() {
    session$sendCustomMessage("updatePaintingsData",
                              jsonlite::toJSON(enriched_paintings(), auto_unbox = TRUE))
  }, once = TRUE)
  
  # -- PAINTING DETAIL FALLBACK (JS array not yet synced) --------------------
  observeEvent(input$request_painting_detail, {
    pid <- input$request_painting_detail$id
    if (is.null(pid)) return()
    p <- rv$paintings_data[rv$paintings_data$id == as.integer(pid), ]
    if (nrow(p) == 0) return()
    session$sendCustomMessage("showPaintingDetail",
                              jsonlite::toJSON(p[1, ], auto_unbox = TRUE))
  })
  
  
  # -- AUTO-REFRESH SUBMISSIONS ON TAB SWITCH --------------------------------
  observeEvent(input$main_tabs, {
    tab <- input$main_tabs
    if (tab %in% c("Gallery", "Compare", "Map")) {
      fresh <- db_load_submissions()
      if (nrow(fresh) != nrow(rv$submissions)) {
        rv$submissions <- fresh
        rv$paintings_data <- db_load_paintings()
        rv$approved_trigger <- rv$approved_trigger + 1
        session$sendCustomMessage("updatePaintingsData",
                                  jsonlite::toJSON(rv$paintings_data, auto_unbox = TRUE))
      }
    }
  })
  
  
  # -- GALLERY / MAP "VIEW COMPARISONS" NAVIGATION ---------------------------
  observeEvent(input$go_compare_painting, {
    val <- input$go_compare_painting
    
    if (is.list(val) && !is.null(val$id)) {
      painting_id <- as.integer(val$id)
    } else if (is.numeric(val)) {
      painting_id <- val
    } else {
      painting_id <- NULL
    }
    
    if (!is.null(painting_id) && painting_id %in% rv$paintings_data$id) {
      rv$filter_painting_id <- as.integer(painting_id)
    } else {
      rv$filter_painting_id <- NULL
    }
    
    session$sendCustomMessage("switchTab", "Compare")
  })
  
  observeEvent(input$clear_compare_filter, {
    rv$filter_painting_id <- NULL
  })
  
  
  # -- CONTRIBUTE BUTTON FROM GALLERY CARD ----------------------------------
  observeEvent(input$contribute_for_painting, {
    val <- input$contribute_for_painting
    if (!is.null(val$id)) {
      updateSelectInput(session, "submit_painting", selected = as.character(val$id))
    }
    session$sendCustomMessage("switchTab", "Contribute")
    shinyjs::delay(100, shinyjs::runjs("selectContributeType('landscape');"))
  })
  
  observeEvent(input$nav_to_upload, {
    session$sendCustomMessage("switchTab", "Contribute")
    shinyjs::delay(100, shinyjs::runjs("selectContributeType('user_painting');"))
  })
  
  
  observeEvent(input$view_painting_from_museum, {
    pid <- input$view_painting_from_museum$id
    if (is.null(pid)) return()
    session$sendCustomMessage("switchTab", "Gallery")
    shinyjs::delay(200, shinyjs::runjs(sprintf("openPaintingDetail(%d);", as.integer(pid))))
  })
  
  
  # -- ADD MUSEUM SUBMISSION (from detail lightbox) --------------------------
  observeEvent(input$submit_add_museum, {
    val <- input$submit_add_museum
    if (is.null(val) || is.null(val$pid) || is.null(val$museum_name) || val$museum_name == "") {
      showNotification("Invalid museum submission.", type = "error")
      return()
    }
    
    tryCatch({
      new_submission <- data.frame(
        submission_id = as.character(as.integer(Sys.time())),
        name = if (!is.null(val$submitter) && val$submitter != "") val$submitter else "Anonymous",
        email = "",
        painting_id = as.integer(val$pid),
        photo_url = "",
        latitude = NA_real_,
        longitude = NA_real_,
        observations = "",
        submission_date = as.character(Sys.Date()),
        approval_status = "Pending",
        submission_type = "add_museum",
        painting_title = NA_character_,
        artist_name = NA_character_,
        painting_year = NA_character_,
        painting_context = NA_character_,
        state = NA_character_,
        region = NA_character_,
        location_notes = NA_character_,
        museum_name = as.character(val$museum_name),
        museum_latitude = if (!is.null(val$museum_lat) && !is.na(val$museum_lat)) as.numeric(val$museum_lat) else NA_real_,
        museum_longitude = if (!is.null(val$museum_lng) && !is.na(val$museum_lng)) as.numeric(val$museum_lng) else NA_real_,
        museum_image_url = NA_character_,
        stringsAsFactors = FALSE
      )
      
      rv$submissions <- rbind(rv$submissions, new_submission)
      db_insert_submission(new_submission)
      showNotification("Museum info submitted for review. Thank you!", type = "message")
    }, error = function(e) {
      showNotification(paste("Failed to submit:", e$message), type = "error", duration = 10)
    })
  })
  
  
  # -- REACTIVE DROPDOWN & FILTER UPDATES ------------------------------------
  observe({
    pd <- rv$paintings_data
    new_choices <- c("Select a painting..." = "", setNames(pd$id, pd$title))
    updateSelectInput(session, "submit_painting", choices = new_choices)
    
    new_artists <- c("All Painters" = "", sort(unique(pd$artist)))
    updateSelectInput(session, "map_artist_filter", choices = new_artists)
    
    session$sendCustomMessage("updatePaintingsData",
                              jsonlite::toJSON(enriched_paintings(), auto_unbox = TRUE))
  })
  
  
  # -- STATS DISPLAY --------------------------------------------------------
  output$stat_submissions <- renderText({ as.character(nrow(rv$submissions)) })
  output$stat_approved <- renderText({ as.character(nrow(rv$submissions[rv$submissions$approval_status == "Approved", ])) })
  
  
  # -- PAINTING CARDS --------------------------------------------------------
  output$painting_cards <- renderUI({
    
    all_subs <- rv$submissions
    
    sub_counts <- if (nrow(all_subs) > 0) {
      as.data.frame(table(all_subs$painting_id), stringsAsFactors = FALSE)
    } else {
      data.frame(Var1 = character(), Freq = integer(), stringsAsFactors = FALSE)
    }
    
    approved_subs <- rv$submissions[rv$submissions$approval_status == "Approved" &
                                      (is.na(rv$submissions$submission_type) | rv$submissions$submission_type == "landscape"), ]
    approved_counts <- if (nrow(approved_subs) > 0) {
      as.data.frame(table(approved_subs$painting_id), stringsAsFactors = FALSE)
    } else {
      data.frame(Var1 = character(), Freq = integer(), stringsAsFactors = FALSE)
    }
    
    museum_subs <- rv$submissions[rv$submissions$approval_status == "Approved" &
                                    !is.na(rv$submissions$submission_type) &
                                    rv$submissions$submission_type == "museum_photo", ]
    
    museum_photos_lookup <- list()
    if (nrow(museum_subs) > 0) {
      for (j in 1:nrow(museum_subs)) {
        pid_char <- as.character(museum_subs[j, "painting_id"])
        if (is.null(museum_photos_lookup[[pid_char]])) {
          museum_photos_lookup[[pid_char]] <- list()
        }
        museum_photos_lookup[[pid_char]] <- c(museum_photos_lookup[[pid_char]], list(museum_subs[j, ]))
      }
    }
    
    cards <- lapply(1:nrow(rv$paintings_data), function(i) {
      p <- rv$paintings_data[i, ]
      
      count_match <- sub_counts[sub_counts$Var1 == as.character(p$id), "Freq"]
      sub_count <- if (length(count_match) > 0) count_match[1] else 0
      
      approved_match <- approved_counts[approved_counts$Var1 == as.character(p$id), "Freq"]
      approved_count <- if (length(approved_match) > 0) approved_match[1] else 0
      
      museum_list <- museum_photos_lookup[[as.character(p$id)]]
      museum_count <- if (!is.null(museum_list)) length(museum_list) else 0
      
      is_private <- !is.null(p$museum_name) && !is.na(p$museum_name) &&
        grepl("private collection", p$museum_name, ignore.case = TRUE)
      
      museum_json <- if (museum_count > 0) {
        photos_arr <- lapply(museum_list, function(ms) {
          list(url = ms$photo_url, name = ms$name)
        })
        gsub("'", "\\\\'", jsonlite::toJSON(photos_arr, auto_unbox = TRUE))
      } else {
        "[]"
      }
      
      tags$div(class = "painting-card",
               `data-title` = tolower(p$title),
               `data-artist` = tolower(p$artist),
               onclick = sprintf("openPaintingDetail(%d)", p$id),
               
               tags$div(class = "painting-card-img-wrap",
                        tags$img(src = p$image_url, class = "painting-image", alt = p$title),
                        tags$div(class = "painting-card-badge", p$year),
                        tags$div(
                          class = paste0("location-status-badge ", if (approved_count > 0) "discovered" else "undiscovered"),
                          if (approved_count > 0) HTML("&#10003; Discovered") else HTML("Undiscovered")
                        ),
                        if (!is_private && museum_count > 0) {
                          tags$div(
                            class = "museum-photo-badge",
                            onclick = sprintf("event.stopPropagation(); openMuseumLightbox('%s', %s);", gsub("'", "\\\\'", p$title), museum_json),
                            title = "View museum photos",
                            HTML(paste0("&#127963; Museum Photo", ifelse(museum_count > 1, "s", ""),
                                        if (museum_count > 1) paste0(" (", museum_count, ")") else ""))
                          )
                        },
                        tags$div(class = "painting-card-overlay",
                                 tags$h3(class = "painting-card-overlay-title", p$title),
                                 tags$div(class = "painting-card-overlay-meta", p$artist)
                        )
               )
      )
    })
    
    add_painting_card <- tags$div(
      class = "painting-card add-painting-card",
      `data-title` = "add",
      `data-artist` = "add",
      onclick = "Shiny.setInputValue('nav_to_upload', Date.now()); ",
      tags$div(class = "painting-card-img-wrap",
               style = "display:flex; align-items:center; justify-content:center; background:var(--glass-bg-light); border:3px dashed var(--glass-border); aspect-ratio:16/10;",
               tags$div(style = "text-align:center; padding:24px;",
                        tags$div(style = "font-size:48px; color:var(--terra); margin-bottom:12px;", HTML("&#43;")),
                        tags$div(style = "font-family:'DM Serif Display',Georgia,serif; font-size:18px; color:var(--text-primary); margin-bottom:6px;", "Add a Painting"),
                        tags$div(style = "font-size:13px; color:var(--text-secondary);", "Contribute to the collection")
               )
      )
    )
    
    tagList(cards, add_painting_card)
  })
  
  
  # -- MAP ------------------------------------------------------------------
  output$main_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "minimal") %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)
  })
  
  # Map marker observer
  observe({
    approved <- rv$submissions[rv$submissions$approval_status == "Approved", ]
    rv$approved_trigger
    filter <- rv$map_filter
    artist_filter <- input$map_artist_filter
    
    filtered_paintings <- if (!is.null(artist_filter) && artist_filter != "") {
      rv$paintings_data[rv$paintings_data$artist == artist_filter, ]
    } else {
      rv$paintings_data
    }
    
    proxy <- leafletProxy("main_map")
    
    proxy %>% clearGroup("submissions")
    if (filter %in% c("all", "submissions") && nrow(approved) > 0) {
      valid_subs <- approved[!is.na(approved$latitude) & !is.na(approved$longitude), ]
      valid_subs <- valid_subs[is.na(valid_subs$submission_type) | valid_subs$submission_type != "museum_photo", ]
      
      if (!is.null(artist_filter) && artist_filter != "" && nrow(valid_subs) > 0) {
        artist_painting_ids <- filtered_paintings$id
        valid_subs <- valid_subs[valid_subs$painting_id %in% artist_painting_ids, ]
      }
      
      if (nrow(valid_subs) > 0) {
        valid_subs$painting_title <- sapply(valid_subs$painting_id, function(pid) {
          match_row <- rv$paintings_data[rv$paintings_data$id == pid, ]
          if (nrow(match_row) > 0) match_row$title[1] else "Unknown Location"
        })
        
        proxy %>% addCircleMarkers(
          data = valid_subs,
          lng = ~longitude, lat = ~latitude,
          radius = 8, color = "#2563EB", fillColor = "#3B82F6", fillOpacity = 0.85,
          weight = 2, stroke = TRUE, group = "submissions",
          layerId = ~paste0("submission_", submission_id),
          label = ~paste0(painting_title, " (", name, ")"),
          labelOptions = labelOptions(style = list("font-weight" = "600", "font-family" = "DM Sans, sans-serif"), textsize = "13px", direction = "top", offset = c(0, -10))
        )
      }
    }
    
    proxy %>% clearGroup("museums")
    if (filter %in% c("all", "museums")) {
      museum_data <- filtered_paintings[!is.na(filtered_paintings$museum_latitude) & !is.na(filtered_paintings$museum_longitude), ]
      if (nrow(museum_data) > 0) {
        museum_data <- museum_data[is.na(museum_data$museum_name) |
                                     !grepl("private collection", museum_data$museum_name, ignore.case = TRUE), ]
      }
      if (nrow(museum_data) > 0) {
        proxy %>% addCircleMarkers(
          data = museum_data,
          lng = ~museum_longitude, lat = ~museum_latitude,
          radius = 8, color = "#DC3545", fillColor = "#E25563", fillOpacity = 0.85,
          weight = 2, stroke = TRUE, group = "museums",
          layerId = ~paste0("museum_", id),
          label = ~paste0(title, " \u2014 ", museum_name),
          labelOptions = labelOptions(style = list("font-weight" = "600", "font-family" = "DM Sans, sans-serif"), textsize = "13px", direction = "top", offset = c(0, -10))
        )
      }
    }
  })
  
  
  # -- MAP FILTER TOGGLE -----------------------------------------------------
  observeEvent(input$set_map_filter, {
    new_filter <- input$set_map_filter
    leafletProxy("main_map") |> clearGroup("state_highlight")
    if (new_filter %in% c("all", "submissions", "museums")) {
      rv$map_filter <- new_filter
      shinyjs::runjs(sprintf("
        document.querySelectorAll('.map-filter-btn').forEach(function(btn) { btn.classList.remove('active'); });
        document.getElementById('map_filter_%s').classList.add('active');
      ", new_filter))
    }
  })
  
  
  # -- STATE FILTER ON MAP ---------------------------------------------------
  observeEvent(input$map_state_filter, {
    st <- input$map_state_filter
    proxy <- leafletProxy("main_map")
    
    #clearing previously highlighted states
    proxy |> clearGroup("state_highlight")
    
    if (is.null(st) || st == "") {
      rv$selected_marker <- NULL
      rv$selected_type <- NULL
      proxy |> setView(lng = -98.5, lat = 39.8, zoom = 4)
      return()
    }
    
    # Alaska and Hawaii are not in the maps "state" database (lower 48 only)
    non_contiguous <- c("Alaska", "Hawaii")
    
    if (st %in% non_contiguous) {
      sc <- state_centers[state_centers$state == st, ]
      if (nrow(sc) > 0) {
        zoom <- if (st == "Alaska") 4 else 7
        proxy %>% flyTo(lng = sc$lng[1], lat = sc$lat[1], zoom = zoom)
      }
    } else {
      # Get state polygon from the maps package (lower 48 only)
      state_map <- map("state", regions = tolower(st), plot = FALSE, fill = TRUE)
      
      # Zoom to fit the actual state bounds
      x_range <- range(state_map$x, na.rm = TRUE)
      y_range <- range(state_map$y, na.rm = TRUE)
      
      proxy %>%
        addPolygons(
          lng = state_map$x,
          lat = state_map$y,
          group = "state_highlight",
          fillColor = "#E8976B",
          fillOpacity = 0.15,
          color = "#E8976B",
          weight = 2,
          opacity = 0.8,
          options = pathOptions(interactive = FALSE)
        ) %>%
        flyToBounds(
          lng1 = x_range[1],
          lat1 = y_range[1],
          lng2 = x_range[2],
          lat2 = y_range[2]
        )
    }
    
    # Show paintings for this state in the info panel
    rv$selected_type <- "state_browse"
    rv$selected_marker <- st
  })
  
  
  # -- USER LIVE LOCATION MARKER --------------------------------------------
  observeEvent(input$user_location, {
    loc <- input$user_location
    proxy <- leafletProxy("main_map")
    
    if (is.null(loc)) {
      proxy %>% clearGroup("user_location")
      return()
    }
    
    proxy %>%
      clearGroup("user_location") %>%
      addMarkers(
        lng = loc$lng, lat = loc$lat,
        group = "user_location",
        icon = makeIcon(iconUrl = NULL, iconWidth = 18, iconHeight = 18, iconAnchorX = 9, iconAnchorY = 9),
        options = markerOptions(interactive = FALSE)
      )
    
    shinyjs::runjs("
      (function() {
        var markers = document.querySelectorAll('.leaflet-marker-icon');
        for (var i = markers.length - 1; i >= 0; i--) {
          var m = markers[i];
          if (!m.src || m.src === '' || m.src === window.location.href) {
            m.style.background = 'none';
            m.style.border = 'none';
            m.style.boxShadow = 'none';
            m.style.width = '40px';
            m.style.height = '40px';
            m.style.marginLeft = '-20px';
            m.style.marginTop = '-20px';
            m.innerHTML = '<div class=\"user-location-pulse\"><div class=\"ring\"></div><div class=\"dot\"></div></div>';
            break;
          }
        }
      })();
    ")
  })
  
  
  # -- MARKER CLICK -> INFO PANEL -------------------------------------------
  observeEvent(input$main_map_marker_click, {
    click <- input$main_map_marker_click
    if (is.null(click) || is.null(click$id)) return()
    
    marker_id <- click$id
    
    if (grepl("^painting_", marker_id)) {
      pid <- as.integer(sub("painting_", "", marker_id))
      rv$selected_marker <- pid
      rv$selected_type <- "painting"
    } else if (grepl("^submission_", marker_id)) {
      sid <- sub("submission_", "", marker_id)
      rv$selected_marker <- sid
      rv$selected_type <- "submission"
    } else if (grepl("^museum_", marker_id)) {
      pid <- as.integer(sub("museum_", "", marker_id))
      rv$selected_marker <- pid
      rv$selected_type <- "museum"
    }
    
    leafletProxy("main_map") %>%
      flyTo(lng = click$lng, lat = click$lat, zoom = max(input$main_map_zoom, 8))
    
    session$sendCustomMessage("scrollToInfoPanel", list(t = as.numeric(Sys.time())))
  })
  
  
  # -- GO TO MUSEUM OBSERVER ------------------------------------------------
  observeEvent(input$go_to_museum, {
    pid <- input$go_to_museum$id
    if (is.null(pid)) return()
    
    p <- rv$paintings_data[rv$paintings_data$id == pid, ]
    if (nrow(p) == 0) return()
    p <- p[1, ]
    if (is.na(p$museum_latitude) || is.na(p$museum_longitude)) return()
    
    session$sendCustomMessage("switchTab", "Map")
    
    if (!rv$map_filter %in% c("all", "museums")) {
      rv$map_filter <- "all"
      shinyjs::runjs("
        document.querySelectorAll('.map-filter-btn').forEach(function(btn) { btn.classList.remove('active'); });
        document.getElementById('map_filter_all').classList.add('active');
      ")
    }
    
    rv$selected_marker <- pid
    rv$selected_type <- "museum"
    
    shinyjs::delay(300, {
      leafletProxy("main_map") %>%
        flyTo(lng = p$museum_longitude, lat = p$museum_latitude, zoom = 10)
    })
  })
  
  
  # -- GO TO PAINTING OBSERVER ----------------------------------------------
  observeEvent(input$go_to_painting, {
    pid <- input$go_to_painting$id
    if (is.null(pid)) return()
    
    p <- rv$paintings_data[rv$paintings_data$id == pid, ]
    if (nrow(p) == 0) return()
    p <- p[1, ]
    if (is.na(p$latitude) || is.na(p$longitude)) return()
    
    rv$selected_marker <- pid
    rv$selected_type <- "painting"
    
    leafletProxy("main_map") %>%
      flyTo(lng = p$longitude, lat = p$latitude, zoom = max(input$main_map_zoom, 8))
  })
  
  
  # -- INFO PANEL CONTENT ---------------------------------------------------
  output$map_info_content <- renderUI({
    if (is.null(rv$selected_marker) || is.null(rv$selected_type)) {
      return(tags$div(class = "map-info-placeholder",
                      tags$div(class = "placeholder-icon", HTML("&#128205;")),
                      tags$p("Click a marker on the map to view location details.")
      ))
    }
    
    if (rv$selected_type == "painting") {
      p <- rv$paintings_data[rv$paintings_data$id == rv$selected_marker, ]
      if (nrow(p) == 0) return(NULL)
      p <- p[1, ]
      
      approved_for_painting <- rv$submissions[rv$submissions$approval_status == "Approved" & rv$submissions$painting_id == p$id, ]
      ap_count <- nrow(approved_for_painting)
      
      tagList(
        tags$div(class = "map-info-header",
                 tags$div(class = "map-info-dot painting"),
                 tags$span(class = "map-info-type-label", "Bierstadt Painting")
        ),
        tags$h3(class = "map-info-title", p$title),
        tags$div(class = "map-info-meta", paste0(p$artist, " | ", p$year)),
        tags$img(class = "map-info-image", src = p$image_url, alt = p$title),
        tags$p(class = "map-info-context", p$context),
        if (ap_count > 0) {
          tags$div(class = "map-info-cta",
                   onclick = sprintf("Shiny.setInputValue('go_compare_painting', {id: %d, t: Date.now()});", p$id),
                   HTML(paste0("View Comparison", ifelse(ap_count != 1, "s", ""), " &rarr;"))
          )
        },
        if (!is.null(p$museum_name) && !is.na(p$museum_name) &&
            grepl("private collection", p$museum_name, ignore.case = TRUE)) {
          tags$div(class = "map-info-private-notice",
                   HTML("&#128274;"),
                   tags$span("This painting is held in a private collection and is not publicly viewable.")
          )
        } else if (!is.null(p$museum_latitude) && !is.na(p$museum_latitude) &&
                   !is.null(p$museum_longitude) && !is.na(p$museum_longitude)) {
          tags$div(class = "map-info-cta museum",
                   onclick = sprintf("Shiny.setInputValue('go_to_museum', {id: %d, t: Date.now()});", p$id),
                   HTML("View Museum &rarr;")
          )
        },
        tags$div(class = "map-info-cta travel",
                 onclick = sprintf("window.open('https://www.google.com/maps/dir/?api=1&destination=%f,%f', '_blank');", p$latitude, p$longitude),
                 HTML("Get Directions &rarr;")
        ),
        tags$div(class = "map-info-coords",
                 tags$div(class = "coord-box",
                          tags$div(class = "coord-label", "Latitude"),
                          tags$div(class = "coord-value", round(p$latitude, 4))
                 ),
                 tags$div(class = "coord-box",
                          tags$div(class = "coord-label", "Longitude"),
                          tags$div(class = "coord-value", round(p$longitude, 4))
                 )
        )
      )
      
    } else if (rv$selected_type == "submission") {
      sub <- rv$submissions[rv$submissions$submission_id == rv$selected_marker & rv$submissions$approval_status == "Approved", ]
      if (nrow(sub) == 0) return(NULL)
      sub <- sub[1, ]
      
      painting <- rv$paintings_data[rv$paintings_data$id == sub$painting_id, ]
      painting_title <- if (nrow(painting) > 0) painting$title[1] else "Unknown Location"
      
      tagList(
        tags$div(class = "map-info-header",
                 tags$div(class = "map-info-dot submission"),
                 tags$span(class = "map-info-type-label", "Community Submission")
        ),
        tags$h3(class = "map-info-title", painting_title),
        tags$div(class = "map-info-meta", paste0("Submitted by ", sub$name, " | ", sub$submission_date)),
        tags$img(class = "map-info-image", src = sub$photo_url, alt = painting_title),
        if (!is.null(sub$observations) && sub$observations != "") {
          tags$div(class = "map-info-observations", sub$observations)
        },
        tags$div(class = "map-info-cta",
                 onclick = sprintf("Shiny.setInputValue('go_compare_painting', {id: %d, t: Date.now()});", sub$painting_id),
                 HTML("View Comparison &rarr;")
        ),
        tags$div(class = "map-info-cta travel",
                 onclick = sprintf("window.open('https://www.google.com/maps/dir/?api=1&destination=%f,%f', '_blank');", sub$latitude, sub$longitude),
                 HTML("Get Directions &rarr;")
        ),
        tags$div(class = "map-info-coords",
                 tags$div(class = "coord-box",
                          tags$div(class = "coord-label", "Latitude"),
                          tags$div(class = "coord-value", round(sub$latitude, 4))
                 ),
                 tags$div(class = "coord-box",
                          tags$div(class = "coord-label", "Longitude"),
                          tags$div(class = "coord-value", round(sub$longitude, 4))
                 )
        )
      )
    } else if (rv$selected_type == "museum") {
      p <- rv$paintings_data[rv$paintings_data$id == rv$selected_marker, ]
      if (nrow(p) == 0) return(NULL)
      p <- p[1, ]
      
      museum_subs <- rv$submissions[rv$submissions$approval_status == "Approved" &
                                      !is.na(rv$submissions$submission_type) &
                                      rv$submissions$submission_type == "museum_photo" &
                                      rv$submissions$painting_id == p$id, ]
      museum_count <- nrow(museum_subs)
      
      museum_json <- if (museum_count > 0) {
        photos_arr <- lapply(1:museum_count, function(j) {
          list(url = museum_subs[j, "photo_url"], name = museum_subs[j, "name"])
        })
        gsub("'", "\\\\'", jsonlite::toJSON(photos_arr, auto_unbox = TRUE))
      } else {
        "[]"
      }
      
      tagList(
        tags$div(class = "map-info-header",
                 tags$div(class = "map-info-dot museum"),
                 tags$span(class = "map-info-type-label", "Museum / Collection")
        ),
        tags$h3(class = "map-info-title", ifelse(!is.null(p$museum_name) && p$museum_name != "", p$museum_name, "Unknown Museum")),
        tags$div(class = "map-info-meta", paste0("Houses: ", p$title)),
        tags$img(class = "map-info-image",
                 src = if (!is.null(p$museum_image_url) && !is.na(p$museum_image_url) && p$museum_image_url != "") {
                   p$museum_image_url
                 } else {
                   p$image_url
                 },
                 alt = ifelse(!is.null(p$museum_name) && !is.na(p$museum_name), p$museum_name, p$title)),
        tags$p(class = "map-info-context", paste0("This museum or collection currently holds \"", p$title, "\" by ", p$artist, " (", p$year, ").")),
        tags$div(class = "map-info-cta",
                 onclick = sprintf("Shiny.setInputValue('view_painting_from_museum', {id: %d, t: Date.now()});", p$id),
                 HTML("View Painting Info &rarr;")
        ),
        if (museum_count > 0) {
          tags$div(class = "map-info-cta",
                   onclick = sprintf("openMuseumLightbox('%s', %s);", gsub("'", "\\\\'", p$title), museum_json),
                   HTML(paste0("View Museum Photo", ifelse(museum_count > 1, "s", ""),
                               if (museum_count > 1) paste0(" (", museum_count, ")") else "", " &rarr;"))
          )
        },
        tags$div(class = "map-info-cta travel",
                 onclick = sprintf("window.open('https://www.google.com/maps/dir/?api=1&destination=%f,%f', '_blank');", p$museum_latitude, p$museum_longitude),
                 HTML("Get Directions &rarr;")
        ),
        tags$div(class = "map-info-coords",
                 tags$div(class = "coord-box",
                          tags$div(class = "coord-label", "Latitude"),
                          tags$div(class = "coord-value", round(p$museum_latitude, 4))
                 ),
                 tags$div(class = "coord-box",
                          tags$div(class = "coord-label", "Longitude"),
                          tags$div(class = "coord-value", round(p$museum_longitude, 4))
                 )
        )
      )
    } else if (rv$selected_type == "state_browse") {
      st <- rv$selected_marker
      
      # Filter paintings by state if column exists
      state_paintings <- if ("state" %in% names(rv$paintings_data)) {
        rv$paintings_data[!is.na(rv$paintings_data$state) & rv$paintings_data$state == st, ]
      } else {
        data.frame()
      }
      
      # Also find submissions for this state
      state_subs <- rv$submissions[rv$submissions$approval_status == "Approved" &
                                     !is.na(rv$submissions$state) &
                                     rv$submissions$state == st, ]
      
      painting_cards <- if (nrow(state_paintings) > 0) {
        lapply(1:nrow(state_paintings), function(i) {
          p <- state_paintings[i, ]
          ap_count <- nrow(rv$submissions[rv$submissions$approval_status == "Approved" &
                                            rv$submissions$painting_id == p$id, ])
          tags$div(style = "display: flex; gap: 12px; padding: 12px 0; border-bottom: 1px solid var(--glass-border-subtle); cursor: pointer;",
                   onclick = sprintf("Shiny.setInputValue('go_to_painting', {id: %d, t: Date.now()});", p$id),
                   tags$img(src = p$image_url, style = "width: 60px; height: 40px; object-fit: cover; border-radius: 8px; flex-shrink: 0;"),
                   tags$div(
                     tags$div(style = "font-weight: 700; font-size: 14px; color: var(--text-primary);", p$title),
                     tags$div(style = "font-size: 12px; color: var(--text-muted);",
                              paste0(p$artist, " | ", ap_count, " submission", ifelse(ap_count != 1, "s", "")))
                   )
          )
        })
      } else {
        list(tags$p(style = "color: var(--text-muted); font-size: 13px; font-style: italic;",
                    "No paintings catalogued for this state yet. Upload one in the Contribute tab!"))
      }
      
      tagList(
        tags$div(class = "map-info-header",
                 tags$div(class = "map-info-dot painting"),
                 tags$span(class = "map-info-type-label", "State Explorer")
        ),
        tags$h3(class = "map-info-title", st),
        tags$div(class = "map-info-meta", paste0(nrow(state_paintings), " painting", ifelse(nrow(state_paintings) != 1, "s", ""), " catalogued")),
        tags$div(style = "margin-top: 8px;", painting_cards)
      )
    }
  })
  
  outputOptions(output, "main_map", suspendWhenHidden = FALSE)
  
  
  # -- BASEMAP AUTO-SWITCH ---------------------------------------------------
  observeEvent(input$main_map_zoom, {
    zoom <- input$main_map_zoom
    if (is.null(zoom)) return()
    
    proxy <- leafletProxy("main_map")
    
    if (zoom >= 8 && current_basemap() != "satellite") {
      proxy %>% clearGroup("minimal") %>% addProviderTiles(providers$Esri.WorldImagery, group = "satellite")
      current_basemap("satellite")
    } else if (zoom < 8 && current_basemap() != "minimal") {
      proxy %>% clearGroup("satellite") %>% addProviderTiles(providers$CartoDB.Positron, group = "minimal")
      current_basemap("minimal")
    }
  })
  
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "Map") {
      shinyjs::delay(200, { shinyjs::runjs("window.dispatchEvent(new Event('resize'));") })
    }
    if (input$main_tabs != "Compare") {
      rv$filter_painting_id <- NULL
    }
  })
  
  
  # -- SUBMISSION FORM MESSAGES ---------------------------------------------
  output$submit_message <- renderUI({
    if (rv$submission_success) {
      tags$div(class = "alert-success-custom", HTML("&#10003; Photo submitted successfully! It's pending admin review."))
    } else if (!is.null(rv$submission_error)) {
      tags$div(class = "alert-error-custom", HTML(paste0("&#10007; ", rv$submission_error)))
    }
  })
  
  
  # -- FORM SUBMISSION HANDLER -----------------------------------------------
  observeEvent(input$submit_button, {
    
    rv$submission_success <- FALSE
    rv$submission_error <- NULL
    
    sub_type <- input$submit_type
    
    if (sub_type %in% c("landscape", "museum_photo") && input$submit_painting == "") {
      rv$submission_error <- "Please select a painting."
      return()
    }
    if (sub_type == "user_painting") {
      if (is.null(input$submit_painting_title) || trimws(input$submit_painting_title) == "") {
        rv$submission_error <- "Please enter the painting title."
        return()
      }
      if (is.null(input$submit_artist_name) || trimws(input$submit_artist_name) == "") {
        rv$submission_error <- "Please enter the artist name."
        return()
      }
      if (is.null(input$submit_state) || input$submit_state == "" || input$submit_state == "Unknown") {
        rv$submission_error <- "Please select a state for the painting location."
        return()
      }
    }
    if (is.null(input$submit_photo)) {
      rv$submission_error <- "Please upload a photo."
      return()
    }
    if (sub_type == "landscape" && (is.na(input$submit_latitude) || is.na(input$submit_longitude))) {
      rv$submission_error <- "Please enter GPS coordinates or use the location button."
      return()
    }
    if (input$submit_photo$size > 5 * 1024 * 1024) {
      rv$submission_error <- "File must be less than 5MB."
      return()
    }
    
    tryCatch({
      submission_id <- as.character(as.integer(Sys.time()))
      file_ext <- tolower(tools::file_ext(input$submit_photo$name))
      if (!(file_ext %in% c("jpg", "jpeg", "png"))) file_ext <- "jpg"
      if (file_ext == "jpeg") file_ext <- "jpg"
      
      photo_url <- upload_to_storage(input$submit_photo$datapath, submission_id, file_ext)
      
      pid <- if (sub_type %in% c("landscape", "museum_photo")) {
        as.integer(input$submit_painting)
      } else {
        NA_integer_
      }
      
      include_museum <- isTRUE(input$include_museum_info) && sub_type == "user_painting"
      
      new_submission <- data.frame(
        submission_id = submission_id,
        name = ifelse(input$submit_name == "", "Anonymous", input$submit_name),
        email = input$submit_email,
        painting_id = pid,
        photo_url = photo_url,
        latitude = if (sub_type == "landscape") input$submit_latitude else NA_real_,
        longitude = if (sub_type == "landscape") input$submit_longitude else NA_real_,
        observations = input$submit_observations,
        submission_date = as.character(Sys.Date()),
        approval_status = "Pending",
        submission_type = sub_type,
        painting_title = if (sub_type == "user_painting") trimws(input$submit_painting_title) else NA_character_,
        artist_name = if (sub_type == "user_painting") trimws(input$submit_artist_name) else NA_character_,
        painting_year = if (sub_type == "user_painting") trimws(input$submit_painting_year) else NA_character_,
        painting_context = if (sub_type == "user_painting") trimws(input$submit_painting_context) else NA_character_,
        state = if (sub_type == "user_painting") input$submit_state else NA_character_,
        region = if (sub_type == "user_painting") trimws(input$submit_region) else NA_character_,
        location_notes = if (sub_type == "user_painting") trimws(input$submit_location_notes) else NA_character_,
        museum_name = if (include_museum) trimws(input$submit_museum_name %||% "") else NA_character_,
        museum_latitude = if (include_museum && !is.null(input$submit_museum_lat) && !is.na(input$submit_museum_lat)) input$submit_museum_lat else NA_real_,
        museum_longitude = if (include_museum && !is.null(input$submit_museum_lng) && !is.na(input$submit_museum_lng)) input$submit_museum_lng else NA_real_,
        museum_image_url = NA_character_,
        stringsAsFactors = FALSE
      )
      
      rv$submissions <- rbind(rv$submissions, new_submission)
      db_insert_submission(new_submission)
      rv$submission_success <- TRUE
      
      shinyjs::delay(2000, shinyjs::runjs("showContributeLanding();"))
      
      updateTextInput(session, "submit_name", value = "")
      updateTextInput(session, "submit_email", value = "")
      updateSelectInput(session, "submit_painting", selected = "")
      updateNumericInput(session, "submit_latitude", value = NA)
      updateNumericInput(session, "submit_longitude", value = NA)
      updateTextAreaInput(session, "submit_observations", value = "")
      
    }, error = function(e) {
      rv$submission_error <- paste("Failed:", e$message)
    })
  })
  
  
  # -- COMPARISON GALLERY ----------------------------------------------------
  output$comparison_gallery <- renderUI({
    rv$approved_trigger
    approved <- rv$submissions[rv$submissions$approval_status == "Approved" &
                                 (is.na(rv$submissions$submission_type) | rv$submissions$submission_type == "landscape"), ]
    filter_id <- rv$filter_painting_id
    
    if (nrow(approved) == 0) {
      return(tags$div(class = "no-comparisons", HTML("No approved comparisons yet. Be the first to contribute!")))
    }
    
    if (!is.null(filter_id)) {
      filtered <- approved[approved$painting_id == filter_id, ]
      filter_painting <- rv$paintings_data[rv$paintings_data$id == filter_id, ]
      filter_name <- if (nrow(filter_painting) > 0) filter_painting$title[1] else "Unknown"
    } else {
      filtered <- approved
    }
    
    if (nrow(filtered) == 0) {
      return(tagList(
        tags$div(class = "compare-filter-banner",
                 tags$span(paste0("No comparisons found for this painting.")),
                 tags$div(class = "compare-filter-see-all",
                          onclick = "Shiny.setInputValue('clear_compare_filter', Math.random());",
                          HTML("See All Comparisons &rarr;"))
        )
      ))
    }
    
    cards <- lapply(1:nrow(filtered), function(i) {
      sub <- filtered[i, ]
      painting <- rv$paintings_data[rv$paintings_data$id == sub$painting_id, ]
      if (nrow(painting) == 0) return(NULL)
      
      tags$div(class = "comparison-thumb",
               `data-submitter` = tolower(sub$name),
               `data-painting` = if (nrow(painting) > 0) tolower(painting$title[1]) else "",
               `data-artist` = if (nrow(painting) > 0) tolower(painting$artist[1]) else "",
               onclick = sprintf("openComparisonLightbox('%s', '%s')", painting$image_url, sub$photo_url),
               tags$div(class = "comparison-thumb-submitter", sub$name),
               tags$img(src = painting$image_url, alt = painting$title),
               tags$div(class = "comparison-thumb-overlay",
                        tags$div(class = "comparison-thumb-label", HTML("&#8644; Compare"))
               )
      )
    })
    
    tagList(
      if (!is.null(filter_id)) {
        tags$div(class = "compare-filter-banner",
                 tags$span(class = "compare-filter-text",
                           HTML(paste0("Showing comparisons for <strong>", htmltools::htmlEscape(filter_name), "</strong>"))
                 ),
                 tags$div(class = "compare-filter-see-all",
                          onclick = "Shiny.setInputValue('clear_compare_filter', Math.random());",
                          HTML("See All Comparisons &rarr;"))
        )
      },
      tags$div(class = "comparison-grid", cards)
    )
  })
  
  
  # -- ADMIN AUTHENTICATION -------------------------------------------------
  observeEvent(input$admin_login, {
    if (input$admin_password == "admin123") rv$admin_auth <- TRUE
  })
  
  output$admin_authenticated <- reactive({ rv$admin_auth })
  outputOptions(output, "admin_authenticated", suspendWhenHidden = FALSE)
  
  
  # -- ADMIN TABLE -----------------------------------------------------------
  admin_filtered <- reactive({
    input$refresh_admin
    subs <- rv$submissions
    if (nrow(subs) == 0) return(subs)
    
    type_filter <- input$admin_type_filter
    if (!is.null(type_filter) && type_filter != "") {
      subs <- subs[!is.na(subs$submission_type) & subs$submission_type == type_filter, ]
    }
    
    status_filter <- input$admin_status_filter
    if (!is.null(status_filter) && status_filter != "") {
      subs <- subs[subs$approval_status == status_filter, ]
    }
    
    subs
  })
  
  output$admin_table <- renderDT({
    filtered <- admin_filtered()
    
    if (nrow(filtered) == 0) return(datatable(data.frame(Message = "No submissions match the current filters."), rownames = FALSE))
    
    display <- filtered[, c("submission_id", "name", "submission_type", "painting_id", "latitude", "longitude", "submission_date", "approval_status")]
    
    display$painting_id <- sapply(display$painting_id, function(pid) {
      if (is.na(pid)) return("(user painting)")
      match_row <- rv$paintings_data[rv$paintings_data$id == pid, ]
      if (nrow(match_row) > 0) match_row$title[1] else as.character(pid)
    })
    names(display)[names(display) == "painting_id"] <- "painting"
    
    datatable(
      display,
      options = list(pageLength = 25, order = list(list(6, 'desc'))),
      rownames = FALSE,
      selection = 'single'
    )
  })
  
  
  # -- APPROVE SUBMISSION ---------------------------------------------------
  observeEvent(input$approve_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      filtered <- admin_filtered()
      if (idx > nrow(filtered)) return()
      sid <- filtered[idx, "submission_id"]
      sub_row <- filtered[idx, ]
      
      db_update_status(sid, "Approved")
      rv$submissions[rv$submissions$submission_id == sid, "approval_status"] <- "Approved"
      
      if (!is.na(sub_row$submission_type) && sub_row$submission_type == "user_painting") {
        tryCatch({
          new_pid <- db_promote_to_painting(sub_row)
          rv$submissions[rv$submissions$submission_id == sid, "painting_id"] <- new_pid
          rv$paintings_data <- db_load_paintings()
          session$sendCustomMessage("updatePaintingsData",
                                    jsonlite::toJSON(enriched_paintings(), auto_unbox = TRUE))
          showNotification(paste0("Painting promoted to collection (ID: ", new_pid, ")"), type = "message")
        }, error = function(e) {
          showNotification(paste0("Approved but failed to promote: ", e$message), type = "error", duration = 10)
        })
      } else if (!is.na(sub_row$submission_type) && sub_row$submission_type == "add_museum") {
        tryCatch({
          db_update_painting_museum(
            painting_id = sub_row$painting_id,
            museum_name = sub_row$museum_name,
            museum_lat = sub_row$museum_latitude,
            museum_lng = sub_row$museum_longitude,
            museum_img = NA_character_
          )
          rv$paintings_data <- db_load_paintings()
          session$sendCustomMessage("updatePaintingsData",
                                    jsonlite::toJSON(enriched_paintings(), auto_unbox = TRUE))
          showNotification("Museum info added to painting.", type = "message")
        }, error = function(e) {
          showNotification(paste0("Approved but failed to update museum: ", e$message), type = "error", duration = 10)
        })
      }
      
      rv$approved_trigger <- rv$approved_trigger + 1
      showNotification("Approved!", type = "message")
    }
  })
  
  
  # -- REJECT SUBMISSION ----------------------------------------------------
  observeEvent(input$reject_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      filtered <- admin_filtered()
      if (idx > nrow(filtered)) return()
      sid <- filtered[idx, "submission_id"]
      
      db_update_status(sid, "Rejected")
      rv$submissions[rv$submissions$submission_id == sid, "approval_status"] <- "Rejected"
      showNotification("Rejected.", type = "warning")
    }
  })
  
  
  # -- DELETE SUBMISSION ----------------------------------------------------
  observeEvent(input$delete_submission, {
    if (length(input$admin_table_rows_selected) > 0) {
      idx <- input$admin_table_rows_selected
      filtered <- admin_filtered()
      if (idx > nrow(filtered)) return()
      sid <- filtered[idx, "submission_id"]
      
      db_delete_submission(sid)
      rv$submissions <- rv$submissions[rv$submissions$submission_id != sid, ]
      rv$approved_trigger <- rv$approved_trigger + 1
      showNotification("Deleted.", type = "error")
    }
  })
  
  
  # -- REFRESH ADMIN DATA ---------------------------------------------------
  observeEvent(input$refresh_admin, {
    rv$submissions <- db_load_submissions()
    rv$paintings_data <- db_load_paintings()
    rv$approved_trigger <- rv$approved_trigger + 1
    session$sendCustomMessage("updatePaintingsData",
                              jsonlite::toJSON(rv$paintings_data, auto_unbox = TRUE))
    showNotification("Data refreshed from database!", type = "message")
  })
}


# ===========================================================================
# LAUNCH THE APP
# ===========================================================================

shinyApp(ui = ui, server = server)
