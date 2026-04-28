# =============================================================================
#  Landscape Through Time
#  -----------------------------------------------------------------------------
#  An R Shiny web application connecting historical American landscape paintings
#  with their real-world locations through a community-driven "treasure hunt."
#  Paintings enter the system with fuzzy location info (state, region, landmark);
#  the community discovers exact GPS coordinates by visiting locations and
#  submitting geotagged photos.
#
#  Features:
#    * Gallery browse with painting detail lightbox
#    * Interactive Leaflet map (community photos + museum markers)
#    * Side-by-side historical / modern photo comparison
#    * Admin panel for reviewing community submissions
#
#  Authors:    Alex, Ben, Emily
#  Started:    February 2026
#  Backend:    Supabase (PostgreSQL) via the connection pooler
#  Hosting:    shinyapps.io
#
#  Deployment notes:
#    * File MUST be named `app.R` for shinyapps.io.
#    * Required env vars (.Renviron): SUPABASE_URL, SUPABASE_ANON_KEY,
#      SUPABASE_HOST, SUPABASE_PORT, SUPABASE_DB, SUPABASE_USER,
#      SUPABASE_PASSWORD.
#    * SUPABASE_HOST must be the pooler hostname (port 6543) -- the direct
#      hostname resolves to IPv6, which shinyapps.io does not support.
#    * `gssencmode = "disable"` is required on the pooler connection.
#
#  File layout:
#    LIBRARIES ............................................. L   43
#    SUPABASE STORAGE HELPERS .............................. L   63
#    DATABASE HELPERS ...................................... L  148
#    INITIAL DATA LOAD ..................................... L  585
#    CUSTOM CSS ............................................ L  609
#    UI .................................................... L 2816
#    SERVER ................................................ L 3996
#    APP LAUNCH ............................................ L 5609
# =============================================================================


# =============================================================================
# LIBRARIES
# =============================================================================

# Core Shiny + UI framework
library(shiny)        # Web app framework
library(bslib)        # Bootstrap 5 theming + page_navbar
library(shinyjs)      # JS helpers (show/hide, toggleClass, etc.)
library(htmltools)    # HTML construction utilities

# Mapping + data tables
library(leaflet)      # Interactive map (community photos, museums)
library(DT)           # Admin panel data tables
library(maps)         # Provides state.center used for map "fly to"

# Database + storage
library(DBI)          # Generic DB interface
library(RPostgres)    # Postgres driver for Supabase
library(httr)         # HTTP for Supabase Storage REST calls

# =============================================================================
# SUPABASE STORAGE HELPERS
# -----------------------------------------------------------------------------
# Image uploads/deletes go through the Supabase Storage REST API rather than
# the Postgres connection. The `submissions` bucket is public-read, anon-write
# (gated by RLS policies on the bucket).
# =============================================================================

readRenviron(".Renviron")

SUPABASE_URL      <- Sys.getenv("SUPABASE_URL")
SUPABASE_ANON_KEY <- Sys.getenv("SUPABASE_ANON_KEY")
STORAGE_BUCKET    <- "submissions"

# Upload an image file to Supabase Storage and return its public URL.
# Used by the public submission flow (community photos) and the admin
# upload flow (new painting images). `submission_id` is used as the
# object key, so each submission produces exactly one file.
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

# Delete a file from Supabase Storage.  ONLY deletes if the URL points at
# OUR Supabase project's storage (not Wikipedia / external sources).
# Returns invisibly: TRUE on successful delete, FALSE if URL was external
# or the delete request failed.  Never throws -- callers may delete a
# DB row even if storage cleanup fails.
delete_from_storage <- function(image_url) {
  if (is.null(image_url) || is.na(image_url) || image_url == "") {
    return(invisible(FALSE))
  }
  if (SUPABASE_URL == "" || SUPABASE_ANON_KEY == "") {
    return(invisible(FALSE))
  }
  
  # Only act on URLs that point at our Supabase storage public endpoint.
  prefix <- paste0(SUPABASE_URL, "/storage/v1/object/public/")
  if (!startsWith(image_url, prefix)) {
    return(invisible(FALSE))  # External URL (Wikipedia, etc.) -- nothing to do.
  }
  
  # Path after the prefix is "<bucket>/<filename>", which is what the
  # delete endpoint expects.
  path <- substring(image_url, nchar(prefix) + 1)
  delete_url <- paste0(SUPABASE_URL, "/storage/v1/object/", path)
  
  res <- tryCatch(
    httr::DELETE(
      delete_url,
      httr::add_headers(`Authorization` = paste("Bearer", SUPABASE_ANON_KEY))
    ),
    error = function(e) NULL
  )
  
  if (is.null(res)) return(invisible(FALSE))
  if (httr::status_code(res) >= 400) return(invisible(FALSE))
  invisible(TRUE)
}


# =============================================================================
# DATABASE HELPERS
# -----------------------------------------------------------------------------
# Every helper opens its own short-lived connection and disconnects on exit.
# This is intentional for shinyapps.io: long-lived pool connections can become
# stale across the platform's idle timeout, so we trade a small per-call cost
# for reliability.
#
# Tables:
#   paintings    -- canonical painting records (linked to museums.id)
#   submissions  -- pending/approved community uploads (legacy museum_*
#                   columns retained as proposal staging)
#   museums      -- canonical museum records (deduped by case-insensitive name)
# =============================================================================

# Open a Postgres connection to Supabase via the connection pooler.
# `gssencmode = "disable"` is required: Supabase's pooler does not accept
# GSSAPI negotiation and shinyapps.io's environment otherwise stalls the
# initial handshake.
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


# -----------------------------------------------------------------------------
# Submissions: load / insert / update status / delete
# -----------------------------------------------------------------------------

# Load all submissions joined to their museum (when linked).
# Returns a data.frame with the canonical column shape used everywhere
# in the app -- and importantly, an empty data.frame WITH that exact shape
# when the table is empty, so callers can rbind() new rows without crashing.
db_load_submissions <- function() {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  # The submissions table still carries legacy museum_* columns to hold
  # PENDING proposals (an "add_museum" submission proposes a museum that
  # may not exist yet, and a "user_painting" submission with museum info
  # carries the proposed museum data until approval).  Once a submission
  # is approved, museum data flows into the museums table and the
  # submission's museum_id is set.  COALESCE picks the canonical value
  # from museums when linked, falling back to the proposed columns.
  result <- dbGetQuery(con, "
    SELECT
      s.submission_id,
      s.name,
      s.email,
      s.painting_id,
      s.photo_url,
      s.latitude,
      s.longitude,
      s.observations,
      s.submission_date,
      s.approval_status,
      s.submission_type,
      s.painting_title,
      s.artist_name,
      s.painting_year,
      s.painting_context,
      s.state,
      s.region,
      s.location_notes,
      s.museum_id,
      COALESCE(m.name,      s.proposed_museum_name)       AS museum_name,
      COALESCE(m.latitude,  s.proposed_museum_latitude)   AS museum_latitude,
      COALESCE(m.longitude, s.proposed_museum_longitude)  AS museum_longitude,
      COALESCE(m.image_url, s.proposed_museum_image_url)  AS museum_image_url
    FROM submissions s
    LEFT JOIN museums m ON s.museum_id = m.id
    ORDER BY s.submission_date DESC
  ")
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
      museum_id = integer(),
      museum_name = character(),
      museum_latitude = numeric(),
      museum_longitude = numeric(),
      museum_image_url = character(),
      stringsAsFactors = FALSE
    ))
  }
  result
}

# Insert a single submission row (one record from the public submission form).
# Caller is responsible for assembling `sub_df` with the expected columns;
# see db_load_submissions() for the canonical column list.
db_insert_submission <- function(sub_df) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
            "INSERT INTO submissions (submission_id, name, email, painting_id, photo_url,
     latitude, longitude, observations, submission_date, approval_status,
     submission_type, painting_title, artist_name, painting_year, painting_context,
     state, region, location_notes,
     proposed_museum_name, proposed_museum_latitude, proposed_museum_longitude, proposed_museum_image_url)
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

# Update a submission's approval_status (e.g. "Approved" / "Rejected").
db_update_status <- function(submission_id, new_status) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
            "UPDATE submissions SET approval_status = $1 WHERE submission_id = $2",
            params = list(new_status, submission_id)
  )
}

# Delete a submission row AND its associated photo from Supabase Storage.
# We capture photo_url before the DELETE so we can clean up the storage file
# afterward; failure to clean up storage will not block the row deletion
# (delete_from_storage() never throws).
db_delete_submission <- function(submission_id) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  
  # Get photo_url first so we can clean up storage after deleting the row.
  url_row <- dbGetQuery(con,
                        "SELECT photo_url FROM submissions WHERE submission_id = $1",
                        params = list(submission_id))
  
  dbExecute(con,
            "DELETE FROM submissions WHERE submission_id = $1",
            params = list(submission_id)
  )
  
  if (nrow(url_row) > 0) {
    delete_from_storage(url_row$photo_url[1])
  }
}


# -----------------------------------------------------------------------------
# Paintings & museums: delete / promote / link / load
# -----------------------------------------------------------------------------

# Delete a painting and its associated storage file (if any).
# Also unlinks any submissions that pointed to this painting (sets
# painting_id = NULL) so they aren't left referring to a missing row.
db_delete_painting <- function(painting_id) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  
  # Capture the image URL before deleting.
  url_row <- dbGetQuery(con,
                        "SELECT image_url FROM paintings WHERE id = $1",
                        params = list(as.integer(painting_id)))
  
  # Unlink any submissions referencing this painting so they don't
  # break (foreign key would prevent the delete otherwise if FK is set).
  dbExecute(con,
            "UPDATE submissions SET painting_id = NULL WHERE painting_id = $1",
            params = list(as.integer(painting_id))
  )
  
  dbExecute(con,
            "DELETE FROM paintings WHERE id = $1",
            params = list(as.integer(painting_id))
  )
  
  if (nrow(url_row) > 0) {
    delete_from_storage(url_row$image_url[1])
  }
}

# Find an existing museum by case-insensitive trimmed name, or create one.
# If creating, uses the provided lat/lng/image as initial values.
# If the museum already exists with NULL coords/image and the caller is
# providing values, we BACKFILL those fields rather than overwriting.
# Returns the museum id, or NA_integer_ if name is blank/missing.
db_find_or_create_museum <- function(con, name, lat = NA_real_, lng = NA_real_, image_url = NA_character_) {
  if (is.null(name) || is.na(name) || trimws(as.character(name)) == "") {
    return(NA_integer_)
  }
  canonical_name <- trimws(as.character(name))
  
  existing <- dbGetQuery(con,
                         "SELECT id, latitude, longitude, image_url
                          FROM museums
                          WHERE LOWER(TRIM(name)) = LOWER($1)",
                         params = list(canonical_name))
  
  if (nrow(existing) > 0) {
    mid <- as.integer(existing$id[1])
    
    # Backfill any NULL fields on the existing museum, but never overwrite.
    set_clauses <- c()
    set_params  <- list()
    
    if (is.na(existing$latitude[1]) && !is.na(lat)) {
      set_clauses <- c(set_clauses, sprintf("latitude = $%d",  length(set_params) + 1))
      set_params  <- c(set_params, list(as.numeric(lat)))
    }
    if (is.na(existing$longitude[1]) && !is.na(lng)) {
      set_clauses <- c(set_clauses, sprintf("longitude = $%d", length(set_params) + 1))
      set_params  <- c(set_params, list(as.numeric(lng)))
    }
    if ((is.na(existing$image_url[1]) || existing$image_url[1] == "") &&
        !is.na(image_url) && image_url != "") {
      set_clauses <- c(set_clauses, sprintf("image_url = $%d", length(set_params) + 1))
      set_params  <- c(set_params, list(as.character(image_url)))
    }
    
    if (length(set_clauses) > 0) {
      sql <- sprintf("UPDATE museums SET %s WHERE id = $%d",
                     paste(set_clauses, collapse = ", "),
                     length(set_params) + 1)
      set_params <- c(set_params, list(mid))
      dbExecute(con, sql, params = set_params)
    }
    
    return(mid)
  }
  
  # Create a new museum row.
  result <- dbGetQuery(con,
                       "INSERT INTO museums (name, latitude, longitude, image_url)
                        VALUES ($1, $2, $3, $4)
                        RETURNING id",
                       params = list(
                         canonical_name,
                         if (is.na(lat)) NA_real_ else as.numeric(lat),
                         if (is.na(lng)) NA_real_ else as.numeric(lng),
                         if (is.na(image_url) || image_url == "") NA_character_ else as.character(image_url)
                       ))
  as.integer(result$id[1])
}

# Promote an approved "user_painting" submission into a real painting row.
# This is the second half of the approval flow: status flips to "Approved"
# (db_update_status), then this copies submission fields into a new
# paintings row, resolves a museum_id (creating a museum if needed), and
# back-links the submission via submissions.painting_id.
# Returns the newly-created painting id.
db_promote_to_painting <- function(sub_row) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  
  # Resolve / create a museum row for this painting if museum info exists.
  museum_id_val <- db_find_or_create_museum(
    con,
    name      = sub_row$museum_name,
    lat       = if (is.null(sub_row$museum_latitude))  NA_real_ else sub_row$museum_latitude,
    lng       = if (is.null(sub_row$museum_longitude)) NA_real_ else sub_row$museum_longitude,
    image_url = if (is.null(sub_row$museum_image_url)) NA_character_ else sub_row$museum_image_url
  )
  
  # New paintings reference museums via museum_id only.  The legacy
  # museum_name / lat / lng / image columns are NOT populated for new rows
  # -- they're being deprecated.
  result <- dbGetQuery(con,
                       "INSERT INTO paintings (title, artist, \"year\", context, image_url,
                                               state, region, location_notes, museum_id)
                        VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
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
                         if (is.na(museum_id_val)) NA_integer_ else as.integer(museum_id_val)
                       )
  )
  new_id <- result$id[1]
  dbExecute(con,
            "UPDATE submissions SET painting_id = $1 WHERE submission_id = $2",
            params = list(new_id, as.character(sub_row$submission_id))
  )
  new_id
}

# Attach museum info to an existing painting (used when an "add_museum"
# submission is approved). Resolves or creates the museum row, then
# updates the painting's museum_id. Lat/lng/image_url backfill the museum
# row only when those fields are currently NULL there.
db_update_painting_museum <- function(painting_id, museum_name, museum_lat, museum_lng, museum_img) {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  
  # New behavior: museum data lives in the museums table.  We find-or-create
  # a museum row matching the given name, then point the painting at it.
  # Note that updating a museum's lat/lng/image now updates ALL paintings
  # at that museum, which is the whole point of the normalization.
  
  # First, fetch the painting's current museum_id (if any) so we know
  # whether we're (a) linking to a brand new museum, (b) linking to an
  # existing museum, or (c) updating the museum the painting already has.
  current <- dbGetQuery(con,
                        "SELECT museum_id FROM paintings WHERE id = $1",
                        params = list(as.integer(painting_id)))
  current_mid <- if (nrow(current) > 0 && !is.na(current$museum_id[1])) {
    as.integer(current$museum_id[1])
  } else {
    NA_integer_
  }
  
  # Resolve target museum.
  target_mid <- db_find_or_create_museum(
    con, name = museum_name, lat = museum_lat, lng = museum_lng, image_url = museum_img
  )
  
  if (is.na(target_mid)) {
    # No museum name provided -- nothing to do.  We don't unlink an
    # existing museum from the painting silently.
    return(invisible(NULL))
  }
  
  # If the painting was already linked to this exact museum, refresh its
  # coords / image (admin is editing the museum, not picking a different one).
  if (!is.na(current_mid) && current_mid == target_mid) {
    set_clauses <- c()
    set_params  <- list()
    
    if (!is.na(museum_lat)) {
      set_clauses <- c(set_clauses, sprintf("latitude = $%d", length(set_params) + 1))
      set_params  <- c(set_params, list(as.numeric(museum_lat)))
    }
    if (!is.na(museum_lng)) {
      set_clauses <- c(set_clauses, sprintf("longitude = $%d", length(set_params) + 1))
      set_params  <- c(set_params, list(as.numeric(museum_lng)))
    }
    if (!is.na(museum_img) && museum_img != "") {
      set_clauses <- c(set_clauses, sprintf("image_url = $%d", length(set_params) + 1))
      set_params  <- c(set_params, list(as.character(museum_img)))
    }
    
    if (length(set_clauses) > 0) {
      sql <- sprintf("UPDATE museums SET %s WHERE id = $%d",
                     paste(set_clauses, collapse = ", "),
                     length(set_params) + 1)
      set_params <- c(set_params, list(target_mid))
      dbExecute(con, sql, params = set_params)
    }
  }
  
  # Link the painting to the (possibly new) museum.
  dbExecute(con,
            "UPDATE paintings SET museum_id = $1 WHERE id = $2",
            params = list(as.integer(target_mid), as.integer(painting_id))
  )
}

# Load all paintings, joined to their museum row. The joined museum_*
# columns preserve the column shape the frontend (gallery, lightbox, map)
# already expects, so callers don't need to know about the museums table.
db_load_paintings <- function() {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  # Museum data lives in the museums table now.  Joined columns keep the
  # frontend's expected shape: museum_name / museum_latitude /
  # museum_longitude / museum_image_url come back from the JOIN.
  dbGetQuery(con, "
    SELECT
      p.id,
      p.title,
      p.artist,
      p.\"year\",
      p.context,
      p.image_url,
      p.state,
      p.region,
      p.location_notes,
      p.museum_id,
      m.name      AS museum_name,
      m.latitude  AS museum_latitude,
      m.longitude AS museum_longitude,
      m.image_url AS museum_image_url
    FROM paintings p
    LEFT JOIN museums m ON p.museum_id = m.id
    ORDER BY p.id
  ")
}

# Load all museums (ordered alphabetically). Used to populate the
# admin "link a museum" autocomplete and the public submission form's
# museum-name suggestions.
db_load_museums <- function() {
  con <- get_db_con()
  on.exit(dbDisconnect(con))
  dbGetQuery(con,
             "SELECT id, name, latitude, longitude, image_url
              FROM museums
              ORDER BY name")
}


# =============================================================================
# INITIAL DATA LOAD
# -----------------------------------------------------------------------------
# These are loaded once at app startup and used to seed the UI (gallery,
# map, dropdowns). Live updates inside the running app go through rv$ in
# the server function, not these globals.
# =============================================================================

paintings_data <- db_load_paintings()
museums_data   <- db_load_museums()

# Enrich paintings_data globally with approved_count and has_museum so the
# initial JS paintingsData array always has these fields populated. Without
# this, the painting detail popup shows wrong discovery / museum status
# until the per-session updatePaintingsData message arrives.
.startup_subs <- tryCatch(db_load_submissions(), error = function(e) NULL)
.startup_approved <- if (!is.null(.startup_subs) && nrow(.startup_subs) > 0) {
  .startup_subs[.startup_subs$approval_status == "Approved" &
                  (is.na(.startup_subs$submission_type) | .startup_subs$submission_type == "landscape"), ]
} else {
  data.frame(painting_id = integer(0))
}
.startup_counts <- if (nrow(.startup_approved) > 0) {
  as.data.frame(table(.startup_approved$painting_id), stringsAsFactors = FALSE)
} else {
  data.frame(Var1 = character(0), Freq = integer(0))
}
paintings_data$approved_count <- sapply(paintings_data$id, function(id) {
  m <- .startup_counts[.startup_counts$Var1 == as.character(id), "Freq"]
  if (length(m) > 0) m[1] else 0
})
paintings_data$has_museum <- !is.na(paintings_data$museum_name) & paintings_data$museum_name != ""
rm(.startup_subs, .startup_approved, .startup_counts)

# State center coordinates used by the "fly to state" map control.
# R's built-in state.center places Alaska/Hawaii at inset-map positions
# rather than their real geographic coordinates, so we patch those manually.
state_centers <- data.frame(
  state = state.name,
  lat   = state.center$y,
  lng   = state.center$x,
  stringsAsFactors = FALSE
)
state_centers[state_centers$state == "Alaska", c("lat", "lng")] <- c(64.2008, -152.4937)
state_centers[state_centers$state == "Hawaii", c("lat", "lng")] <- c(20.7984, -156.3319)


# =============================================================================
# CUSTOM CSS
# -----------------------------------------------------------------------------
# All app-specific styling now lives in www/styles.css and is loaded by Shiny
# via tags$link() in the UI <head>. Shiny automatically serves files from the
# www/ directory at the root URL of the app.
# =============================================================================


# =============================================================================
# UI
# -----------------------------------------------------------------------------
# Built with bslib's page_navbar(). Tabs:
#   * Gallery  -- card grid of paintings + detail lightbox
#   * Map      -- Leaflet map of community photos and museums
#   * Compare  -- side-by-side historical / modern view
#   * Contribute  -- public submission landing + forms
#   * Admin    -- password-gated review console
# =============================================================================

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
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
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
                        condition = "input.submit_type === 'landscape' || input.submit_type === 'user_painting'",
                        tags$div(class = "form-group",
                                 selectInput("submit_state", "State",
                                             choices = c("Select a state..." = "", sort(state.name), "Unknown" = "Unknown"),
                                             selected = "")
                        )
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
                                          # Type-to-search picker.  Existing museums (case-insensitive)
                                          # auto-suggest as the user types; if the typed text doesn't
                                          # match any existing museum, selectize lets them create it
                                          # as a new option (create = TRUE).
                                          selectizeInput(
                                            "submit_museum_name",
                                            "Museum / Collection Name",
                                            choices  = c("", sort(museums_data$name)),
                                            selected = "",
                                            options  = list(
                                              create     = TRUE,
                                              createOnBlur = TRUE,
                                              placeholder = "Start typing to search or add new..."
                                            )
                                          )
                                 ),
                                 tags$div(id = "museum_existing_hint",
                                          style = "display: none; color: var(--text-muted); font-size: 12px; margin-top: -8px; margin-bottom: 12px;",
                                          HTML("&#10003; Using existing museum (coordinates auto-filled).")),
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
               navset_pill(
                 id = "admin_tabs",
                 nav_panel(
                   "Submissions",
                   tags$div(class = "admin-toolbar",
                            actionButton("refresh_admin", "Refresh", class = "btn btn-refresh"),
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
                   DTOutput("admin_submissions_table")
                 ),
                 nav_panel(
                   "Paintings Gallery",
                   tags$div(class = "admin-toolbar",
                            actionButton("refresh_paintings_admin", "Refresh", class = "btn btn-refresh"),
                            tags$div(class = "artist-filter-wrap",
                                     textInput("admin_paintings_search", NULL,
                                               placeholder = "Search by title or artist...",
                                               width = "240px")
                            )
                   ),
                   DTOutput("admin_paintings_table")
                 )
               )
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
             # Subtle drifting clouds across the top of the sky
             tags$div(class = "splash-cloud-layer",
                      HTML('<svg viewBox="0 0 1200 400" preserveAspectRatio="xMidYMin slice" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
                        <defs>
                          <radialGradient id="splashCloudGrad" cx="50%" cy="50%" r="50%">
                            <stop offset="0%" stop-color="#ffffff" stop-opacity="0.85"/>
                            <stop offset="60%" stop-color="#ffffff" stop-opacity="0.55"/>
                            <stop offset="100%" stop-color="#ffffff" stop-opacity="0"/>
                          </radialGradient>
                        </defs>
                        <!-- Cloud 1 (large, slow) -->
                        <g class="splash-cloud splash-cloud-1">
                          <ellipse cx="0" cy="80" rx="140" ry="32" fill="url(#splashCloudGrad)"/>
                          <ellipse cx="-40" cy="70" rx="80" ry="22" fill="url(#splashCloudGrad)"/>
                          <ellipse cx="50" cy="72" rx="70" ry="20" fill="url(#splashCloudGrad)"/>
                        </g>
                        <!-- Cloud 2 (medium, slowest, lower) -->
                        <g class="splash-cloud splash-cloud-2">
                          <ellipse cx="0" cy="160" rx="110" ry="26" fill="url(#splashCloudGrad)"/>
                          <ellipse cx="-30" cy="152" rx="60" ry="18" fill="url(#splashCloudGrad)"/>
                          <ellipse cx="35" cy="154" rx="55" ry="16" fill="url(#splashCloudGrad)"/>
                        </g>
                        <!-- Cloud 3 (small, faster) -->
                        <g class="splash-cloud splash-cloud-3">
                          <ellipse cx="0" cy="50" rx="80" ry="20" fill="url(#splashCloudGrad)"/>
                          <ellipse cx="-25" cy="44" rx="45" ry="14" fill="url(#splashCloudGrad)"/>
                          <ellipse cx="30" cy="46" rx="40" ry="12" fill="url(#splashCloudGrad)"/>
                        </g>
                        <!-- Cloud 4 (medium, mid-height) -->
                        <g class="splash-cloud splash-cloud-4">
                          <ellipse cx="0" cy="220" rx="100" ry="24" fill="url(#splashCloudGrad)"/>
                          <ellipse cx="-30" cy="212" rx="55" ry="16" fill="url(#splashCloudGrad)"/>
                          <ellipse cx="32" cy="214" rx="50" ry="15" fill="url(#splashCloudGrad)"/>
                        </g>
                      </svg>')
             ),
             # Foreground pine silhouettes — inline SVG, ~1KB, no extra request
             tags$div(class = "splash-tree-line",
                      HTML('<svg viewBox="0 0 1200 220" preserveAspectRatio="xMidYMax slice" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
                        <defs>
                          <linearGradient id="splashTreeGrad" x1="0" y1="0" x2="0" y2="1">
                            <stop offset="0%" stop-color="#5a8c4a" stop-opacity="0.85"/>
                            <stop offset="100%" stop-color="#2d5530" stop-opacity="0.95"/>
                          </linearGradient>
                          <linearGradient id="splashRidgeGrad" x1="0" y1="0" x2="0" y2="1">
                            <stop offset="0%" stop-color="#7fa372" stop-opacity="0.45"/>
                            <stop offset="100%" stop-color="#5a8060" stop-opacity="0.55"/>
                          </linearGradient>
                        </defs>
                        <!-- Distant ridge line for depth -->
                        <path d="M0,180 L60,160 L130,170 L210,150 L290,165 L370,145 L450,160 L540,150 L630,165 L720,148 L810,160 L900,150 L990,165 L1080,155 L1140,170 L1200,160 L1200,220 L0,220 Z"
                              fill="url(#splashRidgeGrad)"/>
                        <!-- Tree 1 -->
                        <path class="splash-tree splash-tree-1"
                              d="M90,220 L90,140 L75,150 L88,120 L72,128 L90,90 L108,128 L92,120 L105,150 L90,140 Z"
                              fill="url(#splashTreeGrad)"/>
                        <!-- Tree 2 (taller) -->
                        <path class="splash-tree splash-tree-2"
                              d="M220,220 L220,120 L200,135 L218,95 L196,108 L220,55 L244,108 L222,95 L240,135 L220,120 Z"
                              fill="url(#splashTreeGrad)"/>
                        <!-- Tree 3 (small) -->
                        <path class="splash-tree splash-tree-3"
                              d="M340,220 L340,160 L328,168 L338,140 L326,148 L340,115 L354,148 L342,140 L352,168 L340,160 Z"
                              fill="url(#splashTreeGrad)"/>
                        <!-- Tree 4 (tallest, hero) -->
                        <path class="splash-tree splash-tree-4"
                              d="M470,220 L470,100 L448,118 L468,75 L444,90 L470,30 L496,90 L472,75 L492,118 L470,100 Z"
                              fill="url(#splashTreeGrad)"/>
                        <!-- Tree 5 -->
                        <path class="splash-tree splash-tree-5"
                              d="M620,220 L620,135 L604,148 L618,112 L600,122 L620,80 L640,122 L622,112 L636,148 L620,135 Z"
                              fill="url(#splashTreeGrad)"/>
                        <!-- Tree 6 (taller) -->
                        <path class="splash-tree splash-tree-6"
                              d="M790,220 L790,108 L770,124 L788,85 L766,98 L790,45 L814,98 L792,85 L810,124 L790,108 Z"
                              fill="url(#splashTreeGrad)"/>
                        <!-- Tree 7 -->
                        <path class="splash-tree splash-tree-7"
                              d="M950,220 L950,150 L936,160 L948,130 L934,140 L950,105 L966,140 L952,130 L964,160 L950,150 Z"
                              fill="url(#splashTreeGrad)"/>
                        <!-- Tree 8 (back, small filler) -->
                        <path class="splash-tree splash-tree-3"
                              d="M1080,220 L1080,140 L1066,150 L1078,122 L1064,130 L1080,98 L1096,130 L1082,122 L1094,150 L1080,140 Z"
                              fill="url(#splashTreeGrad)"/>
                        <!-- Tree 9 (back, edge) -->
                        <path class="splash-tree splash-tree-5"
                              d="M1170,220 L1170,160 L1158,170 L1168,142 L1156,150 L1170,118 L1184,150 L1172,142 L1182,170 L1170,160 Z"
                              fill="url(#splashTreeGrad)"/>
                      </svg>')
             ),
             tags$div(class = "splash-inner",
                      tags$h1(class = "splash-title", HTML("Landscape<br>Through <span>Time</span>")),
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

      // -- ADMIN DARK MODE (sticky; immune to inner tab flicker) ----------
      // Track which top-level navbar tab is currently active so inner
      // tab changes (e.g. Submissions <-> Paintings Gallery pills inside
      // Admin) cannot cause the theme to flip back to light.
      window.__currentTopTab = null;

      function applyAdminTheme(tabVal) {
        if (!tabVal) return;
        window.__currentTopTab = tabVal;
        if (tabVal === 'Admin Login') {
          // Admin: force dark-only.  Remove light-mode and DON'T let
          // anything else add it back while admin is active.
          document.body.classList.remove('light-mode');
          document.body.classList.add('admin-dark-locked');
        } else {
          document.body.classList.remove('admin-dark-locked');
          if (!document.body.classList.contains('light-mode')) {
            document.body.classList.add('light-mode');
          }
        }
      }

      // Server-driven theme toggle (authoritative; only fires on
      // top-level navbar changes via input$main_tabs).
      Shiny.addCustomMessageHandler('setTheme', function(tab) {
        applyAdminTheme(tab);
      });

      // Fallback: listen to Bootstrap tab events, but ONLY for top-level
      // navbar tabs.  Inner pill tabs (admin's Submissions / Paintings
      // Gallery) must not be allowed to touch the theme.
      $(document).on('shown.bs.tab', function(e) {
        if (!e.target) return;
        // The top-level navbar tabs live inside the page_navbar's nav.
        // Inner navset_pill tabs are NOT children of that container, so
        // we only act on events whose target is a top-level nav link.
        var inTopNav = e.target.closest('.navbar-nav, nav.navbar');
        if (!inTopNav) return;
        var tabVal = e.target.getAttribute('data-value');
        if (tabVal) applyAdminTheme(tabVal);
      });

      // Belt and suspenders: any time the admin-dark-locked class is
      // present, a MutationObserver immediately strips light-mode if
      // anything tries to add it.  This catches edge cases like
      // re-renders or third-party JS toggling classes.
      (function() {
        var bodyObserver = new MutationObserver(function(mutations) {
          if (document.body.classList.contains('admin-dark-locked') &&
              document.body.classList.contains('light-mode')) {
            document.body.classList.remove('light-mode');
          }
        });
        bodyObserver.observe(document.body, {
          attributes: true,
          attributeFilter: ['class']
        });
      })();

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
        var locText = locParts.join(' &mdash; ');

        var isDiscovered = (p.approved_count || 0) > 0;
        var approvedCount = p.approved_count || 0;
        var hasMuseum = p.has_museum === true;
        var museumName = p.museum_name || '';
        var isPrivate = museumName && /private collection/i.test(museumName);
        var hasMuseumCoords = p.museum_latitude != null && !isNaN(parseFloat(p.museum_latitude)) &&
                              p.museum_longitude != null && !isNaN(parseFloat(p.museum_longitude));
        var safeTitle = (p.title || '').replace(/'/g, \"\\\\'\");

        // Status chip: discovered (sage) or undiscovered (amber)
        var statusChip = isDiscovered
          ? '<div class=\"pd-status pd-status-discovered\">&#10003; Location Discovered' + (approvedCount > 0 ? ' &middot; ' + approvedCount + ' photo' + (approvedCount !== 1 ? 's' : '') : '') + '</div>'
          : '<div class=\"pd-status pd-status-undiscovered\">&#9737; Awaiting Discovery</div>';

        // ===== HEADER =====
        var html = '<div class=\"pd-hero\">' +
          '<img class=\"pd-hero-img\" src=\"' + p.image_url + '\" alt=\"' + p.title + '\">' +
          '</div>' +
          '<h2 class=\"pd-title\">' + p.title + '</h2>' +
          '<div class=\"pd-byline\">' + p.artist + (p.year ? ' &middot; ' + p.year : '') + '</div>' +
          '<div class=\"pd-status-wrap\">' + statusChip + '</div>';

        // ===== INFO CARDS — always render in this order: Location, Museum, About =====
        html += '<div class=\"pd-cards\">';

        // -- Location card (always rendered) --
        html += '<div class=\"pd-card\">' +
          '<div class=\"pd-card-label\">&#128205; Location</div>';
        if (locText) {
          html += '<div class=\"pd-card-value\">' + locText + '</div>';
          if (locationNotes) {
            html += '<div class=\"pd-card-note\">' + locationNotes + '</div>';
          }
        } else if (locationNotes) {
          html += '<div class=\"pd-card-value\">' + locationNotes + '</div>';
        } else {
          html += '<div class=\"pd-card-value pd-muted\">Location not yet documented</div>';
        }
        html += '</div>';

        // -- Museum card (always rendered, three states) --
        if (isPrivate) {
          // STATE 1: Private collection
          html += '<div class=\"pd-card\">' +
            '<div class=\"pd-card-label\">&#127963; Currently Held</div>' +
            '<div class=\"pd-card-value\">&#128274; Private Collection</div>' +
            '<div class=\"pd-card-note\">This painting is held privately and is not publicly viewable.</div>' +
            '</div>';
        } else if (hasMuseum) {
          // STATE 2: Museum on file
          html += '<div class=\"pd-card\">' +
            '<div class=\"pd-card-label\">&#127963; Currently Held At</div>' +
            '<div class=\"pd-card-value\">' + museumName + '</div>';
          if (hasMuseumCoords) {
            html += '<div class=\"pd-card-action\" onclick=\"closePaintingDetail(); Shiny.setInputValue(&#39;go_to_museum&#39;, {id: ' + p.id + ', t: Date.now()});\">View on Map &rarr;</div>';
          }
          html += '</div>';
        } else {
          // STATE 3: Needs update
          html += '<div class=\"pd-card pd-card-needs\">' +
            '<div class=\"pd-card-label\">&#127963; Currently Held</div>' +
            '<div class=\"pd-card-value\">Museum Info Needs to be Updated</div>' +
            '<div class=\"pd-card-note\">Help us complete this record by submitting where this painting is held.</div>' +
            '<div class=\"pd-card-action pd-card-action-amber\" onclick=\"openAddMuseumModal(' + p.id + ', &#39;' + safeTitle + '&#39;);\">&#43; Submit Museum Info</div>' +
            '</div>';
        }

        // -- About card (only if context exists) --
        if (p.context) {
          html += '<div class=\"pd-card\">' +
            '<div class=\"pd-card-label\">&#128214; About</div>' +
            '<div class=\"pd-card-prose\">' + p.context + '</div>' +
            '</div>';
        }

        html += '</div>'; // close .pd-cards

        // ===== PRIMARY ACTIONS =====
        html += '<div class=\"pd-actions\">';

        if (isDiscovered) {
          html += '<div class=\"pd-btn pd-btn-primary\" onclick=\"closePaintingDetail(); Shiny.setInputValue(&#39;go_compare_painting&#39;, {id: ' + p.id + ', t: Date.now()});\">' +
                  '&#128444; View Comparison' + (approvedCount > 1 ? 's' : '') + ' &rarr;</div>';
          html += '<div class=\"pd-btn pd-btn-secondary\" onclick=\"closePaintingDetail(); Shiny.setInputValue(&#39;contribute_for_painting&#39;, {id: ' + p.id + ', t: Date.now()});\">' +
                  '&#43; Add Contemporary Photo</div>';
        } else {
          html += '<div class=\"pd-btn pd-btn-primary\" onclick=\"closePaintingDetail(); Shiny.setInputValue(&#39;contribute_for_painting&#39;, {id: ' + p.id + ', t: Date.now()});\">' +
                  '&#43; Add Contemporary Photo</div>';
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

      // -- MUSEUM PICKER: auto-fill GPS for existing museums, lock fields ----
      // window.museumsData is the canonical list: [{id, name, lat, lng}, ...]
      // It's seeded from the server and refreshed whenever museums change.
      window.museumsData = ", jsonlite::toJSON(museums_data, auto_unbox = TRUE), ";

      Shiny.addCustomMessageHandler('updateMuseumsData', function(data) {
        window.museumsData = JSON.parse(data);
      });

      // Find an existing museum row by case-insensitive trimmed name match.
      function findMuseumByName(name) {
        if (!name) return null;
        var needle = String(name).trim().toLowerCase();
        if (!needle) return null;
        for (var i = 0; i < window.museumsData.length; i++) {
          if (String(window.museumsData[i].name).trim().toLowerCase() === needle) {
            return window.museumsData[i];
          }
        }
        return null;
      }

      function setMuseumGpsLocked(locked) {
        var lat = document.getElementById('submit_museum_lat');
        var lng = document.getElementById('submit_museum_lng');
        var btn = document.getElementById('use_museum_location_btn');
        var hint = document.getElementById('museum_existing_hint');
        if (lat) {
          lat.readOnly = locked;
          lat.style.opacity = locked ? '0.6' : '1';
          lat.style.cursor = locked ? 'not-allowed' : 'text';
        }
        if (lng) {
          lng.readOnly = locked;
          lng.style.opacity = locked ? '0.6' : '1';
          lng.style.cursor = locked ? 'not-allowed' : 'text';
        }
        if (btn) {
          btn.style.opacity = locked ? '0.4' : '1';
          btn.style.pointerEvents = locked ? 'none' : 'auto';
        }
        if (hint) hint.style.display = locked ? '' : 'none';
      }

      // React to selectize changes on submit_museum_name.  Shiny dispatches
      // a 'change' event on the underlying <select>, which we listen for via
      // jQuery delegation (the selectize widget might re-render the DOM).
      $(document).on('change', '#submit_museum_name', function() {
        var picked = this.value || '';
        var existing = findMuseumByName(picked);
        var lat = document.getElementById('submit_museum_lat');
        var lng = document.getElementById('submit_museum_lng');
        if (existing) {
          // Existing museum -> auto-fill and lock.
          if (existing.latitude != null && lat) {
            lat.value = existing.latitude;
            Shiny.setInputValue('submit_museum_lat', existing.latitude);
          }
          if (existing.longitude != null && lng) {
            lng.value = existing.longitude;
            Shiny.setInputValue('submit_museum_lng', existing.longitude);
          }
          setMuseumGpsLocked(true);
        } else {
          // New museum (typed-to-create) -> unlock and clear GPS.
          if (lat) { lat.value = ''; Shiny.setInputValue('submit_museum_lat', null); }
          if (lng) { lng.value = ''; Shiny.setInputValue('submit_museum_lng', null); }
          setMuseumGpsLocked(false);
          var statusEl = document.getElementById('museum_location_status');
          if (statusEl) statusEl.textContent = '';
        }
      });

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


# =============================================================================
# SERVER
# -----------------------------------------------------------------------------
# Reactive state lives in `rv` (reactiveValues). DB writes go through the
# db_* helpers; UI <-> JS messaging goes through session$sendCustomMessage()
# and Shiny.setInputValue() (see message handlers in the UI section).
# =============================================================================

server <- function(input, output, session) {
  
  # Null-coalescing operator: returns `a` unless it's NULL / empty / NA, in
  # which case it returns `b`. Used heavily to give safe defaults to inputs
  # that may not have rendered yet on first tab visit.
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
  
  # -- REACTIVE VALUES ------------------------------------------------------
  rv <- reactiveValues(
    admin_auth = FALSE,
    submission_success = FALSE,
    submission_error = NULL,
    submissions = db_load_submissions(),
    paintings_data = db_load_paintings(),
    museums = db_load_museums(),
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
        rv$museums <- db_load_museums()
        rv$approved_trigger <- rv$approved_trigger + 1
        session$sendCustomMessage("updatePaintingsData",
                                  jsonlite::toJSON(enriched_paintings(), auto_unbox = TRUE))
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
        museum_id = NA_integer_,
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
  
  # Keep the museum picker + the JS-side museumsData in sync whenever
  # the museums table changes (e.g. admin approves a new museum).
  observe({
    md <- rv$museums
    updateSelectizeInput(session, "submit_museum_name",
                         choices = c("", sort(md$name)),
                         server = FALSE)
    session$sendCustomMessage("updateMuseumsData",
                              jsonlite::toJSON(md, auto_unbox = TRUE))
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
        # Dedupe to one marker per museum (multiple paintings at the same
        # museum share a museum_id and would otherwise stack on top of
        # each other).  We group by museum_id when available, falling back
        # to museum_name for any rows that haven't been linked yet.
        museum_data$marker_group <- ifelse(
          !is.na(museum_data$museum_id),
          paste0("id:", museum_data$museum_id),
          paste0("name:", tolower(trimws(museum_data$museum_name)))
        )
        # Keep one row per group; aggregate label as "Museum Name (N paintings)".
        museum_unique <- do.call(rbind, lapply(split(museum_data, museum_data$marker_group), function(grp) {
          n <- nrow(grp)
          row <- grp[1, , drop = FALSE]
          row$marker_label <- if (n > 1) {
            paste0(row$museum_name, " (", n, " paintings)")
          } else {
            paste0(row$title, " \u2014 ", row$museum_name)
          }
          # layerId should reference the museum, not a single painting,
          # so clicking always opens the museum view containing all
          # paintings at that location.
          row$marker_layer_id <- if (!is.na(row$museum_id)) {
            paste0("museum_", row$museum_id)
          } else {
            paste0("museum_", row$id)
          }
          row
        }))
        rownames(museum_unique) <- NULL
        
        proxy %>% addCircleMarkers(
          data = museum_unique,
          lng = ~museum_longitude, lat = ~museum_latitude,
          radius = 8, color = "#DC3545", fillColor = "#E25563", fillOpacity = 0.85,
          weight = 2, stroke = TRUE, group = "museums",
          layerId = ~marker_layer_id,
          label = ~marker_label,
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
      raw_id <- as.integer(sub("museum_", "", marker_id))
      # Try: is this a museum_id?  (Phase 3 markers use museum_<MUSEUM_ID>.)
      via_museum <- rv$paintings_data[!is.na(rv$paintings_data$museum_id) &
                                        rv$paintings_data$museum_id == raw_id, ]
      if (nrow(via_museum) > 0) {
        rv$selected_marker <- raw_id  # museum_id
        rv$selected_type <- "museum"
      } else {
        # Legacy fallback: id is a painting id (paintings without
        # museum_id, e.g. unmigrated rows).  Resolve to museum_id if we
        # can, else hold onto the painting id and the panel will degrade
        # gracefully.
        p <- rv$paintings_data[rv$paintings_data$id == raw_id, ]
        if (nrow(p) > 0 && !is.na(p$museum_id[1])) {
          rv$selected_marker <- as.integer(p$museum_id[1])
        } else {
          rv$selected_marker <- raw_id  # painting id; panel will fall back
        }
        rv$selected_type <- "museum"
      }
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
    
    rv$selected_marker <- if (!is.na(p$museum_id)) as.integer(p$museum_id) else pid
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
      # rv$selected_marker is normally a museum_id (Phase 3) but may be
      # a painting id for unmigrated legacy rows.  Try museum_id first.
      paintings_here <- rv$paintings_data[!is.na(rv$paintings_data$museum_id) &
                                            rv$paintings_data$museum_id == rv$selected_marker, ]
      if (nrow(paintings_here) == 0) {
        # Fallback: treat selected_marker as a painting id.
        paintings_here <- rv$paintings_data[rv$paintings_data$id == rv$selected_marker, ]
      }
      if (nrow(paintings_here) == 0) return(NULL)
      
      # Use the first painting for shared museum metadata (name, coords, image).
      p <- paintings_here[1, ]
      n_paintings <- nrow(paintings_here)
      
      # Approved museum-photo submissions for ANY painting at this museum.
      museum_subs <- rv$submissions[rv$submissions$approval_status == "Approved" &
                                      !is.na(rv$submissions$submission_type) &
                                      rv$submissions$submission_type == "museum_photo" &
                                      rv$submissions$painting_id %in% paintings_here$id, ]
      museum_count <- nrow(museum_subs)
      
      museum_json <- if (museum_count > 0) {
        photos_arr <- lapply(1:museum_count, function(j) {
          list(url = museum_subs[j, "photo_url"], name = museum_subs[j, "name"])
        })
        gsub("'", "\\\\'", jsonlite::toJSON(photos_arr, auto_unbox = TRUE))
      } else {
        "[]"
      }
      
      # Card list of all paintings housed at this museum.  Each card is
      # clickable -- opens the painting detail.
      paintings_list <- if (n_paintings > 0) {
        tags$div(style = "margin-top: 12px;",
                 lapply(1:n_paintings, function(i) {
                   pp <- paintings_here[i, ]
                   tags$div(
                     style = "display: flex; gap: 12px; padding: 10px 0; border-bottom: 1px solid var(--glass-border-subtle); cursor: pointer;",
                     onclick = sprintf("Shiny.setInputValue('view_painting_from_museum', {id: %d, t: Date.now()});", pp$id),
                     tags$img(src = pp$image_url,
                              style = "width: 56px; height: 40px; object-fit: cover; border-radius: 6px; flex-shrink: 0;",
                              alt = pp$title),
                     tags$div(
                       tags$div(style = "font-weight: 700; font-size: 14px; color: var(--text-primary);", pp$title),
                       tags$div(style = "font-size: 12px; color: var(--text-muted);",
                                paste0(pp$artist, ifelse(!is.na(pp$year) && pp$year != "",
                                                         paste0(" \u2022 ", pp$year), "")))
                     )
                   )
                 })
        )
      } else {
        NULL
      }
      
      tagList(
        tags$div(class = "map-info-header",
                 tags$div(class = "map-info-dot museum"),
                 tags$span(class = "map-info-type-label", "Museum / Collection")
        ),
        tags$h3(class = "map-info-title", ifelse(!is.null(p$museum_name) && !is.na(p$museum_name) && p$museum_name != "", p$museum_name, "Unknown Museum")),
        tags$div(class = "map-info-meta",
                 paste0(n_paintings, " painting", ifelse(n_paintings != 1, "s", ""), " at this location")),
        tags$img(class = "map-info-image",
                 src = if (!is.null(p$museum_image_url) && !is.na(p$museum_image_url) && p$museum_image_url != "") {
                   p$museum_image_url
                 } else {
                   p$image_url
                 },
                 alt = ifelse(!is.null(p$museum_name) && !is.na(p$museum_name), p$museum_name, p$title)),
        paintings_list,
        if (museum_count > 0) {
          tags$div(class = "map-info-cta",
                   onclick = sprintf("openMuseumLightbox('%s', %s);", gsub("'", "\\\\'", p$museum_name %||% ""), museum_json),
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
      
      # Approved contemporary photo submissions geotagged to this state.
      # Excludes user-painting uploads (those aren't contemporary photos).
      state_subs <- rv$submissions[
        rv$submissions$approval_status == "Approved" &
          !is.na(rv$submissions$state) &
          rv$submissions$state == st &
          (is.na(rv$submissions$submission_type) |
             rv$submissions$submission_type != "user_painting"),
      ]
      
      # Build one card per approved contemporary photo: thumb of the
      # submitter's photo, submitter name + the painting it was matched to.
      # Click opens the side-by-side comparison lightbox.
      photo_cards <- if (nrow(state_subs) > 0) {
        lapply(1:nrow(state_subs), function(i) {
          s <- state_subs[i, ]
          
          # Resolve the painting this photo is paired with.  Guard against
          # NA painting_id (which would index every row to NA and crash).
          painting_match <- if (!is.na(s$painting_id)) {
            rv$paintings_data[!is.na(rv$paintings_data$id) & rv$paintings_data$id == s$painting_id, ]
          } else {
            rv$paintings_data[0, ]
          }
          has_match <- nrow(painting_match) > 0 &&
            !is.na(painting_match$image_url[1]) &&
            painting_match$image_url[1] != "" &&
            !is.na(s$photo_url) && s$photo_url != ""
          
          painting_title <- if (nrow(painting_match) > 0) {
            painting_match$title[1]
          } else {
            "(painting not found)"
          }
          
          # Only attach the click-through if we have both URLs to compare.
          card_attrs <- list(
            style = "display: flex; gap: 12px; padding: 12px 0; border-bottom: 1px solid var(--glass-border-subtle);"
          )
          if (has_match) {
            card_attrs$style <- paste0(card_attrs$style, " cursor: pointer;")
            # JS-escape the URLs so single quotes / backslashes in URLs don't break the onclick string.
            esc <- function(s) gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", s), fixed = FALSE)
            card_attrs$onclick <- sprintf(
              "openComparisonLightbox('%s', '%s');",
              esc(painting_match$image_url[1]),
              esc(s$photo_url)
            )
          }
          
          do.call(tags$div, c(
            card_attrs,
            list(
              tags$img(
                src = s$photo_url,
                style = "width: 60px; height: 40px; object-fit: cover; border-radius: 8px; flex-shrink: 0;",
                alt = paste0("Photo by ", s$name)
              ),
              tags$div(
                tags$div(
                  style = "font-weight: 700; font-size: 14px; color: var(--text-primary);",
                  s$name
                ),
                tags$div(
                  style = "font-size: 12px; color: var(--text-muted);",
                  painting_title
                )
              )
            )
          ))
        })
      } else {
        list(tags$p(
          style = "color: var(--text-muted); font-size: 13px; font-style: italic;",
          "No contemporary photos submitted for this state yet. Visit a location and contribute one!"
        ))
      }
      
      tagList(
        tags$div(class = "map-info-header",
                 tags$div(class = "map-info-dot painting"),
                 tags$span(class = "map-info-type-label", "State Explorer")
        ),
        tags$h3(class = "map-info-title", st),
        tags$div(style = "margin-top: 8px;", photo_cards)
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
  
  # Triggered by a JS-side shinyjs::delay() 2 seconds after a successful
  # submission, to clear the banner so it doesn't persist into the next
  # time the form is opened.
  observeEvent(input$clear_submission_success, {
    rv$submission_success <- FALSE
    rv$submission_error <- NULL
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
    if (sub_type == "landscape") {
      if (is.null(input$submit_state) || input$submit_state == "" || input$submit_state == "Unknown") {
        rv$submission_error <- "Please select the state where the photo was taken."
        return()
      }
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
      # Museum-info validation, only when the checkbox is checked.
      if (isTRUE(input$include_museum_info)) {
        m_name <- trimws(input$submit_museum_name %||% "")
        if (m_name == "") {
          rv$submission_error <- "Please pick or enter a museum name (or uncheck 'Also add museum info')."
          return()
        }
        # Determine if this is an existing museum (case-insensitive name match)
        # or a brand-new one being created.  We re-load museums fresh so the
        # check sees any museum admin approved during this session.
        live_museums <- tryCatch(db_load_museums(), error = function(e) data.frame(name = character()))
        is_new_museum <- !any(tolower(trimws(live_museums$name)) == tolower(m_name))
        if (is_new_museum) {
          if (is.null(input$submit_museum_lat) || is.na(input$submit_museum_lat) ||
              is.null(input$submit_museum_lng) || is.na(input$submit_museum_lng)) {
            rv$submission_error <- "New museum: please enter GPS coordinates (use the location button or type them)."
            return()
          }
        }
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
        state = if (sub_type %in% c("landscape", "user_painting")) input$submit_state else NA_character_,
        region = if (sub_type == "user_painting") trimws(input$submit_region) else NA_character_,
        location_notes = if (sub_type == "user_painting") trimws(input$submit_location_notes) else NA_character_,
        museum_id = NA_integer_,
        museum_name = if (include_museum) trimws(input$submit_museum_name %||% "") else NA_character_,
        museum_latitude = if (include_museum && !is.null(input$submit_museum_lat) && !is.na(input$submit_museum_lat)) input$submit_museum_lat else NA_real_,
        museum_longitude = if (include_museum && !is.null(input$submit_museum_lng) && !is.na(input$submit_museum_lng)) input$submit_museum_lng else NA_real_,
        museum_image_url = NA_character_,
        stringsAsFactors = FALSE
      )
      
      # Defensive: rbind() in base R is strict about column counts AND
      # names matching exactly. If rv$submissions and new_submission
      # disagree, log both column sets to stderr so the mismatch can be
      # diagnosed from the shinyapps.io logs before crashing.
      if (length(rv$submissions) > 0 && !identical(names(rv$submissions), names(new_submission))) {
        message("[submission] rbind shape mismatch.")
        message(sprintf("  rv$submissions cols (%d): %s",
                        length(rv$submissions),
                        paste(names(rv$submissions), collapse = ", ")))
        message(sprintf("  new_submission cols (%d): %s",
                        length(new_submission),
                        paste(names(new_submission), collapse = ", ")))
        only_in_rv  <- setdiff(names(rv$submissions), names(new_submission))
        only_in_new <- setdiff(names(new_submission), names(rv$submissions))
        if (length(only_in_rv) > 0) {
          message("  Only in rv$submissions: ", paste(only_in_rv, collapse = ", "))
        }
        if (length(only_in_new) > 0) {
          message("  Only in new_submission: ", paste(only_in_new, collapse = ", "))
        }
        
        # Harmonize column sets so we don't crash: add any missing
        # columns to each side as NA, then re-order to match.
        for (col in only_in_rv)  new_submission[[col]] <- NA
        for (col in only_in_new) rv$submissions[[col]] <- NA
        # Order new_submission to match rv$submissions (the canonical shape).
        new_submission <- new_submission[, names(rv$submissions), drop = FALSE]
      }
      
      rv$submissions <- rbind(rv$submissions, new_submission)
      db_insert_submission(new_submission)
      rv$submission_success <- TRUE
      
      # After 2 seconds: bounce back to landing AND clear the success
      # banner.  Both fire in the same JS delay so they happen together.
      shinyjs::delay(2000, shinyjs::runjs(
        "showContributeLanding();
         Shiny.setInputValue('clear_submission_success', Date.now());"
      ))
      
      # Reset ALL contribute-form fields so stale values don't appear if
      # the user opens the form again.  Uses Shiny's update* functions
      # for fields whose ids/values Shiny manages, plus a small JS pass
      # for the file input and the include-museum checkbox-driven inputs
      # (those need DOM manipulation since updateFileInput doesn't exist).
      
      # Common fields
      updateTextInput(session, "submit_name", value = "")
      updateTextInput(session, "submit_email", value = "")
      updateTextAreaInput(session, "submit_observations", value = "")
      
      # Landscape / museum_photo specific
      updateSelectInput(session, "submit_painting", selected = "")
      updateNumericInput(session, "submit_latitude",  value = NA)
      updateNumericInput(session, "submit_longitude", value = NA)
      
      # State (used by both landscape and user_painting)
      updateSelectInput(session, "submit_state", selected = "")
      
      # User-painting specific
      updateTextInput(session, "submit_painting_title",   value = "")
      updateTextInput(session, "submit_artist_name",      value = "")
      updateTextInput(session, "submit_painting_year",    value = "")
      updateTextInput(session, "submit_painting_context", value = "")
      updateTextInput(session, "submit_region",           value = "")
      updateTextInput(session, "submit_location_notes",   value = "")
      
      # User-painting museum-info subform
      updateCheckboxInput(session, "include_museum_info", value = FALSE)
      updateSelectizeInput(session, "submit_museum_name", selected = "")
      updateNumericInput(session, "submit_museum_lat",    value = NA)
      updateNumericInput(session, "submit_museum_lng",    value = NA)
      
      # Reset the file input cleanly using shinyjs's purpose-built reset
      # (the input has data-shinyjs-resettable attributes for this).
      shinyjs::reset("submit_photo")
      
      # shinyjs::reset() handles the input itself but does NOT clear:
      #   1. the readonly text field showing the filename
      #   2. the "Upload complete" progress bar (Shiny leaves it visible)
      # Both have to be hidden via JS.
      shinyjs::runjs("
        try {
          // Hide the 'Upload complete' progress bar.
          var progress = document.getElementById('submit_photo_progress');
          if (progress) {
            progress.style.visibility = 'hidden';
            var bar = progress.querySelector('.progress-bar');
            if (bar) {
              bar.style.width = '0%';
              bar.textContent = '';
            }
          }

          // Clear the filename text shown next to the Browse button.
          // It's the readonly text input inside the .input-group of the
          // submit_photo wrapper.
          var fi = document.getElementById('submit_photo');
          if (fi) {
            var wrapper = fi.closest('.shiny-input-container');
            if (wrapper) {
              var filenameField = wrapper.querySelector('input[type=\"text\"][readonly]');
              if (filenameField) filenameField.value = '';
            }
          }

          // Clear the geolocation status text below the GPS inputs.
          var ls = document.getElementById('location_status');
          if (ls) ls.textContent = '';
        } catch (e) { /* no-op */ }
      ")
      
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
  
  # Helper: builds an HTML button cell that fires a Shiny input on click.
  # We use the {action, id, t: Date.now()} payload so that re-clicking the
  # same row's same action still re-fires the observer.
  admin_action_btn <- function(label, action, id, style = "") {
    sprintf(
      paste0(
        '<button class="admin-row-btn admin-row-btn-%s" ',
        'style="margin-right:4px; padding:4px 10px; font-size:12px; ',
        'border:none; border-radius:6px; cursor:pointer; %s" ',
        'onclick="Shiny.setInputValue(\'%s\', {id: \'%s\', t: Date.now()});">%s</button>'
      ),
      action, style, action, id, label
    )
  }
  
  # =========================================================================
  # SUBMISSIONS TAB
  # =========================================================================
  output$admin_submissions_table <- renderDT({
    filtered <- admin_filtered()
    
    if (nrow(filtered) == 0) {
      return(datatable(data.frame(Message = "No submissions match the current filters."),
                       rownames = FALSE, escape = FALSE,
                       options = list(dom = 't')))
    }
    
    display <- data.frame(
      Date         = filtered$submission_date,
      Submitter    = filtered$name,
      Type         = filtered$submission_type,
      Painting     = sapply(filtered$painting_id, function(pid) {
        if (is.na(pid)) return("(user painting)")
        match_row <- rv$paintings_data[rv$paintings_data$id == pid, ]
        if (nrow(match_row) > 0) match_row$title[1] else as.character(pid)
      }),
      Status       = filtered$approval_status,
      Photo        = ifelse(
        is.na(filtered$photo_url) | filtered$photo_url == "", "",
        sprintf('<img src="%s" style="height:42px; width:42px; object-fit:cover; border-radius:4px;" />',
                filtered$photo_url)
      ),
      Actions      = mapply(function(sid, status) {
        approve_btn <- if (status != "Approved") {
          admin_action_btn("Approve", "admin_approve_sub", sid,
                           "background:#28a745; color:white;")
        } else ""
        reject_btn <- if (status != "Rejected") {
          admin_action_btn("Reject", "admin_reject_sub", sid,
                           "background:#FFC107; color:#222;")
        } else ""
        delete_btn <- admin_action_btn("Delete", "admin_delete_sub", sid,
                                       "background:#DC3545; color:white;")
        paste(approve_btn, reject_btn, delete_btn)
      }, filtered$submission_id, filtered$approval_status),
      stringsAsFactors = FALSE
    )
    
    datatable(
      display,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      options = list(
        pageLength = 25,
        order = list(list(0, 'desc')),
        columnDefs = list(list(orderable = FALSE, targets = c(5, 6)))
      )
    )
  })
  
  # =========================================================================
  # PAINTINGS GALLERY TAB
  # =========================================================================
  paintings_admin_filtered <- reactive({
    input$refresh_paintings_admin
    p <- rv$paintings_data
    if (nrow(p) == 0) return(p)
    
    q <- input$admin_paintings_search
    if (!is.null(q) && nzchar(trimws(q))) {
      needle <- tolower(trimws(q))
      keep <- grepl(needle, tolower(p$title %||% ""), fixed = TRUE) |
        grepl(needle, tolower(p$artist %||% ""), fixed = TRUE)
      p <- p[keep, ]
    }
    p
  })
  
  output$admin_paintings_table <- renderDT({
    filtered <- paintings_admin_filtered()
    
    if (nrow(filtered) == 0) {
      return(datatable(data.frame(Message = "No paintings match the current filter."),
                       rownames = FALSE, escape = FALSE,
                       options = list(dom = 't')))
    }
    
    display <- data.frame(
      ID      = filtered$id,
      Image   = ifelse(
        is.na(filtered$image_url) | filtered$image_url == "", "",
        sprintf('<img src="%s" style="height:42px; width:60px; object-fit:cover; border-radius:4px;" />',
                filtered$image_url)
      ),
      Title   = filtered$title,
      Artist  = filtered$artist,
      Year    = filtered$year,
      State   = ifelse(is.na(filtered$state), "", filtered$state),
      Museum  = ifelse(is.na(filtered$museum_name), "(none)", filtered$museum_name),
      Actions = sapply(filtered$id, function(pid) {
        admin_action_btn("Delete", "admin_delete_painting", pid,
                         "background:#DC3545; color:white;")
      }),
      stringsAsFactors = FALSE
    )
    
    datatable(
      display,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      options = list(
        pageLength = 25,
        order = list(list(0, 'asc')),
        columnDefs = list(list(orderable = FALSE, targets = c(1, 7)))
      )
    )
  })
  
  
  # =========================================================================
  # ROW-BUTTON HANDLERS (Submissions)
  # =========================================================================
  
  # -- APPROVE SUBMISSION ---------------------------------------------------
  observeEvent(input$admin_approve_sub, {
    val <- input$admin_approve_sub
    if (is.null(val) || is.null(val$id)) return()
    sid <- as.character(val$id)
    
    sub_row <- rv$submissions[rv$submissions$submission_id == sid, ]
    if (nrow(sub_row) == 0) return()
    sub_row <- sub_row[1, ]
    
    db_update_status(sid, "Approved")
    rv$submissions[rv$submissions$submission_id == sid, "approval_status"] <- "Approved"
    
    if (!is.na(sub_row$submission_type) && sub_row$submission_type == "user_painting") {
      tryCatch({
        new_pid <- db_promote_to_painting(sub_row)
        rv$submissions[rv$submissions$submission_id == sid, "painting_id"] <- new_pid
        rv$paintings_data <- db_load_paintings()
        rv$museums <- db_load_museums()
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
          museum_lat  = sub_row$museum_latitude,
          museum_lng  = sub_row$museum_longitude,
          museum_img  = NA_character_
        )
        rv$paintings_data <- db_load_paintings()
        rv$museums <- db_load_museums()
        session$sendCustomMessage("updatePaintingsData",
                                  jsonlite::toJSON(enriched_paintings(), auto_unbox = TRUE))
        showNotification("Museum info added to painting.", type = "message")
      }, error = function(e) {
        showNotification(paste0("Approved but failed to update museum: ", e$message), type = "error", duration = 10)
      })
    }
    
    rv$approved_trigger <- rv$approved_trigger + 1
    showNotification("Approved!", type = "message")
  })
  
  # -- REJECT SUBMISSION ----------------------------------------------------
  observeEvent(input$admin_reject_sub, {
    val <- input$admin_reject_sub
    if (is.null(val) || is.null(val$id)) return()
    sid <- as.character(val$id)
    
    db_update_status(sid, "Rejected")
    rv$submissions[rv$submissions$submission_id == sid, "approval_status"] <- "Rejected"
    showNotification("Rejected.", type = "warning")
  })
  
  # -- DELETE SUBMISSION (with confirmation) --------------------------------
  observeEvent(input$admin_delete_sub, {
    val <- input$admin_delete_sub
    if (is.null(val) || is.null(val$id)) return()
    sid <- as.character(val$id)
    
    sub_row <- rv$submissions[rv$submissions$submission_id == sid, ]
    if (nrow(sub_row) == 0) return()
    
    submitter <- sub_row$name[1]
    sub_type  <- sub_row$submission_type[1]
    
    showModal(modalDialog(
      title = "Delete submission?",
      tags$p(sprintf(
        "Delete %s submission from %s? This will remove the database record and the photo file from storage. This cannot be undone.",
        ifelse(is.na(sub_type), "this", sub_type), submitter
      )),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_sub", "Delete",
                     style = "background:#DC3545; color:white; border:none;")
      ),
      easyClose = TRUE
    ))
    
    rv$pending_delete_sub <- sid
  })
  
  observeEvent(input$confirm_delete_sub, {
    sid <- rv$pending_delete_sub
    if (is.null(sid) || is.na(sid) || sid == "") {
      removeModal()
      return()
    }
    
    tryCatch({
      db_delete_submission(sid)
      rv$submissions <- rv$submissions[rv$submissions$submission_id != sid, ]
      rv$approved_trigger <- rv$approved_trigger + 1
      showNotification("Submission deleted (record + storage file).", type = "warning")
    }, error = function(e) {
      showNotification(paste0("Delete failed: ", e$message), type = "error", duration = 10)
    })
    
    rv$pending_delete_sub <- NULL
    removeModal()
  })
  
  
  # =========================================================================
  # ROW-BUTTON HANDLERS (Paintings)
  # =========================================================================
  
  # -- DELETE PAINTING (with confirmation) ----------------------------------
  observeEvent(input$admin_delete_painting, {
    val <- input$admin_delete_painting
    if (is.null(val) || is.null(val$id)) return()
    pid <- as.integer(val$id)
    
    p_row <- rv$paintings_data[rv$paintings_data$id == pid, ]
    if (nrow(p_row) == 0) return()
    
    title  <- p_row$title[1]
    artist <- p_row$artist[1]
    in_storage <- !is.na(p_row$image_url[1]) &&
      startsWith(p_row$image_url[1],
                 paste0(SUPABASE_URL, "/storage/v1/object/public/"))
    
    showModal(modalDialog(
      title = "Delete painting?",
      tags$p(sprintf("Delete \"%s\" by %s? This will remove it from the gallery and the map.",
                     title, artist)),
      tags$p(if (in_storage) {
        "The image file will also be deleted from storage."
      } else {
        "The image is hosted externally (Wikipedia/museum) so no storage file will be deleted."
      },
      style = "color: var(--text-muted); font-size: 13px;"),
      tags$p("This cannot be undone.",
             style = "color: #DC3545; font-weight: 600; margin-top: 8px;"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_painting", "Delete",
                     style = "background:#DC3545; color:white; border:none;")
      ),
      easyClose = TRUE
    ))
    
    rv$pending_delete_painting <- pid
  })
  
  observeEvent(input$confirm_delete_painting, {
    pid <- rv$pending_delete_painting
    if (is.null(pid) || is.na(pid)) {
      removeModal()
      return()
    }
    
    tryCatch({
      db_delete_painting(pid)
      rv$paintings_data <- db_load_paintings()
      rv$museums <- db_load_museums()
      # Submissions that referenced this painting now have NULL painting_id;
      # refresh the local copy so the UI doesn't show stale links.
      rv$submissions <- db_load_submissions()
      rv$approved_trigger <- rv$approved_trigger + 1
      session$sendCustomMessage("updatePaintingsData",
                                jsonlite::toJSON(enriched_paintings(), auto_unbox = TRUE))
      showNotification("Painting deleted (record + storage file if applicable).", type = "warning")
    }, error = function(e) {
      showNotification(paste0("Delete failed: ", e$message), type = "error", duration = 10)
    })
    
    rv$pending_delete_painting <- NULL
    removeModal()
  })
  
  
  # =========================================================================
  # REFRESH HANDLERS
  # =========================================================================
  observeEvent(input$refresh_admin, {
    rv$submissions <- db_load_submissions()
    rv$paintings_data <- db_load_paintings()
    rv$museums <- db_load_museums()
    rv$approved_trigger <- rv$approved_trigger + 1
    session$sendCustomMessage("updatePaintingsData",
                              jsonlite::toJSON(enriched_paintings(), auto_unbox = TRUE))
    showNotification("Data refreshed from database!", type = "message")
  })
  
  observeEvent(input$refresh_paintings_admin, {
    rv$paintings_data <- db_load_paintings()
    rv$museums <- db_load_museums()
    rv$approved_trigger <- rv$approved_trigger + 1
    session$sendCustomMessage("updatePaintingsData",
                              jsonlite::toJSON(enriched_paintings(), auto_unbox = TRUE))
    showNotification("Paintings refreshed from database!", type = "message")
  })
}


# =============================================================================
# APP LAUNCH
# =============================================================================

shinyApp(ui = ui, server = server)