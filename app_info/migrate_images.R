# ==========================================================================
# ONE-TIME MIGRATION SCRIPT — Supabase Storage
# 
# Moves all base64 photo_url values from the submissions table into the
# 'submissions' Storage bucket, then updates each row with the public URL.
#
# BEFORE RUNNING:
# 1. Back up your Supabase database (Supabase dashboard -> Database -> Backups)
# 2. Confirm the 'submissions' bucket exists and is public
# 3. Fill in SUPABASE_URL and SUPABASE_ANON_KEY below (or put them in .Renviron)
#
# HOW TO RUN:
#   In RStudio: open this file and click Source, or run:
#     source("migrate_images.R")
#   The script is safe to re-run — it skips rows already migrated.
# ==========================================================================

library(DBI)
library(RPostgres)
library(httr)
library(base64enc)

readRenviron(".Renviron")

# --- Config --------------------------------------------------------------
SUPABASE_URL      <- Sys.getenv("SUPABASE_URL")       # e.g. "https://xxxxx.supabase.co"
SUPABASE_ANON_KEY <- Sys.getenv("SUPABASE_ANON_KEY")  # the "anon public" key
BUCKET_NAME       <- "submissions"

if (SUPABASE_URL == "" || SUPABASE_ANON_KEY == "") {
  stop("Set SUPABASE_URL and SUPABASE_ANON_KEY in .Renviron before running.")
}

# --- DB connection -------------------------------------------------------
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

# --- Upload one base64 image to Storage, return the public URL -----------
upload_base64_to_storage <- function(base64_data_uri, filename) {
  # Strip "data:image/jpeg;base64," prefix if present
  base64_part <- sub("^data:image/[^;]+;base64,", "", base64_data_uri)
  raw_bytes <- base64decode(base64_part)
  
  # Detect content type from the data URI prefix
  ct <- "image/jpeg"
  if (grepl("^data:image/png", base64_data_uri)) ct <- "image/png"
  
  upload_url <- paste0(SUPABASE_URL, "/storage/v1/object/", BUCKET_NAME, "/", filename)
  
  res <- POST(
    upload_url,
    add_headers(
      `Authorization` = paste("Bearer", SUPABASE_ANON_KEY),
      `Content-Type`  = ct,
      `x-upsert`      = "true"
    ),
    body = raw_bytes
  )
  
  if (status_code(res) >= 400) {
    stop("Upload failed (", status_code(res), "): ", content(res, "text", encoding = "UTF-8"))
  }
  
  # Public URL for the bucket
  paste0(SUPABASE_URL, "/storage/v1/object/public/", BUCKET_NAME, "/", filename)
}

# --- Main migration ------------------------------------------------------
cat("Connecting to database...\n")
con <- get_db_con()

# Find all submissions with base64 photos (photo_url starting with 'data:')
cat("Finding rows to migrate...\n")
rows <- dbGetQuery(con, "
  SELECT submission_id, photo_url
  FROM submissions
  WHERE photo_url LIKE 'data:image%'
")

cat("Found", nrow(rows), "submissions with base64 photos to migrate.\n\n")

if (nrow(rows) == 0) {
  cat("Nothing to migrate. All submissions already use URLs.\n")
  dbDisconnect(con)
  invisible()
} else {
  
  success_count <- 0
  fail_count <- 0
  failures <- character()
  
  for (i in seq_len(nrow(rows))) {
    sid <- rows$submission_id[i]
    b64 <- rows$photo_url[i]
    
    # Decide file extension from MIME
    ext <- "jpg"
    if (grepl("^data:image/png", b64)) ext <- "png"
    
    filename <- paste0(sid, ".", ext)
    
    cat(sprintf("[%d/%d] Migrating submission_id=%s ... ", i, nrow(rows), sid))
    
    tryCatch({
      public_url <- upload_base64_to_storage(b64, filename)
      
      # Update the DB row with the URL
      dbExecute(con,
                "UPDATE submissions SET photo_url = $1 WHERE submission_id = $2",
                params = list(public_url, sid)
      )
      
      cat("OK\n")
      success_count <- success_count + 1
    }, error = function(e) {
      cat("FAILED:", e$message, "\n")
      fail_count <<- fail_count + 1
      failures <<- c(failures, sid)
    })
  }
  
  cat("\n==============================\n")
  cat("Migration complete.\n")
  cat("  Succeeded:", success_count, "\n")
  cat("  Failed:   ", fail_count, "\n")
  if (length(failures) > 0) {
    cat("  Failed submission IDs:\n")
    cat("   ", paste(failures, collapse = ", "), "\n")
  }
  cat("==============================\n")
  
  dbDisconnect(con)
}
