# **Landscape Through Time**

## **1\. Introduction**

Landscape Through Time is an interactive geospatial web application that allows users to explore historical landscape paintings through their real-world locations. The platform connects art, geography, and time by enabling users to examine how landscapes have changed over time.

The primary goal of the application is to provide an interactive way to analyze spatial and temporal relationships in landscape art while encouraging user participation and data contribution.

This project demonstrates full-stack geospatial application development, relational database design with foreign-key relationships, spatial visualization, user-generated content workflows, and interactive UI development using R Shiny and PostgreSQL.

Key features include:

* A curated gallery of historical landscape artwork with discovery-status badges
* Detailed painting information cards showing location, museum, and historical context
* An interactive map displaying user-submitted contemporary photos and museum locations with toggleable filters
* A multi-type contribution system supporting contemporary photos, user-uploaded paintings, museum photos, and museum information submissions
* A side-by-side comparison tool for analyzing historical and contemporary landscapes
* Administrative tools with filterable workflows for managing and validating content

---

## **2\. Summary of Functions**

The application is organized into five core functional components that support exploration, visualization, and user interaction with geospatial art data.

### **Gallery**

Provides a curated browsing experience of historical landscape paintings. Each painting card displays:

* The artwork itself with year and discovery-status badge ("Discovered" once at least one approved contemporary photo exists; "Undiscovered" otherwise)
* Searchable filter by title or artist
* A detailed information lightbox accessed by clicking a card, which always presents content in a consistent layout regardless of which fields are populated

The painting detail lightbox includes:

* Location information (state, region, location notes)
* Museum status, which always renders one of three states:
  * **Museum on file:** displays the museum name with a "View on Map" action
  * **Private collection:** noted with a lock indicator
  * **Needs update:** prompts users to submit museum information via a dedicated form
* Historical context about the painting
* Action buttons including "View Comparison" (when the painting has approved contemporary photos) and "Add Contemporary Photo"

### **Map**

An interactive geospatial visualization built using Leaflet. The map plots two layers that can be toggled via filter buttons:

* **Museums:** real-world museum and collection locations referenced by the paintings
* **Submissions:** approved user-submitted contemporary photographs

Users can interact with map markers to view details in a side panel, navigate to related paintings or museums via cross-links, and click state-level markers to browse all approved photos within a state.

### **Contribute**

A contribution landing page presents three submission paths via styled cards. Each leads to a focused submission form. The four submission types are:

* **Contemporary Photo (`landscape`):** geotagged photographs of present-day painting locations, linked to a specific painting from the gallery
* **User-Uploaded Painting (`user_painting`):** historic landscape artwork uploaded by a user with associated geographic and museum information
* **Museum Photo (`museum_photo`):** photographs of museum interiors or exterior views of buildings holding the paintings
* **Museum Info (`add_museum`):** proposals to add or update the museum where a specific painting is currently held

All submissions enter a pending state and require admin approval before appearing publicly.

### **Compare**

Enables side-by-side comparison between historical paintings and approved contemporary photographs of the same location. Users can:

* Filter by painting title or artist
* Open a focused comparison lightbox showing the original painting alongside the contemporary photo
* Browse all available comparisons in a grid view

### **Admin**

A password-gated console for managing application content. Features include:

* Submission review queue with **filter dropdowns** for type (`all`, `landscape`, `user_painting`, `museum_photo`, `add_museum`) and status (`Pending`, `Approved`, `Rejected`)
* Approve / reject / delete workflows for individual submissions
* On approval, the appropriate side effects are applied automatically:
  * `landscape` submissions become viewable on the public map
  * `user_painting` submissions are promoted into the paintings table, creating a museum record if needed
  * `add_museum` submissions update the linked painting's `museum_id` and create or reuse a museum record
* A separate paintings management view for editing or removing paintings directly
* Auto-refresh of submissions when switching tabs to keep the review queue current

---

## **3\. Libraries / Dependencies (with Versions)**

The application is built using R and the Shiny framework, with supporting libraries for geospatial analysis, database connectivity, and interactive visualization.

### **Tech Stack**

* **Language:** R
* **Frontend:** HTML, CSS (external `www/styles.css`), JavaScript (inline)
* **Platform:** Shiny Web Application, deployed on shinyapps.io
* **Database:** PostgreSQL hosted on Supabase

### **Core Framework**

* **shiny (v1.12.1)** — Core framework for building the interactive web application
* **bslib (v0.10.0)** — Provides theming and Bootstrap-based UI customization

### **Database & Backend**

* **DBI (v1.3.0)** — Standard interface for connecting R to relational databases
* **RPostgres (v1.4.10)** — PostgreSQL database driver used for data storage and retrieval
* **jsonlite** — Serializes R data frames for client-side JavaScript consumption

### **Geospatial Visualization**

* **leaflet (v2.2.3)** — Creates interactive web maps for visualizing painting locations, museum locations, and user submissions
* **maps (v3.4.3)** — Provides base geographic datasets for geographic reference and visualization support

### **UI & Interactivity**

* **DT (v0.34.0)** — Interactive data tables for the admin submissions queue
* **shinyjs (v2.1.1)** — Adds JavaScript functionality to enhance user interactivity
* **htmltools (v0.5.9)** — Enables custom HTML generation within the Shiny UI

---

## **4\. How to Use the Application**

This application is deployed on a Shiny server and can be accessed through a deployment URL. No local installation is required for general users.

### **Accessing the Application**

* The application is hosted on shinyapps.io
* Users can access the app directly via the provided URL
* No local setup is required for general use

### **Installation / Local Setup (Development Only)**

For developers or contributors running the application locally:

1. Clone the repository:

```
git clone <YOUR-REPO-URL>
cd landscape-through-time
```

2. Open the project in RStudio.

3. Install required dependencies:

```r
install.packages(c(
  "shiny", "bslib", "leaflet", "maps",
  "DBI", "RPostgres", "jsonlite",
  "DT", "shinyjs", "htmltools"
))
```

4. Set up local credentials. The repo includes a `.Renviron.example` file as a template — it lists every environment variable the app needs (Supabase host, port, database name, user, and password) with placeholder values. The real `.Renviron` file holds the actual credentials and is excluded from Git via `.gitignore` so secrets are never committed.

   Copy the template and fill in the real values:

```
cp .Renviron.example .Renviron
```

   Then open `.Renviron` and replace the placeholder values with the actual Supabase credentials. The variables loaded are:

   * `SUPABASE_HOST` — the Supabase pooler hostname (e.g. `aws-0-us-west-2.pooler.supabase.com`)
   * `SUPABASE_PORT` — the pooler port (`6543`)
   * `SUPABASE_DB` — the database name (`postgres`)
   * `SUPABASE_USER` — the pooler-prefixed username (e.g. `postgres.abcd1234efgh5678`)
   * `SUPABASE_PASSWORD` — the database password

   **Do not commit `.Renviron` to Git.** Anyone needing access to the project should request the credentials directly from a project maintainer and create their own local `.Renviron` from the template.

5. Restart your R session so the new environment variables are picked up.

### **Database Access**

The application uses a Supabase PostgreSQL backend.

* General users do not need database access or credentials
* Only users modifying or developing the application require access to the Supabase project
* The connection uses Supabase's **connection pooler** (`aws-0-us-west-2.pooler.supabase.com`, port `6543`) rather than the direct hostname, because shinyapps.io does not support IPv6 and the direct hostname resolves to IPv6 only
* The connection is opened with `gssencmode = "disable"` to avoid GSSAPI negotiation issues

### **Running the Application (Local Development)**

```r
shiny::runApp()
```

### **Project Structure**

```
landscape-through-time/
├── app.R               # Main application: UI, server, helpers
├── www/
│   └── styles.css      # All app-specific styling
├── .Renviron.example   # Template for environment variables (commit this)
├── .Renviron           # Real credentials (DO NOT commit)
├── .gitignore
└── README.md
```

### **Using Key Features**

#### **Gallery**

Browse a curated collection of historical landscape paintings. Filter by title or artist using the search input. Click any painting card to open its detail lightbox.

#### **Map**

* Interactive map displaying museum locations and approved user submissions
* Filter buttons toggle each layer independently
* Click any marker to view details in the side panel
* Click a state-level marker to browse all approved photos for that state

#### **Contribute**

Choose from four submission types via the contribution landing page. Each form collects the appropriate metadata (geocoordinates, museum info, painting attribution, etc.). Submissions enter a pending state until reviewed by an admin.

#### **Compare**

Browse all paintings with approved contemporary photos. Filter by title or artist, and click any pairing to view a focused side-by-side comparison.

#### **Admin**

Administrative access is restricted by password authentication. Admins can:

* Filter the submission queue by type and status
* Approve, reject, or delete submissions
* Manage paintings directly through a separate paintings view
* Refresh data from the database manually if needed

---

## **5\. Data Used & Metadata**

### **Dataset Overview**

The Supabase backend hosts three relational tables and two storage buckets:

* **`paintings`** table — historical landscape artwork with location and museum references
* **`museums`** table — museum and collection records, referenced by paintings via foreign key
* **`submissions`** table — all user contributions across all submission types
* **Storage bucket: contemporary photos** — uploaded user images of present-day locations
* **Storage bucket: museum and painting images** — uploaded painting and museum photos

### **Data Structure**

The initial dataset includes 22 curated landscape paintings by Albert Bierstadt, selected for their strong geographic and historical relevance. Additional records are generated through user-submitted painting uploads and geotagged landscape photography, with all new content flowing through the moderated submissions workflow.

The original `BPaintings.csv` was loaded into Supabase via SQL during initial setup. The `museums` table was added later as part of a normalization effort: museum information was previously embedded directly in the paintings table and is now stored separately and referenced by `museum_id`. This allows multiple paintings to share a museum, prevents duplicate museum records, and supports cleaner display logic on the map.

### **Variables / Fields**

#### **`paintings`**

* `id`
* `title`
* `artist`
* `year`
* `context`
* `image_url`
* `state`
* `region`
* `location_notes`
* `museum_id` *(foreign key to `museums.id`)*

#### **`museums`**

* `id`
* `name`
* `latitude`
* `longitude`
* `image_url`

#### **`submissions`**

* `submission_id`
* `name`
* `email`
* `painting_id` *(foreign key to `paintings.id`, nullable)*
* `photo_url`
* `latitude`
* `longitude`
* `observations`
* `submission_date`
* `approval_status` *(`Pending`, `Approved`, `Rejected`)*
* `submission_type` *(`landscape`, `user_painting`, `museum_photo`, `add_museum`)*
* `painting_title`
* `artist_name`
* `painting_year`
* `painting_context`
* `state`
* `region`
* `location_notes`
* `museum_id` *(foreign key to `museums.id`, set on approval)*
* `proposed_museum_name`
* `proposed_museum_latitude`
* `proposed_museum_longitude`
* `proposed_museum_image_url`

The `proposed_museum_*` columns hold pending museum information until a submission is approved. On approval, the data is promoted into the `museums` table and the submission's `museum_id` is set, after which the proposed columns are no longer the canonical source.

### **Data Collection / Processing**

* Initial painting data was collected and curated by the project team
* Ongoing data collection happens through the in-app submission workflow
* All user-submitted data is moderated through the admin queue before becoming publicly visible

### **Metadata Notes**

* Supabase database created 3/1
* Geographic scope: continental United States, Alaska, and Hawaii

---

## **6\. Known Issues**

* Initial gallery load can be slow due to the size of historical painting images
* Map markers for Alaska and Hawaii use manually patched coordinates because R's built-in `state.center` places them at inset-map positions rather than their true geographic locations
* When deploying changes to shinyapps.io, the `.Renviron` file must be explicitly included in the deployment file list (it is gitignored, so RStudio's deploy dialog may not check it by default)

---

## **7\. Future Features / Improvements**

* **Comparison swap button** — let users flip the order of the historical and contemporary images in the comparison view
* **Painting titles on comparison cards** — surface the painting title and artist directly on each comparison thumbnail in the grid view
* **Loading experience enhancements** — splash-screen polish, progressive image loading, and better perceived performance on first paint
* **Multi-criteria gallery filtering** — combine artist, state, and discovery-status filters
* **Global expansion** — extend the dataset and filtering beyond the United States to support paintings and contributors worldwide
* **Performance optimizations** — image compression and lazy-loading for large datasets
* **Expanded artist representation** — broaden the curated collection beyond Albert Bierstadt
* **Scalability improvements** — caching, paginated submission queues, and infrastructure changes to support broader public access

---

## **8\. References / Resources**

* Supabase PostgreSQL backend and cloud database infrastructure
* R Shiny framework for UI and deployment, hosted on shinyapps.io
* Leaflet for interactive web mapping
* HTML/CSS for frontend customization
* AI-assisted development support using Anthropic Claude and OpenAI ChatGPT for debugging and workflow brainstorming
* Historical metadata verification using Wikipedia and museum archives

---

## **9\. Contact Information**

* [Alex Wood](mailto:jawvt@vt.edu), [Ben D'Elia](mailto:bmdelia07@vt.edu), [Emily Smiley](mailto:Emilysmiley@vt.edu)
* Virginia Polytechnic Institute and State University
