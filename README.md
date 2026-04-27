# **Landscape Through Time**

## **1\. Introduction**

Landscape Through Time is an interactive geospatial web application that allows users to explore historical landscape paintings through their real-world locations. The platform connects art, geography, and time by enabling users to examine how landscapes have changed over time.

The primary goal of the application is to provide an interactive way to analyze spatial and temporal relationships in landscape art while encouraging user participation and data contribution.

This project demonstrates full-stack geospatial application development, database design, spatial visualization, user-generated content workflows, and interactive UI development using R Shiny and PostgreSQL.

Key features include:

* A curated gallery of historical landscape artwork  
* An interactive map displaying user submitted contemporary photos and museum locations  
* A contribution system for uploading geotagged images and historical artwork  
* A comparison tool for analyzing historical and contemproary landscapes  
* Administrative tools for managing and validating content

---

## **2\. Summary of Functions**

The application is organized into five core functional components that support exploration, visualization, and user interaction with geospatial art data.

### **Gallery**

Provides a curated browsing experience of historical landscape paintings. Users can view artwork alongside metadata such as artist, location, and historical context, enabling structured and visual exploration of the dataset.

### **Map**

An interactive geospatial visualization built using Leaflet that plots:

* Related real-world sites and museums  
* User-submitted geotagged images

Users can interact with map markers to explore painting locations, museum references, and user-submitted geotagged images while navigating seamlessly between application components.

### **Contribute**

Allows users to connect historical landscape paintings with real-world geography by uploading and geotagging contemporary images of painting locations. Users can either:

* Upload contemporary photographs of historical painting locations, or  
* Upload historic landscape artwork to allow others to connect it with a real-world geographic location

This feature enables spatial comparison between artistic representations and present-day landscapes, allowing users to explore how locations have changed over time or how different artists interpret the same place.

### **Compare**

Enables side-by-side comparison between historical paintings and contemporary images of the same or similar locations, supporting visual analysis of landscape change over time.

### **Admin**

Provides administrative functionality for managing application content, including:

* Reviewing and validating user submissions  
* Moderating uploaded content  
* Maintaining data integrity within the database

---

## **3\. Libraries / Dependencies (with Versions)**

The application is built using R and the Shiny framework, with supporting libraries for geospatial analysis, database connectivity, and interactive visualization.

### **Tech Stack**

* **Language:** R  
* **Frontend:** HTML, CSS (via Shiny)  
* **Platform:** Shiny Web Application  
* **Database:** PostgreSQL

### **Core Framework**

* **shiny (v1.12.1)** — Core framework for building the interactive web application  
* **bslib (v0.10.0)** — Provides theming and Bootstrap-based UI customization

### **Database & Backend**

* **DBI (v1.3.0)** — Standard interface for connecting R to relational databases  
* **RPostgres (v1.4.10)** — PostgreSQL database driver used for data storage and retrieval

### **Geospatial Visualization**

* **leaflet (v2.2.3)** — Creates interactive web maps for visualizing painting locations and user submissions  
* **maps (v3.4.3)** — Provides base geographic datasets for geographic reference and visualization support

### **UI & Interactivity**

* **DT (v0.34.0)** — Interactive data tables for filtering and displaying painting metadata  
* **shinyjs (v2.1.1)** — Adds JavaScript functionality to enhance user interactivity  
* **htmltools (v0.5.9)** — Enables custom HTML generation within the Shiny UI

---

## **4\. How to Use the Application**

This application is deployed on a Shiny server and can be accessed through a deployment URL. No local installation is required for general users.

### **Accessing the Application**

* The application is hosted on a ShinyApps server  
* Users can access the app directly via the provided URL  
* No local setup is required for general use

### **Installation / Local Setup (Development Only)**

For developers or contributors running the application locally:

1. Clone the repository:

git clone **\<YOUR-REPO-URL\>**

2. Open the project in RStudio.  
3. Install required dependencies:

install.packages(c(  
  "shiny",  
  "leaflet",  
  "DBI",  
  "RPostgres",  
  "DT",  
  "bslib",  
  "shinyjs",  
  "htmltools",  
  "maps"  
))

4. Ensure database connectivity is configured (Supabase/PostgreSQL credentials may be required for development access).

### **Database Access**

The application uses a Supabase PostgreSQL backend.

* General users do not need database access or credentials  
* Only users modifying or developing the application require access to the Supabase project

### **Running the Application (Local Development)**

shiny::runApp("app/")

### **Using Key Features**

#### **Gallery**

Browse a curated collection of historical landscape paintings and view associated metadata.

#### **Map**

* Interactive map displaying painting locations and user submissions  
* Users can click markers to view artwork details  
* Filtering options include:  
  * Artist  
  * State (to view geographic distribution and density of submissions)

#### **Contribute**

Users can submit geotagged content directly through the application by following the instructions provided in the Contribute tab. Submissions are stored in the database and integrated into the map visualization.

#### **Compare**

Enables side-by-side comparison of:

* Historical landscape paintings  
* Contemporary or user-submitted images of the same location

This feature highlights visual and spatial changes over time.

#### **Admin**

Administrative access is restricted by password authentication.  
Admins can:

* Review user submissions  
* Moderate content  
* Manage dataset integrity

---

**5\. Data Used & Metadata**

### **Dataset Overview**

* Supabase   
  * 2 storage buckets  
* Submissions table  
  * User submissions  
* Paintings table  
  * The initial dataset includes 22 curated landscape paintings by Albert Bierstadt, selected for their strong geographic and historical relevance.  
  *  Additional records are generated through user-submitted painting uploads and geotagged landscape photography.  
    

### 

### 

### **Data Structure**

* BPaintings.csv  
  * Initial dataset   
  * Altered once uploaded to Supabase using SQL

### **Variables / Fields**

* Submissions  
  * submission\_id  
  * name  
  * email  
  * painting\_id  
  * photo\_url  
  * latitude  
  * longitude  
  * observations  
  * submission\_date  
  * approval\_status  
  * submission\_type  
  * painting\_title  
  * artist\_name  
  * painting\_year  
  * painting\_context  
  * state  
  * region  
  * location\_notes  
  *   
* Paintings  
  * id  
  * title  
  * year  
  * context  
  * image\_url  
  * artist  
  * museum\_name  
  * museum\_latitude  
  * museum\_longitude  
  * museum \_image\_url  
  * state  
  * region  
  * location\_notes

### **Data Collection / Processing**

* Initial painting data was collected by the group  
* User input is the standard data collection method  
  * Painting uploads  
  * Landscape photo uploads

### **Metadata Notes**

* Supabase Database creation 3/1  
* United States

---

## **6\. Known Issues**

* Slow loading due to painting size  
* Admin tab sometimes reverts to the application’s main theme rather than its standard dark theme  
* Map highlighting for Alaska and Hawaii requires additional frontend refinement.

---

## **7\. Future Features / Improvements**

* Improved filtering (multi-criteria selection)  
* Better UI consistency across painting cards  
* Paintings, artists, and filtering scaled globally instead of limited to United States  
* Performance optimizations for large datasets  
* Expanded artist representation and broader artwork diversity  
* Scalability improvements for broader public access and long-term deployment

---

## **8\. References / Resources**

* Supabase PostgreSQL backend and cloud database infrastructure  
* R Shiny framework for UI and deployment  
* HTML/CSS for frontend customization  
* AI-assisted development support using OpenAI ChatGPT and Anthropic Claude for debugging and workflow brainstorming  
* Historical metadata verification using Wikipedia and museum archives

---

**9\. Contact Information:**

* [Alex Wood](mailto:jawvt@vt.edu), [Ben D'Elia](mailto:bmdelia07@vt.edu), [Emily Smiley](mailto:Emilysmiley@vt.edu)  
* Virginia Polytechnic Institute and State University

