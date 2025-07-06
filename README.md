#  Flood Exposure Dashboard – Indonesia

An interactive Shiny dashboard to visualize district-level population exposure to riverine flooding in Indonesia under climate change scenarios (RCP 4.5 and RCP 8.5).

 **How to Run the App:**

Option 1: Run locally
1. Clone or download this repository
2. Open `app.R` in RStudio
3. Make sure `indonesia_riverine_floods_people_exposed_rcp4.5_and_8p5_historical.csv` is in the same folder
4. Click **Run App**

Option 2: Open in [Posit Cloud](https://posit.cloud/)
1. Create a new R project in Posit Cloud
2. Upload `app.R` and the CSV file
3. Open `app.R` and click **Run App**

 Features:
- Interactive district + scenario selection
- WRI-themed design (colors, fonts)
- Leaflet-based map (demo)
- Custom legend explaining RCPs and models

---

##  Files

| File Name | Description |
|-----------|-------------|
| `app.R`   | Main Shiny app file |
| `indonesia_riverine_floods_people_exposed_rcp4.5_and_8p5_historical.csv` | Input data for chart/map |

---

##  Background

Built as part of a climate risk and circular economy project.  
Styled in line with WRI branding and intended for deployment on shinyapps.io or Posit Cloud.

