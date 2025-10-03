# UFC Fight Analytics - Interactive Visualization Tool
*(Data Visualization course - Project)*

**Authors:**
- Davis Siemens (Idiom 1)  
- Ádám Földvári (Idiom 2)  
- **Máté Lukács (Idiom 3)**
- Inés Simón del Collado (Idiom 4)  

**Instructor:** Pablo Toharia  

---

This project was developed as part of the *"Data Visualization"* course at the **Universidad Politécnica de Madrid (Master in Digital Innovation - EIT Digital)**.  
It presents an interactive **Shiny web app** for analyzing fight data from the Ultimate UFC Dataset.

🔗 **Live App:** https://davissiemens.shinyapps.io/final/  
📊 **Dataset Source:** https://www.kaggle.com/datasets/mdabbert/ultimate-ufc-dataset  

---

## 📁 Project Structure
```
├── app.R # Shiny application source code
├── report.pdf # Full group report structured by abstraction levels
├── data/ # Folder containing CSV files
│ ├── ufc-master.csv # Main UFC fight dataset from Kaggle
│ ├── countries.geojson # GeoJSON with country polygons for map rendering
│ └── us_states.geojson # GeoJSON with US state polygons for map rendering
├── README.md # Project description and instructions
└── requirements.txt # List of R package dependencies to run the app
```
---

## 📖 Project Overview
The goal was to build an interactive tool that assists analysts in exploring UFC fight data through multiple visual idioms.  
Each idiom was designed to answer specific analytical questions using visualization techniques and interactions.

This was a **group project** consisting of four members.  
My contribution focused on **Idiom 3: UFC Fight Performance Heatmap (pages 14–19 in report.pdf)**.

---

## ✨ Features
- **Idiom 1** – Finish Trend Analysis Over Time *(stacked area chart)*  
- **Idiom 2** – Outcome Distributions by Weight Class *(interactive radar chart)*  
- **My Part: Idiom 3** – UFC Fight Performance Heatmap *(interactive heatmap)*  
- **Idiom 4** – Geographic Distribution of UFC Fights *(choropleth maps)*  

---

## 🛠️ My Contribution – Idiom 3: UFC Fight Performance Heatmap
I was responsible for designing and implementing **Idiom 3**, an **interactive heatmap** for exploring fight performance trends by **winner’s age, nationality, and average rounds fought**.  
This module allows analysts to:

- Explore how the winner’s age influences fight duration.  
- Identify which countries tend to have fighters in longer or shorter bouts.  
- Compare fight patterns across weight classes, genders, and fight locations.  
- Track performance trends over time with a year range slider.  

---

## 🔧 Features I Implemented
- **Heatmap Visualization** (age × country with color encoding for average rounds)  
- **Interactive Filters**:  
  - Year Range Slider  
  - Country Grouping Selector  
  - Weight Class Filter  
  - Gender Filter  
  - Fight Location Filter  
  - Age Category Binning  
- **Hover Tooltips** with detailed fight statistics  
- **Dynamic Color Encoding** to highlight performance patterns  

This idiom supports both **discovery** (e.g., identifying correlations between age and fight length) and **comparison** (e.g., contrasting performance between genders, weight classes, or countries).  

For a full explanation of the design, data abstraction, task abstraction, and implementation, refer to the **Idiom 3 section** in `report.pdf` (pages 14–19).  

---

## 📦 Requirements
Install the following R packages before running the app locally:

```r
install.packages("leaflet")
install.packages("sf")
install.packages("dplyr")
install.packages("readr")
install.packages("tidyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("fmsb")
install.packages("shinyWidgets")

▶️ Run Locally

In RStudio or any R environment, run:
shiny::runApp("app.R")
