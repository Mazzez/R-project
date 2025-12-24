# R-project
# Global CO₂ Concentration Analysis (1979–2025)

This project analyzes the evolution of global atmospheric CO₂ concentrations using monthly data from the **NOAA Global Monitoring Laboratory** for the period 1979–2025.[file:3][file:1][file:2]

## Project structure

- `co2_mm_gl.csv`: Monthly global CO₂ data provided by NOAA GML (ppm).[file:3]
- `main_final.r`: Main R script for data cleaning, statistical analysis, and generation of plots and summary CSV files.[file:2]
- `rapport_co2.rmd`: Full R Markdown report (statistics, visualizations, interpretation) and the generated `rapport_co2.html` report.[file:1]
- `app_shiny_co2.r`: **R Shiny** application for interactive exploration of the time series, seasons, decades, and detailed statistics.[file:4]

## Installation

1. Install R (RStudio is recommended).
2. Clone this repository:

git clone https://github.com/Mazzez/R-project.git

cd R-project
4. Install the required R packages:

install.packages(c(
"tidyverse","lubridate","ggplot2","ggpubr","zoo",
"gridExtra","viridis","scales","forecast","grid",
"knitr","kableExtra","shiny","plotly","DT","shinyWidgets","bslib"
))

## Usage

### 1. Script-based analysis

Run in R/RStudio:

source("main_final.r")

This script:
- cleans `co2_mm_gl.csv`,
- computes descriptive statistics (annual, seasonal, by decade),
- creates the main figures,
- saves several CSV summary files in the `processed/` folder (cleaned data, annual summary, monthly climatology, decade trends, etc.).[file:2]

### 2. R Markdown report

Open `rapport_co2.rmd` in RStudio and click **Knit** to generate the full HTML report (`rapport_co2.html`).[file:1]

### 3. Shiny application

Launch the interactive app:

source("app_shiny_co2.r")

or

shiny::runApp("app_shiny_co2.r")


The app allows you to:
- filter by time period and seasons,
- visualize temporal evolution, seasonal cycle, decade distributions,
- inspect statistics by year, decade, and season,
- download filtered data as CSV.[file:4]

## Key results

- Total increase of about **88.6 ppm** between 1979 and 2025 (≈ **26%** relative increase).[file:1][file:2]
- Global linear trend ≈ **1.88 ppm/year** with **R² ≈ 0.985**.[file:1][file:2]
- Mean seasonal amplitude of about **2.8 ppm**, with maximum in spring and minimum in autumn (Northern Hemisphere).[file:1][file:2]
- Evidence of an accelerating growth rate in recent decades, shown by annual growth and decade-specific slopes.[file:1][file:2][file:4]

## Data source

Data are from the **NOAA Global Monitoring Laboratory (GML)**:  
https://gml.noaa.gov/ccgg/trends/ [file:3]

Please cite NOAA GML appropriately if you reuse these data in publications or reports.

