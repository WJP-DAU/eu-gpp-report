## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU GPP Report - RunMe File
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     March 26th, 2024
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Presettings                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading settings and functions
source("code/settings.R")
source("code/functions.R")
source("code/EUmap.R")
source("code/EUbars.R")

# Loading data
master_data <- read_dta(
  file.path(
    path2EU,
    "EU-S Data/eu-gpp/1. Data/3. Merge/EU_GPP_2024.dta"
  )
)

# Loading outline
outline <- read.xlsx(
  "inputs/report_outline_1.xlsx",
  sheet = "outline"
)

# Loading map layers
base_map <- st_read(
  file.path(
    path2DA,
    "8. Data/EU-NUTS-GIS/EU_base_map.geojson"
  )
) %>%
  filter(!(polID %in% c("GL")))

insets <- getInsets(list(
  "Canarias/Madeiras" = c("ES7", "PT3"),
  "Açores"            = "PT2",
  "Cyprus"            = "CY0"
))

map_layers <- c(
  list("Main" = base_map), 
  insets
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Wrangle data                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Creating a named list to loop over
chart_list <- c(1:4, 9:17, 30:31, 39:100, 114:116, 119:120, 129, 135:142)
names(chart_list) <- paste("Chart",chart_list)

# Applying the wrangling function across charts
data_points <- lapply(
  chart_list,
  wrangleData
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Create Viz                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Calling the visualizer for each chart
lapply(
  chart_list,
  callVisualizer
)

