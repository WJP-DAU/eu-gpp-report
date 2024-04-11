## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU GPP Report - Functions
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
## 1.  General functions                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

resetDirs <- function(){
  
  # List and reset previous outputs
  prevOutputs <- list.files(
    "outputs", 
    include.dirs = F, 
    full.names   = T, 
    recursive    = T
  )
  file.remove(prevOutputs)
}

saveIT <- function(chart, n, w, h) {
  ggsave(
    plot   = chart,
    file   = file.path(path2EU,
      "EU-S Data/reports/eu-gpp-report/data-viz/outputs", paste0("chart_", n, ".svg"),
      fsep = "/"
    ), 
    width  = w, 
    height = h,
    units  = "mm",
    dpi    = 72,
    device = "svg"
  )
} 

getInsets <- function(targets){
  lapply(targets, 
         function(pol){
           base_map %>% filter(polID %in% pol)
         })
}

callVisualizer <- function(chart_n) {
  
  type <- outline %>%
    filter(n == chart_n) %>%
    slice(1) %>%
    pull(type) 
  
  data4chart <- data_points[[paste("Chart", chart_n)]]
  
  if (type == "Map"){
    chart <- genMap(data4chart)
  }
  if (type == "Bars"){
    chart <- genBar(data4chart)
  }
  
  saveIT(
    chart = chart, 
    n = chart_n, 
    w = 189.7883, 
    h = 168.7007
  )

  return(chart)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Wrangling function                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wrangleData <- function(chart_n){
  
  # Getting variable info
  id <- outline %>%
    filter(n == chart_n) %>%
    pull(id)
  
  topic <- outline %>%
    filter(n == chart_n) %>%
    slice(1) %>%
    pull(topic)
  
  # Defining a transforming function
  if (topic %in% c("Trust", 
                   "Security", 
                   "Law Enforcement Performance", 
                   "Criminal Justice Performance",
                   "Law Enforcement Performance", 
                   "Perceptions on Authoritarian Behavior", 
                   "Justice System Evaluation",
                   "Civic Participation A",
                   "Civic Participation B",
                   "Opinions regarding Corruption",
                   "Information Provision",
                   "Information Requests")) {
    trfunc <- function(value) {
      case_when(
        value <= 2 ~ 1,
        value <= 4 ~ 0
      )
    }
  }
  if (topic %in% c("Corruption Change")) {
    trfunc <- function(value) {
      case_when(
        value <= 2 ~ 1,
        value <= 5 ~ 0
      )
    }
  }
  if (topic %in% c("Corruption Perceptions",
                   "Bribe Victimization")) {
    trfunc <- function(value) {
      case_when(
        value <= 2 ~ 0,
        value <= 4 ~ 1
      )
    }
  }
  if (topic %in% c("Security Violence",
                   "Civic Participation A Civic Participation B",
                   "Discrimination")) {
    trfunc <- function(value) {
      case_when(
        value == 1 ~ 1,
        value == 2 ~ 0
      )
    }
  }
  
  # Creating data2plot
  data2plot <- master_data %>%
    select(country_name_ltn, nuts_id, all_of(id)) %>%
    mutate(
      across(
        all_of(id),
        ~trfunc(.x)
      )
    ) %>%
    group_by(country_name_ltn, nuts_id) %>%
    summarise(
      value2plot = mean(c_across(all_of(id)), na.rm = T),
      .groups = "keep"
    )
  
  return(data2plot)
  
}



