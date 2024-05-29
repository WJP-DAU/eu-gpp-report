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

# List of legal problems
legalProblems <- c(
  "A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4", "D1", "D2", "D3", "D4", "D5", "D6", "E1", 
  "E2", "E3", "F1", "F2", "G1", "G2", "G3", "H1", "H2", "H3", "I1", "J1", "J2", "J3", "J4", "K1", "K2", "K3", 
  "L1", "L2"
)




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
  if (type == "Lollipop"){
    chart <- genLollipop(data4chart)
  }
  
  saveIT(
    chart = chart, 
    n = chart_n, 
    w = 189.7883, 
    h = 168.7007
  )

  return(chart)
  
}

getAvgData <- function(){
  
  data_wght <- data_points_df %>%
    left_join(region_names) %>%
    mutate(
      weighted_value = value2plot*pop_weight,
      level = "regional"
    )
  
  country_avg <- data_wght %>%
    group_by(country_name_ltn, chart) %>%
    summarise(
      nuts_id    = first(nuts_id),
      value2plot = sum(weighted_value, na.rm = T) 
    ) %>%
    mutate(
      nuts_id   = substr(nuts_id, 1, 2),
      nameSHORT = country_name_ltn,
      level     = "national",
      weighted_value = value2plot  # Simple average for the European Union value. No special weights.
    )
  
  eu_avg <- country_avg %>%
    group_by(chart) %>%
    summarise(
      value2plot       = mean(weighted_value, na.rm = T),
      country_name_ltn = "European Union",
      nuts_id          = "EU",
      nameSHORT        = "European Union",
      level            = "eu"
    )
  
  data_out <- data_wght %>%
    select(country_name_ltn, level, nuts_id, nameSHORT, chart, value2plot) %>%
    bind_rows(
      country_avg %>% select(-weighted_value), 
      eu_avg
    )
  
  return(data_out)
  
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
        value <= 2  ~ 1,
        value <= 4  ~ 0,
        value == 98 ~ 0
      )
    }
  }
  if (topic %in% c("Corruption Change")) {
    trfunc <- function(value) {
      case_when(
        value <= 2  ~ 1,
        value <= 5  ~ 0,
        value == 98 ~ 0
      )
    }
  }
  if (topic %in% c("Corruption Perceptions")) {
    trfunc <- function(value) {
      case_when(
        value <= 2  ~ 0,
        value <= 4  ~ 1,
        value == 98 ~ 0
      )
    }
  }
  if (topic %in% c("Security Violence",
                   "Bribe Victimization",
                   "Civic Participation A Civic Participation B",
                   "Discrimination")) {
    trfunc <- function(value) {
      case_when(
        value == 1  ~ 1,
        value == 2  ~ 0,
        value == 98 ~ 0
      )
    }
  }
  if (topic %in% c("Problem Selection", # % yes to >= 1 question
                   "Problem Resolution", 
                   "Problem Description", 
                   "Problem Evaluation",
                   "Demographics")){

    if (id == "AJP_*_bin"){
      
      
    }
    
  }
  
  # Getting the data to plot
  
  # create demographics
  master_data <- master_data %>%
    mutate(age_bin = case_when(
      age >= 18 & age <= 24 ~ "18-24",
      age >= 25 & age <= 34 ~ "25-34",
      age >= 35 & age <= 44 ~ "35-44",
      age >= 45 & age <= 54 ~ "45-54",
      age >= 55 & age <= 64 ~ "55-64",
      age >= 65 ~ "65+"
    ),
         gender_text = case_when(
           gend == 1 ~ "Male",
           gend == 2 ~ "Female"
         ),
    urban_text = ifelse(urban == 1, "Urban", "Rural"),
    income_q = case_when(
      income_quintile == 1 ~ "Income Quintile 1",
      income_quintile == 2 ~ "Income Quintile 2",
      income_quintile == 3 ~ "Income Quintile 3",
      income_quintile == 4 ~ "Income Quintile 4",
      income_quintile == 5 ~ "Income Quintile 5"
    )
    )
  
  
  
  # list of grouping variables (including dem categories)
  grouping_vars <- list(
    "Total"   = c("country_name_ltn", "nuts_id"),
    "Income"  = c("country_name_ltn", "nuts_id", "income_q"),
    "Gender"  = c("country_name_ltn", "nuts_id", "gender_text"),
    "Age Bin" = c("country_name_ltn", "nuts_id", "age_bin"),
    "Urban"   = c("country_name_ltn", "nuts_id", "urban_text")
  )
  
  
  # For each element in grouping vars, grab those variables
  # from the master data, apply transformation function, and
  # then group by the respective collection of vars and calculate
  # the mean value for that group. 
  # This should create a data2plot for each element in 
  # grouping_vars. 
  
data2plot_list <- imap(grouping_vars, function(vars, demograph) {
    data2plot <- master_data %>%
      select(all_of(vars), all_of(id)) %>%
      mutate(across(all_of(id), ~trfunc(.x))) %>%
      group_by(across(all_of(vars))) %>%
      summarise(
        value2plot = mean(c_across(all_of(id)), na.rm = TRUE),
        .groups = "keep")%>%
      mutate(demographic = ifelse(demograph == "Total", "Total", as.character(get(vars[3])))) #make this the value of element3

    return(data2plot)
  })
  
  combined_data2plot <- bind_rows(data2plot_list) %>%
  select(-income_q, -gender_text, -age_bin, -urban_text)

  
  return(combined_data2plot)
  
}
  
  

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Special Wrangling functions (Access to Justice)                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
add_A2J <- function(df){
  
  # Defining problem variables
  targetVars <- paste0("AJP_", legalProblems, "_bin")
  targetSevs <- paste0("AJP_", legalProblems, "_sev")
  
  # Extracting severity of problem selected
  selec_sev <- df %>%
    pivot_longer(
      !c(country_year_id, AJP_problem), 
      names_to = c("set", ".value"), 
      names_pattern = "AJP_(.*)_(.*)"
    ) %>%
    mutate(
      sev = if_else(AJP_problem == set, sev, NA_real_)
    ) %>%
    group_by(country_year_id) %>%
    summarise(
      AJP_problem = first(AJP_problem),
      sev_problem_selected = sum(sev, na.rm = T)
    ) %>%
    mutate(
      sev_problem_selected = if_else(
        AJP_problem == "", 
        NA_real_, 
        sev_problem_selected 
      )
    ) %>%
    select(-AJP_problem)

  # Estimating problem prevalence 
  probPrev <- purrr::reduce(
    list(
      
      # Data 1: incidence
      df %>%
        select(country_year_id, all_of(targetVars)) %>%
        pivot_longer(
          !country_year_id,
          names_to  = "problem",
          values_to = "answer"
        ) %>%
        mutate(
          problem = str_remove_all(problem, "AJP|_|bin")
        ),
      
      # Data 2: severity
      df %>%
        select(country_year_id, all_of(targetSevs)) %>%
        pivot_longer(
          !country_year_id,
          names_to  = "problem",
          values_to = "severity"
        ) %>%
        mutate(
          problem = str_remove_all(problem, "AJP|_|sev")
        )
    ),
    left_join
  ) %>%
    mutate(
      prevalence1 = case_when(
        answer == 1 ~ 1,
        answer == 2 ~ 0
      ),
      prevalence2 = case_when(
        answer == 1 & severity >= 4 ~ 1,
        answer == 1 & severity  < 4 ~ 0,
        answer == 2 ~ 0
      )
    ) %>%
    group_by(country_year_id) %>%
    summarise(
      across(
        starts_with("prevalence"),
        \(x) sum(x, na.rm = T)
      )
    ) %>%
    mutate(
      across(
        starts_with("prevalence"),
        \(x) if_else(x > 0, 1, 0)
      )
    )
  
  # A2DRM <- df
  
  # Reducing data into a single data frame
  output <- purrr::reduce(
    c(probPrev)
  )
  
  return(output)
  
  
  
}

