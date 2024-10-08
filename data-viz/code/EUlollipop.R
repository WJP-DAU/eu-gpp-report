genLollipop <- function(dta){
  
  panels <- list(
    "A" = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France"),
    "B" = c("Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania"),
    "C" = c("Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
  )
  
  region_data <- dta %>%
    left_join(
      region_names,
      by = "nuts_id"
    ) %>%
    mutate(
      level = "Regional"
    ) %>%
    ungroup() %>%
  select(country = country_name_ltn, nameSHORT, level, value2plot)

  country_data <- region_data %>%
    group_by(country) %>%
    summarise(
      value2plot = mean(value2plot, na.rm = T),
      level      = "National"
    ) %>%
    mutate(
      nameSHORT = country
    )
  
  data4plot <- region_data %>%
    bind_rows(country_data) %>%
    mutate(
      level = factor(level,
                     levels = c("National", "Regional"))
    ) %>%
    arrange(country, level) %>%
    distinct(country, nameSHORT,
             .keep_all = T) %>%
    mutate(
      order = row_number()
    )
  
  chart_panels <- lapply(
    panels,
    function(group){
      
      subset_data <- data4plot %>%
        filter(country %in% group)
      
      bchart <- ggplot() +
        geom_segment(data      = subset_data,
                     aes(x     = reorder(nameSHORT,
                                         desc(order)),
                         xend  = reorder(nameSHORT,
                                         desc(order)),
                         y     = 0,
                         yend  = value2plot,
                         color = level),
                     linewidth = 1) +
        geom_point(data      = subset_data,
                   aes(x     = reorder(nameSHORT,
                                       desc(order)),
                       y     = value2plot,
                       color = level),
                   size = 2) +
        scale_y_continuous(position = "right",
                           breaks   = seq(0, 1, 0.25),
                           labels   = paste0(seq(0, 1, 0.25)*100, "%"),
                           limits   = c(-0.01, 1.03),
                           expand   = expansion(mult = 0)) +
        scale_colour_manual(values   = lpop_palette)  +
        coord_flip() +
        theme(
          axis.title.y     = element_blank(),
          axis.title.x     = element_blank(),
          axis.text.x      = element_text(family    = "Lato Full",
                                          face      = "plain",
                                          size      = 3.063138*.pt,
                                          color     = "#524F4C",
                                          margin    = margin(10, 0, 0, 0)),
          axis.text.y      = element_text(family    = "Lato Full",
                                          face      = "plain",
                                          size      = 3.063138*.pt,
                                          color     = "#524F4C",
                                          margin    = margin(10, 0, 0, 0),
                                          hjust     = 0),
          axis.line.x      = element_line(linewidth = 0.25,
                                          colour    = "#5e5c5a",
                                          linetype  = "solid"),
          axis.ticks.y       = element_blank(),
          panel.grid         = element_blank(),
          panel.grid.major.x = element_line(linewidth = 0.25,
                                            colour    = "#5e5c5a",
                                            linetype  = "dashed"),
          panel.background   = element_blank(),
          legend.position    = "none"
        )
      
      return(bchart)
    }
  )
  
  patch <- chart_panels[["A"]] | chart_panels[["B"]] | chart_panels[["C"]]
  
  return(patch)
  
}
