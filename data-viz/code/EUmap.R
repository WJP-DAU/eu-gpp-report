genMap <- function(variable){
  
  # Drawing individual panels
  panels <- imap(
    map_layers,
    function(panel, panel_name){
      
      # Merging map layers with data
      data4map <- panel %>% 
        left_join(data_points[[variable]], 
                  by = c("polID" = "nuts_id")
        ) %>%
        mutate(
          color_group = case_when(
            value2plot >  0.00 & value2plot <= 0.10 ~ "0%-10%",
            value2plot >  0.10 & value2plot <= 0.25 ~ "10%-25%",
            value2plot >  0.25 & value2plot <= 0.50 ~ "25%-50%",
            value2plot >  0.50 & value2plot <= 0.75 ~ "50%-75%",
            value2plot >  0.75 & value2plot <= 0.90 ~ "75%-90%",
            value2plot >  0.90 & value2plot <= 1.00 ~ "90%-100%"
          ),
          color_group = as.factor(color_group)
        )
      
      country_level <- data4map %>%
        group_by(CNTR_CODE) %>%
        summarise()
      
      # Drawing plot
      p <- ggplot() +
        geom_sf(data  = data4map,
                aes(fill = color_group),
                color = "grey65",
                size  = 0.5) +
        geom_sf(data  = country_level,
                fill  = NA,
                color = "grey25") +
        scale_fill_manual("",
                          values = c("0%-10%"   = "#E03849",
                                     "10%-25%"  = "#FF7900",
                                     "25%-50%"  = "#FFC818",
                                     "50%-75%"  = "#46B5FF",
                                     "75%-90%"  = "#0C75B6",
                                     "90%-100%" = "#18538E"),
                          na.value = "grey95",
                          drop = F)
      
      if (panel_name == "Main"){
        p <- p+
          scale_y_continuous(limits = c(1445631, 5273487)) +
          scale_x_continuous(limits = c(2581570, 5967160))
      } else {
        p <- p +
          coord_sf(expand = F)
      }
      
      p +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid      = element_blank(),
          axis.text       = element_blank()
        )
    }
  )
}