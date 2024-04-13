genBar <- function(dta){
  
  bars <- dta %>%
    group_by(country_name_ltn) %>%
    summarise(
      avg_value = 100*mean(value2plot, na.rm = T),
      .groups   = "keep"
    )%>%
    mutate(
      color = cut(avg_value, 
                  breaks = c(0, 10, 25, 50, 75, 90, 100),
                  labels = c("0%-10%", "10%-25%", "25%-50%", "50%-75%", "75%-90%", "90%-100%"))
    )%>%
    ggplot()+
  geom_col(aes(avg_value, country_name_ltn, fill = color), width = 0.5) +
    scale_fill_manual(values = cat_palette) +
    scale_x_continuous(
      limits   = c(0, 110),
      breaks   = seq(0, 100, by = 20), 
      expand   = c(0, 0), 
      position = "top" 
    ) +
    theme(legend.position = "none")+
    scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
    theme(
      panel.background   = element_rect(fill      = "white"),
      panel.grid.major.x = element_line(color     = "#5e5c5a", 
                                        linewidth = 0.25, 
                                        linetype  = "dashed"),
      axis.ticks.length  = unit(0, "mm"),
      axis.title         = element_blank(),
      axis.line.y.left   = element_line(color = "black", 
                                        linewidth = 1),
      axis.text.y        = element_text(family = "Lato Full",
                                        face   = "plain",
                                        color  = "#222221", 
                                        size   = 3.514598*.pt),
      axis.text.x        = element_text(family = "Lato Full",
                                        face   = "plain",
                                        color  = "#524F4C", 
                                        size   = 3.514598*.pt, 
                                        hjust  = 0)
    )

  return(bars)

}

