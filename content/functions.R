# Useful Functions

create_line_chart <- function(w.data, w.x, w.y, w.color, w.ltype, w.lwidth, w.palette) {
  
  y.max <- 1.25 * max(w.data[w.y])
  x.breaks <- unique(w.data %>% pull(w.x))
  
  g <-  ggplotly(ggplot(data=w.data, 
                        aes(x = get(eval(w.x)), 
                            y = get(eval(w.y)), 
                            color = get(eval(w.color)), 
                            group=1, 
                            text = paste0("<b>Year: </b>",  get(eval(w.x)), "<br>","<b>Population: </b>", prettyNum(round(get(eval(w.y)), 0), big.mark = ","), "<br>")))  + 
                   geom_line(linetype = w.ltype,
                             size = w.lwidth) +
                   scale_y_continuous(labels = label_comma(), limits = c(0,y.max)) +
                   scale_x_continuous(breaks= x.breaks) +
                   scale_color_manual(values = w.palette) +
                   labs(x = w.x, y = NULL) +
                   theme(plot.title = element_text(size = 10, face = 'bold'),
                         axis.text.x = element_text(angle = 0,
                                                    hjust = 1, 
                                                    vjust = 1,
                                                    family = 'Comic Sans MS'),
                         axis.ticks.x = element_blank(),
                         axis.line.x = element_blank(),
                         axis.line.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.background = element_blank(),
                         panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         text = element_text(family = "Segoe UI"),
                         legend.position = "bottom",
                         legend.title = element_blank()),
                 tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25), hovermode = "x")
  
  return(g)
  
}

create_bar_chart <- function(w.data, w.x, w.y, w.color, w.bartype, w.transparent, w.palette, x.type) {
  
  if (x.type == "discrete") {
  
    g <-  ggplotly(ggplot(data = w.data,
                          aes(x = get(eval(w.x)), 
                              y = get(eval(w.y)), 
                              fill = get(eval(w.color)), 
                              group=1, 
                              text = paste0("<b>", get(eval(w.color))," Population: </b>", prettyNum(round(get(eval(w.y)), 0), big.mark = ","), "<br>"))) +
                    geom_col(color = "black",
                              alpha = w.transparent,
                              position = w.bartype) +
                    scale_y_continuous(labels = label_comma()) +
                    labs(x = w.x, y = NULL) +
                    theme(plot.title = element_text(size = 10, face = 'bold'),
                         axis.text.x = element_text(angle = 0,
                                                    hjust = 1, 
                                                    vjust = 1,
                                                    family = 'Comic Sans MS'),
                         axis.ticks.x = element_blank(),
                         axis.line = element_blank(),
                         panel.background = element_blank(),
                         panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         text = element_text(family = "Segoe UI"),
                         legend.position = "bottom",
                         legend.title = element_blank()),
                  tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25))
    
  } else {
    x.breaks <- unique(w.data %>% pull(w.x))
    g <-  ggplotly(ggplot(data = w.data,
                          aes(x = get(eval(w.x)), 
                              y = get(eval(w.y)), 
                              fill = get(eval(w.color)), 
                              group=1, 
                              text = paste0("<b>", get(eval(w.color))," Population: </b>", prettyNum(round(get(eval(w.y)), 0), big.mark = ","), "<br>"))) +
                     geom_col(color = "black",
                              alpha = w.transparent,
                              position = w.bartype) +
                     scale_y_continuous(labels = label_comma()) +
                     scale_fill_manual(values = w.palette) +
                     scale_x_continuous(breaks= x.breaks) +
                     labs(x = w.x, y = NULL) +
                     theme(plot.title = element_text(size = 10, face = 'bold'),
                           axis.text.x = element_text(angle = 0,
                                                      hjust = 1, 
                                                      vjust = 1,
                                                      family = 'Comic Sans MS'),
                           axis.ticks.x = element_blank(),
                           axis.line = element_blank(),
                           panel.background = element_blank(),
                           panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                           panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                           panel.grid.major.x = element_blank(),
                           panel.grid.minor.x = element_blank(),
                           text = element_text(family = "Segoe UI"),
                           legend.position = "bottom",
                           legend.title = element_blank()),
                   tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25))    
    
    
  }
  
  return(g)
  
}

create_facet_bar_chart <- function(w.data, w.x, w.y, w.color, w.vars, w.scales) {
  
  g <-  ggplotly(ggplot(data = w.data,
                        aes(x = reorder(get(eval(w.x)), -get(eval(w.y))), 
                            y = get(eval(w.y)), 
                            fill = get(eval(w.color)),
                            text = paste0("<b>", get(eval(w.color)), ": </b>", prettyNum(round(get(eval(w.y)), 0), big.mark = ","), "<br>"))) +
                             geom_col(
                               color = "black",
                               alpha = 1.0,
                               position = "dodge") +
                             labs(x = NULL, y = NULL) +
                             scale_y_continuous(labels = label_comma() ) +
                             theme(plot.title = element_text(size = 10, face = 'bold'),
                                   axis.text.x = element_blank(),
                                   axis.ticks.x = element_blank(),
                                   axis.line = element_blank(),
                                   panel.background = element_blank(),
                                   panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                                   panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                                   panel.grid.major.x = element_blank(),
                                   panel.grid.minor.x = element_blank(),
                                   text = element_text(family = "Segoe UI"),
                                   legend.position = "none",
                                   legend.title = element_blank())+
                             facet_wrap(vars(get(eval(w.vars))), scales = w.scales) +
                             theme(legend.position = "none"),
                 tooltip = c("text"))
    
  return(g)
  
}

  
return_population_estimate <- function(data, place, year) {
  
  pop <- data %>% filter(Jurisdiction == place & Year == year) %>% select(Estimate) %>% as.numeric
  return(pop)
  
}

