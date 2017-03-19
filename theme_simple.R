#Theme
theme_simple <- theme(
  text = element_text(size = 14),
  plot.title = element_text(size = rel(1.6),
                            face = "bold"),
  plot.subtitle = element_text(colour = "#666867", face="italic", size = rel(.8), margin = unit(c(.30, .25, .55, .25), "cm")),
  axis.title = element_text(
    size = rel(1),
    debug = FALSE,
    colour = "#666867"),
  axis.text = element_text(size = rel(1), face = "bold", colour = "#666867"),
  plot.caption = element_text(color = "#666666", size=rel(.6), hjust = 0),
  #panel.border = element_rect(fill = NA, colour = "#666867", linetype = 1),
  panel.grid.major = element_line(
    color = "black"),
  panel.grid.minor = element_line(
    color = "black"),
  panel.grid.major.x = element_line(linetype = 0
  ),
  panel.grid.minor.x = element_line(linetype = 0
  ),
  panel.grid.major.y = element_line(linetype = 3,
                                    colour = "#666867"
  ),
  panel.grid.minor.y = element_line(linetype = 3
  ),
  panel.background = element_rect(fill="white",
                                  colour = "grey")
)