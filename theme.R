## OPTFFL Theme Creation

library(showtext)
library(ggplot2)


font_add(family = "FranklinGothic", regular = "framd.ttf")
font_add(family = "ComicSans", regular = "comic.ttf")
font_add(family = "PalatinoLinotype", regular = "pala.ttf")
font_add(family = "Trebuchet", regular = "trebuc.ttf")
showtext_auto()


optffl_theme <- function() {
  theme(
    panel.background = element_rect(fill = "#fafafa", color = NA),
    plot.background = element_rect(fill = "#fafafa", color = NA),
    text = element_text(family = "Trebuchet",face = "bold"),panel.grid.major.y = element_line(color = "#fafafa"),
    panel.grid.minor.y = element_line(color = "#fafafa"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
}


