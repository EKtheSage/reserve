library(data.table)
library(ggplot2)
library(showtext)


font_add_google('Nunito', 'nunito')
font_add_google('Inter', 'inter')
showtext_auto()

theme_set(
  theme_minimal() +
    theme(text = element_text(family = 'nunito'))
)

pie <- 100 #premium