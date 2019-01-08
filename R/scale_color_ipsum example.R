
pacman::p_load(tidyverse, hrbrthemes, tntpr)

data(mtcars)

mtcars %>% glimpse()

mtcars %>%
  ggplot(aes(x = factor(cyl), y = carb, fill = carb)) +
  geom_bar(stat = "identity") +
  theme_ipsum() +
  scale_color_continuous()


