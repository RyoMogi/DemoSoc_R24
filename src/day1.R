install.packages("tidyverse")
install.packages("openxlsx")

library(tidyverse)
library(openxlsx)

ptfr <- read.xlsx("data/TFR.xlsx", sheet = "Total fertility rates")
#ptfr <- read.xlsx("data/TFR.xlsx", sheet = "Total fertility rates", startRow = 2)

# ggplot easy

# ggplot detailed
data %>% 
  ggplot(aes(x = year, y = ptfr, group = country)) +
  facet_wrap(~ region) +
  geom_hline(yintercept = 1.3, colour = , linetype = "dashed", size = 1.1) +
  geom_line(colour = "grey40") +
  labs(x = , y =,
       caption = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 12))
ggsave("out/.png", width = 7.5, height = 5, bg = "white")

# data handling (mutate, filter, mean, gather, spread, join)

# save data