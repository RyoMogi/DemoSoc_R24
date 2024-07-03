install.packages("tidyverse")
install.packages("openxlsx")
install.packages("countrycode")

library(tidyverse)
library(openxlsx)
library(countrycode)

ptfr <- read.xlsx("data/TFR.xlsx", sheet = "Total fertility rates", startRow = 3)
ptfr_adj <- read.xlsx("data/adjTFR.xlsx", sheet = "Tempo-adjusted TFR", startRow = 3)

# ggplot easy
ptfr %>% 
  select(PERIOD, ESP) %>% 
  ggplot(aes(x = PERIOD, y = ESP)) +
  geom_line()

# data handling (mutate, filter, mean, gather, spread, join)
## mutate ifelse
ptfr %>% 
  select(PERIOD, ESP) %>% 
  mutate(ESP = ifelse(ESP == 0, NA, ESP)) %>% 
  ggplot(aes(x = PERIOD, y = ESP)) +
  geom_line()

## gather
ptfr %>% 
  gather(key = country, value = tfr, - PERIOD) %>% 
  mutate(tfr1 = ifelse(tfr == 0, NA, tfr)) %>% 
  filter(country == "ESP") %>% 
  ggplot(aes(x = PERIOD, y = tfr, colour = country)) +
  geom_line()

ptfr %>% 
  gather(key = country, value = tfr, - PERIOD) %>% 
  mutate(tfr1 = ifelse(tfr == 0, NA, tfr)) %>% 
  filter(country %in% c("ESP", "ITA", "JPN")) %>% 
  ggplot(aes(x = PERIOD, y = tfr1, colour = country)) +
  geom_line()

## save the object
d_ptfr <- ptfr %>% 
  gather(key = country, value = tfr, - PERIOD) %>% 
  mutate(tfr = ifelse(tfr == 0, NA, tfr))

## mutate case_when
unique(d_ptfr$country)

`%out%` = Negate(`%in%`)

ptfr %>% 
  gather(key = country, value = tfr, - PERIOD) %>% 
  filter(country %out% c("DEUTE", "DEUTNP", "DEUTW", "FRATNP", "GBR_NIR", "GBR_NP", "GBR_SCO", "GBRTENW")) %>% 
  mutate(tfr = ifelse(tfr == 0, NA, tfr),
         country_name = countrycode(country, "iso3c", "country.name"),
         region = case_when(country_name %in% c("Lithuania", "Estonia", "Russia", "Latvia",
                                                "Ukraine", "Belarus", "Romania", "Bulgaria",
                                                "Slovakia", "Hungary", "Slovenia", "Poland",
                                                "Czechia", "Croatia") ~ "Eastern Europe",
                            country_name %in% c("Portugal", "Spain", "Italy") ~ "Southern Europe",
                            country_name %in% c("Germany", "Austria", "Switzerland", "France", 
                                                "Netherlands", "United.Kingdom", "Belgium",
                                                "Ireland") ~ "Western Europe",
                            country_name %in% c("Taiwan", "Japan", "South Korea") ~ "East Asis",
                            country_name %in% c("Sweden", "Norway", "Denmark", "Finland",
                                                "Iceland") ~ "Northern Europe",
                            country_name %in% c("United States", "Canada"),
                            T ~ "South America")) %>% View() 

# group_by and summarise
ptfr %>% 
  gather(key = country, value = tfr, - PERIOD) %>% 
  mutate(tfr = ifelse(tfr == 0, NA, tfr)) %>% 
  group_by(country) %>% 
  summarise(mean = mean(tfr, na.rm = T))

ptfr %>% 
  gather(key = country, value = tfr, - PERIOD) %>% 
  mutate(tfr = ifelse(tfr == 0, NA, tfr)) %>% 
  group_by(PERIOD) %>% 
  summarise(mean = mean(tfr, na.rm = T))

## spread
ptfr %>% 
  gather(key = country, value = tfr, -PERIOD) %>% 
  spread(key = country, value = tfr)

## join
d_ptfr_adj <- ptfr_adj %>% 
  gather(key = country, value = tfr, -PERIOD) %>% 
  rename(adjtfr = tfr)

ptfr %>% 
  gather(key = country, value = tfr, -PERIOD) %>% 
  left_join(d_ptfr_adj, by = c("PERIOD", "country"))

# save data
write.csv(d_ptfr_adj, "out/d_ptfr_adj.csv")

# ggplot step 2
ptfr %>% 
  select(PERIOD, ESP) %>% 
  mutate(ESP = ifelse(ESP == 0, NA, ESP)) %>% 
  ggplot(aes(x = PERIOD, y = ESP)) +
  geom_line() +
  labs(x = "Year", y = "Total fertility rate", title = "Spain") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 12))

# ggplot step 3
d_ptfr %>% 
  ggplot(aes(x = PERIOD, y = tfr)) +
  facet_wrap(~ country) +
  geom_line(colour = "grey40") +
  labs(x = "Year", y = "Total fertility rate") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 12))
ggsave("out/.png", width = 7.5, height = 5, bg = "white")