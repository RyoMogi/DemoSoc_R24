library(tidyverse)
library(openxlsx)
library(countrycode)

ptfr <- read.xlsx("data/TFR.xlsx", sheet = "Total fertility rates", startRow = 3)
ptfr_adj <- read.xlsx("data/adjTFR.xlsx", sheet = "Tempo-adjusted TFR", startRow = 3)
mab <- read.xlsx("data/MAB.xlsx", sheet = "Mean age at birth", startRow = 3)

# some tips
## 1. ?function: you can get a detailed information. It takes time to get used to read this help information, 
## but it is helpful

## 2. When you get an error message, you can copy and paste this error message to google.
## There are several persons who got the same messages and solved them already.

# %>% : cntr (or command) + shift + m

# exercise 1: make a plot of MAB of a one country
mab %>% 
  select(PERIOD, AUT) %>% 
  ggplot(aes(x = PERIOD, y = AUT)) +
  geom_line()
  
mab %>% 
  select(PERIOD, AUT) %>% 
  mutate(AUT = ifelse(AUT == 0, NA, AUT)) %>% 
  ggplot(aes(x = PERIOD, y = AUT)) +
  geom_line() +
  geom_point()
  
# exercise 2: make a plot of MAB of three countries
mab %>% 
  gather(key = country, value = mab, -PERIOD) %>%
  filter(country %in% c("AUT", "ESP", "JPN")) %>% 
  mutate(mab = ifelse(mab == 0, NA, mab)) %>% 
  ggplot(aes(x = PERIOD, y = mab, colour = country)) +
  geom_line()


mab %>% 
  gather(key = country, value = mab, -PERIOD) %>% View()

# exercise 3: calculate mean MABs of ever 10 years (e.g. 1960-69) of every country
## convert wide data to long data
## create a variable of 10ys years: 1960-69, 1970-79, 1980-89, etc
## use summary function to calculate the mean

mab %>% 
  gather(key = country, value = mab, -PERIOD) %>% 
  mutate(year10 = case_when(PERIOD %in% 1960:1969 ~ "1960s",
                            PERIOD %in% 1970:1979 ~ "1970s",
                            PERIOD %in% 1980:1989 ~ "1980s",
                            PERIOD %in% 1990:1999 ~ "1990s",
                            PERIOD %in% 2000:2009 ~ "2000s",
                            PERIOD %in% 2010:2019 ~ "2010s",
                            PERIOD %in% 2020:2029 ~ "2020s"),
         mab = ifelse(mab == 0, NA, mab)) %>%
  group_by(year10) %>% 
  summarise(mean10 = mean(mab, na.rm = T))

mab %>% 
  gather(key = country, value = mab, -PERIOD) %>% 
  mutate(year10 = case_when(PERIOD %in% 1960:1969 ~ "1960s",
                            PERIOD %in% 1970:1979 ~ "1970s",
                            PERIOD %in% 1980:1989 ~ "1980s",
                            PERIOD %in% 1990:1999 ~ "1990s",
                            PERIOD %in% 2000:2009 ~ "2000s",
                            PERIOD %in% 2010:2019 ~ "2010s",
                            PERIOD %in% 2020:2029 ~ "2020s"),
         mab = ifelse(mab == 0, NA, mab)) %>%
  group_by(country, year10) %>% 
  summarise(mean = mean(mab, na.rm = T))
  

# exercise 4: merge the TFR data and MAB data and plot them (x: MAB, y: TFR)
d_mab <- mab %>% 
  gather(key = country, value = mab, -PERIOD)

d_ptfr <- ptfr %>% 
  gather(key = country, value = tfr, -PERIOD)

d_mab %>% 
  left_join(d_ptfr, by = c("PERIOD", "country")) %>% 
  filter(country == "AUT") %>% 
  mutate(mab = ifelse(mab == 0, NA, mab),
         tfr = ifelse(tfr == 0, NA, tfr)) %>% 
  ggplot(aes(x = mab, y = tfr)) +
  geom_point() +
  labs(x = "Mean age at birth", y = "Total fertility rate") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 12))

d_mab %>% 
  left_join(d_ptfr, by = c("PERIOD", "country")) %>% 
  mutate(mab = ifelse(mab == 0, NA, mab),
         tfr = ifelse(tfr == 0, NA, tfr)) %>% 
  ggplot(aes(x = mab, y = tfr)) +
  facet_wrap(~ country) +
  geom_point() +
  labs(x = "Mean age at birth", y = "Total fertility rate") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 12))

## facet_wrap
d_ptfr %>% 
  mutate(tfr = ifelse(tfr == 0, NA, tfr)) %>% 
  ggplot(aes(x = PERIOD, y = tfr)) +
  facet_wrap(~ country) +
  geom_line(col = "red",
            linewidth = 2)

d_ptfr %>% 
  mutate(tfr = ifelse(tfr == 0, NA, tfr),
         country_name = countrycode(sourcevar = country, 
                                    origin = "iso3c", 
                                    destination = "country.name")) %>% 
  ggplot(aes(x = PERIOD, y = tfr, col = country_name)) +
  geom_line()

d_ptfr %>% 
  mutate(country_name = countrycode(sourcevar = country, 
                                    origin = "iso3c", 
                                    destination = "country.name"))
