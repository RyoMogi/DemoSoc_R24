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


# exercise 1: make a plot of TFR of a one country

# exercise 2: make a plot of TFR of three countries

# exercise 3: calculate mean TFRs of ever 10 years (e.g. 1960-69) of every country

# exercise 4: merge the TFR data and MAB data and plot them (x: MAB, y: TFR)