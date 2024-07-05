#library(foreign)

library(tidyverse)

# The aim of the day 3 is to apply some regression models using a mock data 

# download the codebook (ESS9 Appendix A7 Codebook ed. 3.1) from this link: https://ess.sikt.no/en/study/bdc7c350-1029-4cb3-9d5e-53f668b8fa74/220

#ESS9 <- read.spss("", 
#                  to.data.frame = T, use.value.labels = F)
#ESS9_sel <- ESS9 %>% 
#  select(idno, cntry, agea, evlvptn, gndr, yrbrn, bthcld, nbthcld, edulvlb, eduyrs)
#
#sel <- sample(NROW(ESS9_sel), 10000)
#mock <- ESS9_sel[sel,]
#write.csv(mock, "data/mock-data.csv", row.names = F)

data <- read.csv("data/mock-data.csv")

# Aim: run a logistic regression of Y (ever had children), X (ever been in union), and controls (sex, age, education, country)

# To do 1: create variables
# bthcld: ever given birth to/fathered a child?. 1: yes, 2: no -> 0: no, 1: yes
# evlvptn: ever lived with a spouse or partner for three months or more?. 1: yes, 2: no -> 0: no, 1: yes
# gndr: sex. 1: men, 2: women -> 0: women, 1: men
# cntry: country
# agea: age (numeric)
# edulvlb: Highest level of education
# eduyrs: number of schooling years

data2 <- data %>% 
  mutate(everchild = ifelse(bthcld == 2, 0, bthcld),
         evlvptn = ifelse(evlvptn == 2, 0, 1),
         gndr = ifelse(gndr == 2, 0, gndr),
         # educational level
         edulvlb2 = case_when(edulvlb %in% c(0, 1, 113, 129) ~ 1,
                              edulvlb %in% c(2, 212, 213, 221, 222, 223, 229) ~ 2,
                              edulvlb %in% c(3, 311, 312, 313, 321, 322, 323) ~ 3,
                              edulvlb %in% c(4, 412, 413, 421, 422, 423) ~ 4,
                              edulvlb %in% c(5, 510, 520, 610, 620, 710, 720, 800) ~ 5,
                              edulvlb %in% c(55, 555) ~ 0),
         education = case_when(edulvlb2 %in% c(0, 1, 2) ~ "Low",
                               edulvlb2 %in% c(3, 4) ~ "Medium",
                               edulvlb2 == 5 ~ "High"))

data_new <- data %>% 
  mutate(everchild = ifelse(bthcld == 2, 0, bthcld),
         gndr = ifelse(gndr == 2, 0, gndr))

try2 <- try1 %>% 
  mutate(gndr = ifelse(gndr == 2, 0, gndr))

# To do 2: run a logistic regression
logit1 <- glm(everchild ~ evlvptn,
              data = data2, family = binomial("logit"))

logit2 <- glm(everchild ~ evlvptn + gndr + agea,
              data = data2, family = binomial("logit"))

logit <- glm(everchild ~ evlvptn + gndr +
               education + cntry + agea,
               data = data2, family = binomial("logit"))
summary(logit)

install.packages("gtsummary")
library(gtsummary)
#gtsummary()

# Aim: OLS? poisson?
ols <- lm(nbthcld ~ evlvptn + gndr +
            education + cntry + agea,
          data = data2)
summary(ols)
tbl_regression(ols)

poisson <- glm(nbthcld ~ evlvptn + gndr +
                 education + cntry + agea,
               data = data2, family = "poisson")
summary(poisson)