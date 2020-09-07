# 01 load libraries ----------------
library(tidyverse)
source('theme_javier.R')
theme_set(theme_javier())

# read data -----------
bilge <- read_csv('data/cleaned data/bilge.csv')
bilge1 <- read_csv('data/cleaned data/bilge1.csv')

# test for differences in hazard scores between vessel type (all vessels)------------------------
# Use a GLM with Gamma errors and log link for strictly positive and overdispersed data
m0.1 <- glm(hazard_score+1~ 1, data = bilge, family = Gamma('log')) 
m1 <- glm(hazard_score+1~ vessel_type, data = bilge, family = Gamma('log')) 
summary(m1) # trailer boat has lower risk than commercial vessel (baseline level)  
anova(m0.1, m1, test = 'Chisq')
drop1(m1, test = 'Chisq')

# test for differences in hazard scores between vessel type (risky vessels only)------------------------
# Use a GLM with Gamma errors and log link for strictly positive and overdispersed data
m0.2 <- glm(hazard_score~ 1, data = bilge1, family = Gamma('log')) 
m2 <- glm(hazard_score~ vessel_type, data = bilge1, family = Gamma('log')) 
summary(m2) # trailer boats and yachts have lower risk than commercial vessel (baseline level).  
anova(m0.2, m2, test = 'Chisq')
