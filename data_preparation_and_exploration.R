# 01 load libraries ----------------
library(readxl)
library(tidyverse)
library(forcats)
source('theme_javier.R')
theme_set(theme_javier())

# 02 read, prep data and calculate hazard score-----------------------------------
bilge <-
  read_excel('data/Bilge.xlsx', 7) %>% 
  mutate(
    hazard_score = # calculate hazard score
      freq_use *
      last_trip *
      source_water *
      risky_water *
      max_vol *
      empty_depart *
      empty_arrival *
      freq_discharge
  )

# number of zeros
table(bilge$hazard_score)
sum(bilge$hazard_score==0)/nrow(bilge)

bilge %>% distinct(vessel_type) 


# 03 Rename variables  ----------------
bilge <-
  bilge %>%
  rename(
    FreqUse = "freq_use",
    Length = "length",
    Speed = 'speed',
    TripType = "last_trip",
    SourceWater = "source_water" ,
    RiskyWater = "risky_water" ,
    MaxVol = "max_vol" ,
    EmptyDepart = "empty_depart" ,
    EmptyArrival = "empty_arrival" ,
    FreqDischarge = "freq_discharge"
  )


## Remove vessels with hazard score zero
bilge1 <- 
  bilge %>% 
  dplyr::filter(hazard_score>0)

write_csv(bilge, 'data/cleaned data/bilge.csv') #all vessel
write_csv(bilge1, 'data/cleaned data/bilge1.csv') # risky vessel data

# Calculate hazard sccore IQR ------------
# All vessels, i.e. bilge dataset

IQR_bilge <- 
  bilge %>% 
  group_by(vessel_type) %>% 
  nest() %>% 
  mutate(stats = map(.x = data, ~boxplot.stats(.$hazard_score,coef = 1.5)))

IQR_bilge$stats[[1]][1]$stats[4] - IQR_bilge$stats[[1]][1]$stats[2] # commercial
IQR_bilge$stats[[2]][1]$stats[4] - IQR_bilge$stats[[2]][1]$stats[2] # launch
IQR_bilge$stats[[3]][1]$stats[4] - IQR_bilge$stats[[3]][1]$stats[2] # trailer
IQR_bilge$stats[[4]][1]$stats[4] - IQR_bilge$stats[[4]][1]$stats[2] #yacht

# overall bilge
boxplot.stats(bilge$hazard_score,coef = 1.5)$stats[4] - boxplot.stats(bilge$hazard_score,coef = 1.5)$stats[2] 

## for bilge1 'risky vessels-------
IQR_bilge1 <- 
  bilge1 %>% 
  group_by(vessel_type) %>% 
  nest() %>% 
  mutate(stats = map(.x = data, ~boxplot.stats(.$hazard_score,coef = 1.5)))

IQR_bilge1$stats[[1]][1]$stats[4] - IQR_bilge1$stats[[1]][1]$stats[2] # commercial
IQR_bilge1$stats[[2]][1]$stats[4] - IQR_bilge1$stats[[2]][1]$stats[2] # launch
IQR_bilge1$stats[[3]][1]$stats[4] - IQR_bilge1$stats[[3]][1]$stats[2] # trailer
IQR_bilge1$stats[[4]][1]$stats[4] - IQR_bilge1$stats[[4]][1]$stats[2] #yacht

# overall bilge1 IQR
boxplot.stats(bilge1$hazard_score, coef = 1.5)$stats[4] - boxplot.stats(bilge1$hazard_score, coef = 1.5)$stats[2] 

# Data exploration of risk predictors-------------------------------------

# plot different frequency rate variables
bilge %>%
  gather(key, value, steam_fine:max_rate) %>%
  ggplot(., aes(x = vessel_type, y = value)) +
  geom_boxplot(na.rm = T) +
  facet_wrap( ~ key) +
  scale_y_log10() ## we picked mean rate as representation of all 4 situations

# Boxplot of all predictors and descriptors----------------------
bilge %>%
  gather(key, value, c(2:8, Length, Speed, FreqDischarge)) %>%
  ggplot(., aes(x = vessel_type, y = value)) +
  geom_boxplot() +
  facet_wrap( ~ key, nrow = 2, scales = 'free') +
  theme_javier()

## Risk scores-------------------
## Including zeros
boxplot_risk_score <- 
  ggplot(bilge, aes(x = vessel_type, y = hazard_score )) +
  geom_boxplot() 

ggplot_build(boxplot_risk_score)$data

bilge %>% 
  group_by(vessel_type) %>% 
  summarise_at(vars (hazard_score), funs(median, mean, min, max))


## Excluding zeros
ggplot(bilge1, aes(x = vessel_type, y = hazard_score)) +
  geom_boxplot()

bilge1 %>% 
  group_by(vessel_type) %>% 
  summarise_at(vars (hazard_score), funs(median, mean, min, max))


ggplot(bilge1, aes(x = Speed, y = hazard_score)) + # descriptor
  geom_point(aes(shape = vessel_type, color = vessel_type)) +
  scale_y_log10() +
  geom_smooth(method = 'lm')

ggplot(bilge1, aes(x = Length, y = hazard_score)) +# descriptor
  geom_point(aes(shape = vessel_type, color = vessel_type)) +
  scale_y_log10() +
  geom_smooth(method = 'lm')

ggplot(bilge1, aes(x = storage, y = hazard_score)) + # storage is not considered because of difficulty of assigning risk, so it is a descriptor
  geom_boxplot() +
  scale_y_log10()


