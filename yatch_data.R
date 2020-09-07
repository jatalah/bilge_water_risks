##Oli's yacht data-----------
library(readxl)
library(tidyverse)
library(forcats)
library(stringr)

#read data-----------
trip_dist <- 
  read_excel('data/Oli_yacht data.xls', 2) %>%
  dplyr::filter(type == "DOM") %>% # filter domestic trips
  mutate(key = str_extract(id, "[[:digit:]]+"))  %>% # get numeric vessel key
  write_csv('data/cleaned data/trip_dist.csv') 

no_trips <-
  read_excel('data/Oli_yacht data.xls', 3) %>%
  mutate(key = str_extract(ID, "[[:digit:]]+"))

all_yacht <-
  full_join(trip_dist, no_trips, by = 'key') %>%
  write_csv('data/cleaned data/all_yacht.csv') 

  
hist(trip_dist$dist)
dotchart(trip_dist$dist)
summary(trip_dist$dist)
quant <- quantile(trip_dist$dist, probs = c(0,.5, .9,.1)) %>% data.frame
quant


         