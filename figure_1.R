# load libraries--------------------------
library(tidyverse)
library(ggpubr)

source('theme_javier.R')
theme_set(theme_javier())

# Figure 1. Boxplot of all predictors and descriptors----------------------
bilge <- read_csv('data/cleaned data/bilge.csv')

bilge_long <-
  bilge %>%
  gather(key, value, 2:9) %>%
  mutate(
    key = fct_recode(
      key,
      "A. FreqUse" = "FreqUse" ,
      "B. TripType" = "TripType",
      "C. SourceWater" = "SourceWater",
      "D. RiskyWater" = "RiskyWater",
      "E. MaxVol" = "MaxVol",
      "F. EmptyDepart" = "EmptyDepart",
      "G. EmptyArrival" = "EmptyArrival",
      "H. FreqDischarge" = "FreqDischarge"
    ),
    key = fct_relevel(
      key,
      "A. FreqUse" ,
      "B. TripType" ,
      "C. SourceWater",
      "D. RiskyWater" ,
      "E. MaxVol",
      "F. EmptyDepart" ,
      "G. EmptyArrival" ,
      "H. FreqDischarge"
    ),
    vessel_type = fct_recode(
      vessel_type,
      " Comm." = "Commercial",
      Yacht = "Yacht",
      Launch = "Launch",
      Trailer = "Trailer boat"
    )
  )

boxplot <- 
  ggplot(bilge_long, aes(x = vessel_type, y = value, fill = vessel_type)) +
  geom_boxplot() +
  facet_wrap( ~ key, nrow = 2, scales = 'free') +
  theme_javier() +
  labs (x = 'Vessel type', y = 'Risk factor score') +
  scale_fill_discrete(guide = F)

ggsave(
  plot = boxplot,
  filename = 'figures/fig1_predictors_boxplot.tiff',
  dpi = 600,
  device = "tiff",
  compression = 'lzw',
  width = 8,
  height = 4
)
