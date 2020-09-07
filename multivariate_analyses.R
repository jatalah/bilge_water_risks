# 01 load libraries ----------------
library(tidyverse)
library(vegan)
library(ggord)
source('theme_javier.R')
theme_set(theme_javier())

bilge <- read_csv('data/cleaned data/bilge.csv')

# Boxplot of all predictors and descriptors----------------------
bilge %>%
  gather(key, value, c(2:8, Length, Speed, FreqDischarge)) %>%
  ggplot(., aes(x = vessel_type, y = value)) +
  geom_boxplot() +
  facet_wrap( ~ key, nrow = 2, scales = 'free') +
  theme_javier()

# PCA of predictors
pca <- prcomp(bilge %>% dplyr::select(c(2:8, Length, Speed,FreqDischarge)), scale = T, center = T)
saveRDS(pca, 'results/pca.RDS')

ggord(
  pca,
  grp_in = bilge$vessel_type,
  poly = F,
  alpha = 1,
  ellipse = T,
  arrow = .3,
  repel = T,
  vec_ext = 5
) +
  theme_javier() +
  scale_shape_manual(values = 15:19) 

##PERMANOVA--------
perma_dat <-
  bilge %>%
  dplyr::select(c(2:8, Length, Speed, FreqDischarge, vessel_type))

perma_dat <-
  scale(dplyr::select(bilge, c(2:8, Length, Speed, FreqDischarge))) %>%
  data.frame() %>%
  bind_cols(., dplyr::select(bilge, vessel_type))

adonis(perma_dat[, 1:10] ~ vessel_type, data = perma_dat, method = 'euclidean')

# Pair-wise comparisons
source('pairwise.adonis.R')

pairwise.adonis(
  perma_dat[, 1:10],
  perma_dat$vessel_type,
  sim.method = 'euclidean',
  p.adjust.m
  = 'bonferroni'
)
