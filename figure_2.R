library(tidyverse)
bilge <- read_csv('data/cleaned data/bilge.csv')
source('theme_javier.R')
theme_set(theme_javier())

# Figure 2. PCA-------------------------------------------------------------- 
pca <- readRDS('results/pca.RDS')

pca_biplot <-
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

ggsave(
  plot = pca_biplot,
  filename = 'figures/fig2_pca_biplot.tiff',
  dpi = 600,
  device = "tiff",
  compression = 'lzw'
)
