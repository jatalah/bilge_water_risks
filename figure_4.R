library(tidyverse)

# read data -----------
bilge1 <- read_csv('data/cleaned data/bilge1.csv')

# Figure 4: Scatter plots of speed and length vs. risk score-------------
speed_score <- 
  ggplot(bilge1, aes(x = Speed, y = hazard_score)) + # descriptor
  geom_point(aes(shape = vessel_type, color = vessel_type), size = 2) +
  scale_y_log10() +
  geom_smooth(method = 'lm') +
  ylab('Risk score') +
  xlab('Speed (kn)') +
  theme(legend.position = c(0.2,.15), legend.background = element_blank(), legend.key.height=unit(.6,"line")) +
  scale_color_discrete(name = 'Vessel type') +
  scale_shape_manual(values = 15:19, name = 'Vessel type') +
  stat_poly_eq(aes(label = ..rr.label..),formula = y~x,
               parse = TRUE)

length_score <- 
  ggplot(bilge1, aes(x = Length, y = hazard_score)) +# descriptor
  geom_point(aes(shape = vessel_type, color = vessel_type), size = 2) +
  scale_y_log10() +
  geom_smooth(method = 'lm') +
  ylab('Risk score')+
  xlab('Length (m)') +
  scale_color_discrete(name = 'Vessel type', guide = F) +
  scale_shape_manual(guide = F,values = 15:19) +
  stat_poly_eq(aes(label = ..rr.label..),formula = y~x,
               parse = TRUE)

descriptor_scatterplot <- ggarrange(speed_score, length_score, labels = 'auto')
descriptor_scatterplot

ggsave(
  plot = descriptor_scatterplot,
  filename = 'figures/fig5_descriptor_scatterplots.tiff',
  dpi = 600,
  device = "tiff",
  compression = 'lzw',
  width = 6,
  height = 3
)

summary(lm(log(hazard_score)~Speed, bilge1))
summary(lm(log(hazard_score)~Length, bilge1))

