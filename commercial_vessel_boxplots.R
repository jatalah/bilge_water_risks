library(tidyverse)
library(ggpubr)
bilge_com <- read_csv('data/cleaned data/bilge_commercial.csv') %>% 
  drop_na(comm_activity)
bilge_com0 <- 
  bilge_com %>% 
  filter(hazard_score>0)

comm_p1 <- 
  ggplot(bilge_com,
         aes(x = comm_activity , y = hazard_score , fill = comm_activity)) +
  geom_boxplot() +
  labs(x = 'Vessel type', y = 'Risk score') +
  scale_fill_discrete(guide = F) +
  theme_javier() +
  theme(axis.text.x =  element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

comm_p2 <- 
  ggplot(bilge_com0,
         aes(x = comm_activity , y = hazard_score, fill = comm_activity)) +
  geom_boxplot() +
  labs(x = 'Vessel type', y = 'Risk score') +
  scale_fill_discrete(guide = F) +
  theme_javier() +
  theme(axis.text.x =  element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())


comm_boxplots <- ggarrange(comm_p1, comm_p2, labels = 'auto')
comm_boxplots

ggsave(
  plot = comm_boxplots,
  filename = 'figures/fig3_risk_boxplots_commercial.tiff',
  dpi = 600,
  device = "tiff",
  compression = 'lzw',
  width = 6,
  height = 3
)

# version 2----
bilge_com <- 
  read_csv('data/cleaned data/bilge_commercial.csv') %>% 
  drop_na(comm_activity) %>% 
  mutate(comm_act = fct_recode(comm_activity,
                               MC = "Marine construction",
                               CS = "Charter services",
                               DS = "Dive support",
                               `F/A` = "Fishing/aquaculture",
                               `R/C` = "Research/consulting",
                               HP = "Harbour patrol",
                               SS = "Shipping support"
  ))

bilge_com0 <- 
  bilge_com %>% 
  filter(hazard_score>0)

comm_p1 <- 
  ggplot(bilge_com,
         aes(x = comm_act , y = hazard_score , fill = comm_activity)) +
  geom_boxplot() +
  labs(x = 'Vessel type', y = 'Risk score') +
  scale_fill_discrete() +
  theme_javier() +
  theme(axis.text.x =  element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.title = element_blank())

comm_p2 <- 
  ggplot(bilge_com0,
         aes(x = comm_act , y = hazard_score, fill = comm_activity)) +
  geom_boxplot() +
  labs(x = 'Vessel type', y = 'Risk score') +
  scale_fill_discrete() +
  theme_javier() +
  theme(axis.text.x =  element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.title = element_blank())


comm_boxplots <- ggarrange(comm_p1, comm_p2, labels = 'auto', common.legend = T,legend = 'right')
comm_boxplots

ggsave(
  plot = comm_boxplots,
  filename = 'figures/fig3_risk_boxplots_commercial.svg',
  width = 8,
  height = 3
)


ggsave(
  plot = comm_boxplots,
  filename = 'figures/fig3_risk_boxplots_commercial.tiff',
  dpi = 600,
  device = "tiff",
  compression = 'lzw',
  width = 8,
  height = 3
)

