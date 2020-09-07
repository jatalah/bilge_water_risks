library(tidyverse)

# Figure 3. Risks scores by vessel type--------------------------------
bilge <- read_csv('data/cleaned data/bilge.csv')
bilge1 <- read_csv('data/cleaned data/bilge1.csv')

dat_fig3a <-
  bilge %>%
  mutate(
    vessel_type = fct_recode(
      vessel_type,
      " Comm." = "Commercial",
      Yacht = "Yacht",
      Launch = "Launch",
      Trailer = "Trailer boat"
    )
  )

dat_fig3b <-
  bilge1 %>%
  mutate(
    vessel_type = fct_recode(
      vessel_type,
      " Comm." = "Commercial",
      Yacht = "Yacht",
      Launch = "Launch",
      Trailer = "Trailer boat"
    )
  )


risk_boxplot0 <-
  ggplot(dat_fig3a, aes(x = vessel_type, y = hazard_score, fill = vessel_type)) +
  geom_boxplot() +
  labs(x = 'Vessel type', y = 'Risk score') +
  scale_fill_discrete(guide = F) 


## Excluding zeros
risk_boxplot1 <-
  ggplot(dat_fig3b, aes(x = vessel_type, y = hazard_score, fill = vessel_type)) +
  geom_boxplot() +
  labs(x = 'Vessel type', y = 'Risk score') +
  scale_fill_discrete(guide = F) 


risk_boxplots <- ggarrange(risk_boxplot0, risk_boxplot1, labels = 'auto')

ggsave(
  plot = risk_boxplots,
  filename = 'figures/fig3_risk_boxplots.tiff',
  dpi = 600,
  device = "tiff",
  compression = 'lzw',
  width = 6,
  height = 3
)
