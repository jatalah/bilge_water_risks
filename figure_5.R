library(tidyverse)

# read data ----
bilge <- read_csv('data/cleaned data/bilge.csv')

# distance traveled before discharge---------------------------
bilge2 <- 
  bilge %>% 
  mutate(steam_rate = (steam_fine+steam_rough)/2,
         Speed_km = Speed* 1.852,
         time = 1/steam_rate,
         distance = Speed_km*time,
         dist_fine = Speed* 1/steam_fine * 1.852,
         dist_rough = Speed* 1/steam_rough * 1.852,
         time_fine = 1/steam_fine,
         time_rough = 1/steam_rough) %>% 
  filter(vessel_type =='Yacht') %>% 
  write_csv('data/cleaned data/distance_traveled_data.csv')


ggplot(bilge2, aes(x = time)) +
  geom_histogram(na.rm=TRUE, bins = 30)+
  xlab ('hours') +
  geom_vline(xintercept = 24, lty = 2, col = 2) +
  geom_vline(xintercept = 98, lty = 2, col = 3) 


ggplot(bilge2, aes(x = vessel_type, y  = distance)) +
  geom_boxplot() +
  ylab('Travel distance at first discharge (km)')

# read Oli Floerl trip distance data-----
trip_dist <- read_csv('data/cleaned data/trip_dist.csv')
quant <- quantile(trip_dist$dist, probs = c(.5,.9)) %>% data.frame

# read pump modeled survival from Fletcher et al. 2017 ---------
surv <- 
  read_csv('data/cleaned data/pump_surv.csv') %>% 
  filter(Time>6 & Time<72) %>% 
  dplyr::select(Time, p_surv, p_surv_se, sp) %>% 
  arrange(Time) %>% 
  mutate(p_surv = p_surv *100,
         p_surv_se = p_surv_se*100)

# Sketch figure 5 Relationship between time to first bilge water discharge and theoretical distance from home port that the first discharge occurs----
spatial_risk <- 
  ggplot(bilge2, aes(x = time, y = distance)) +
  geom_point(position = position_jitter(width = 1),
             alpha = .7,
             size = 4) +
  geom_vline(xintercept = 24,
             lty = 3,
             col = 2) +
  xlab('Time to first discharge (hrs)') +
  ylab('Distance to first discharge (km)') +
  geom_smooth(method = 'lm') +
  geom_hline(yintercept = quant$.,
             lty = 3,
             col = 1) +
  geom_vline(xintercept = c(12, 24, 48),
             lty = 3,
             col = 1) +
  scale_x_continuous(limits = c(0, 60), breaks = c(0, 12, 24, 48)) +
  scale_y_continuous(limits = c(0, 800)) +
  annotate(
    geom = 'text',
    x = 12.5,
    y = 750,
    label = 'Ciona = 43.9 \u00B1 7.7% \nDidemnum = 31.5 ± 7.1% \nBugula <1%',
    size = 3,
    hjust = 0
  ) +
  annotate(
    geom = 'text',
    x = 24.5,
    y = 750,
    label = 'Ciona = 38.9 \u00B1 7.4% \nDidemnum = 25.2 \u00B1 6.5% \nBugula <1%',
    size = 3,
    hjust = 0
  ) +
  annotate(
    geom = 'text',
    x = 48.5,
    y = 750,
    label = 'Ciona = 29.7 \u00B1 8.5% \nDidemnum = 15.4 ± 6.3%\nBugula <1%',
    size = 3,
    hjust = 0
  ) +
  annotate(
    geom = 'text',
    x = 58,
    y = 110,
    label = '50% of voyages',
    size = 3
  ) +
  annotate(
    geom = 'text',
    x = 58,
    y = 290,
    label = '90% of voyages',
    size = 3
  )
spatial_risk

ggsave(spatial_risk,
       filename = 'figures/spatial_risk.tiff',
       device = 'tiff',
       compression = 'lzw',
       width = 9,
       height = 5)


ggsave(spatial_risk,
       filename = 'figures/spatial_risk.svg',
       device = 'svg',
       width = 9,
       height = 5)
