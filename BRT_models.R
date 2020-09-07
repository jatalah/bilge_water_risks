# load libraries ----------------
library(tidyverse)
library(gbm)
library(plotmo)
source('theme_javier.R')
theme_set(theme_javier())

# read data -----------
bilge <- read_csv('data/cleaned data/bilge.csv')
bilge1 <- read_csv('data/cleaned data/bilge1.csv')

# BRT model 1---------------------------------------------
#a boosted regression tree on the log hazard score in relation to 8 predictors of risk and 2 additional vessel descriptors-------------------


##Model 1 with all vessels, i.e. including zero hazard scores
set.seed(123)
hist(
  log(bilge$hazard_score + 1),
  breaks = 15,
  main = '',
  xlab = 'Log Hazard score + 1 all vessels',
  col = 'gray90'
)

gbm0 <-
  gbm(
    log(hazard_score+1) ~ 
      FreqUse + 
      Length + #descriptor
      Speed + #descriptor
      TripType + 
      SourceWater +
      RiskyWater + 
      MaxVol + 
      EmptyDepart + 
      EmptyArrival +
      FreqDischarge,
    interaction.depth = 3,
    n.minobsinnode = 10,
    shrinkage = 0.001,
    bag.fraction = 0.5,
    n.trees = 1000,
    cv.folds = 3,
    data = bilge
  )

## GBM all vessel model results 
best.iter0 <- gbm.perf(gbm0, method = 'cv')
print(best.iter0)
saveRDS(gbm0, 'results/gbm0.RDS')

par(mar = c(5, 7, 3, 1))
model0 <- 
  summary(gbm0,
          n.trees = best.iter0,
          las = 2,
          main = 'All vessels model')

model0 <-
  model0 %>%
  data.frame()

brt0_plot <- 
  ggplot(model0,aes(x= var, y = rel.inf, fill = var)) +
  geom_col(color = 1) +
  coord_flip() +
  labs(x = 'Risk variables', y = 'Relative influence (%)') +
  ggtitle('a. All vessel model') +
  scale_fill_viridis_d( guide = F)

# marginal effect plots for each risk factor on their hazard score
plotmo(
  gbm0,
  ylim = NA,
  all1 = T,
  caption = '',
  degree2 = F,
  mfrow =c(3,4),
  pmethod = 'plotmo'
)

# explore 2-way interactons--------------------------------------------- 
plotmo(
  gbm0,
  caption = '',
  degree1 = F,
  degree2 = c(1:6),
  main = '',
  mfrow =c(2,3),
  persp.col=rev(heat.colors(100))
)

##GBM model 2 only with vessel with hazrd scores >0 
set.seed(123)
hist(
  log(bilge1$hazard_score + 1),
  breaks = 15,
  main = '',
  xlab = 'Log Hazard score + 1 risky vessels',
  col = "gray90"
)

gbm1 <-
  gbm(
    log(hazard_score+1) ~ 
      FreqUse + 
      Length + #descriptor
      Speed + #descriptor
      TripType + 
      SourceWater +
      RiskyWater + 
      MaxVol + 
      EmptyDepart + 
      EmptyArrival +
      FreqDischarge,
    interaction.depth = 3,
    n.minobsinnode = 10,
    shrinkage = 0.001,
    bag.fraction = 0.5,
    n.trees = 1000,
    cv.folds = 3,
    data = bilge1
  )

best.iter1 <- gbm.perf(gbm1, method = 'cv')
print(best.iter1)
saveRDS(gbm1, 'results/gbm1.RDS')

par(mar = c(5, 7, 3, 1))
model1 <- 
  summary(gbm1,
          n.trees = best.iter1,
          las = 2,
          main = "Risky vessels model")

model1 <- 
  model1 %>% 
  data.frame() 

write_csv(model1,'data/cleaned data/model1.csv')
write_csv(model0,'data/cleaned data/model0.csv')

brt1_plot <- 
  ggplot(model1,aes(x= var, y = rel.inf, fill = var)) +
  geom_col(color = 1) +
  coord_flip() +
  labs(x = 'Risk variables', y = 'Relative influence (%)') +
  ggtitle('b. Risky vessel model') +
  scale_fill_viridis_d(guide = F)

# GBM marginal plots---------------------------------------------
# marginal effect plots for each risk factor on their hazard score
plotmo(
  gbm1,
  ylim = NA,
  all1 = T,
  caption = '',
  degree2 = F,
  mfrow =c(3,4),
  pmethod = 'plotmo'
)

# explore 2-way interactons---------------------------------------------
plotmo(
  gbm1,
  caption = '',
  degree1 = F,
  degree2 = c(1:6),
  main = '',
  mfrow =c(2,3),
  persp.col=rev(heat.colors(60))
)

summary(lm(log(bilge$hazard_score+1)~predict(gbm0)))
summary(lm(log(bilge1$hazard_score+1)~predict(gbm1)))


# Attempt to orginise 2 x 2 paner with interaction and rel. imp.------
gbm0 <- readRDS('results/gbm0.RDS')
gbm1 <- readRDS('results/gbm1.RDS')


par(mfrow = c(2, 2))
plotmo(
  gbm0,
  caption = '',
  degree1 = F,
  degree2 = c(1:6),#pairs of the four variables with the largest relative influence (thus six plots)
  main = '',
  mfrow = c(3, 2),
  persp.col = rev(heat.colors(100)))

plotmo(
  gbm1,
  caption = '',
  degree1 = F,
  degree2 = c(1:6),#pairs of the four variables with the largest relative influence (thus six plots)
  main = '',
  mfrow = c(3, 2),
  persp.col = rev(heat.colors(60)))

plotmo(
  gbm1,
  caption = '',
  degree1 = F,
  degree2 = c(1:6),
  main = '',
  mfrow = c(4, 3),
  persp.col = rev(heat.colors(100)), 
  do.par = T
)

#Figure BRT plots--------------------------------------------------
model0 <- read_csv('data/cleaned data/model0.csv')
model1 <- read_csv('data/cleaned data/model1.csv')

brt0_plot <- 
  ggplot(model0,aes(x= var, y = rel.inf, fill = var)) +
  geom_col(color = 1) +
  coord_flip() +
  labs(x = 'Risk variables', y = 'Relative influence (%)') +
  ggtitle('b. All vessel model') +
  scale_fill_viridis(discrete = T , guide = F)

brt1_plot <- 
  ggplot(model1,aes(x= var, y = rel.inf, fill = var)) +
  geom_col(color = 1) +
  coord_flip() +
  labs(x = 'Risk variables', y = 'Relative influence (%)') +
  ggtitle('b. Risky vessel model') +
  scale_fill_viridis(discrete = T , guide = F)

brt_rel_imp_plots <- ggarrange(brt0_plot, brt1_plot)

ggsave(
  plot = brt_rel_imp_plots,
  filename = 'figures/fig4_brt_rel_imp_plots.tiff',
  dpi = 600,
  device = "tiff",
  compression = 'lzw',
  width = 8,
  height = 6
)