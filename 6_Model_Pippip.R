
library(tidyverse)
library(glmmTMB)
library(ggeffects)

data <- read_delim("../Supplementary_material_3.csv") %>% 
  as.data.frame()

# Response variable
data$buzz_rate <- (data$nb_buzz/(data$duration_sum))*3600 # calculate number of buzzes per hour ; duration_sum = duration of cumulated bat passes
data$buzz_rate <- as.integer(data$buzz_rate)
hist(data$buzz_rate)

# Julian Day
data$jour_j <- yday(data$night)
hist(data$jour_j)

# Select common pipistrelle
data_pippip <- subset(data, data$species=="pippip")


#
# Plots histograms of predictors
# 

setwd("C:/Users/croemer01/Documents/Publications/Bat pass duration")
png(filename=paste("Hist_Bat_pass_duration.png",sep=""), height=1000, width=1500,res=300)
Hist1=ggplot(data_pippip, aes(mean_bat_pass_duration)) +
  geom_histogram(fill = "#003333", binwidth = 1)  +
  labs(x = "Mean bat pass duration (s)",
       y = "Frequency") +
  theme_bw(base_size = 15)
print(Hist1)
dev.off()

setwd("C:/Users/croemer01/Documents/Publications/Bat pass duration")
png(filename=paste("Hist_standard_deviation.png",sep=""), height=1000, width=1500,res=300)
Hist2=ggplot(data_pippip, aes(st_deviation)) +
  geom_histogram(fill = "#009966", binwidth = 1)  +
  labs(x = "Standard deviation of \nthe inter-pass duration (s)",
       y = "Frequency") +
  theme_bw(base_size = 15)
print(Hist2)
dev.off()

#
# Correlation tests
# 

cor.test(log10(data_pippip$mean_bat_pass_duration+1), data_pippip$st_deviation, method = 'pearson')
cor.test(log10(data_pippip$buzz_rate+1), data_pippip$st_deviation, method = 'pearson')
cor.test(log10(data_pippip$buzz_rate+1), log10(data_pippip$mean_bat_pass_duration+1), method = 'pearson')
# cor.test(log10(data_pippip$buzz_rate+1), log10(data_pippip$nb_bat_passes+1), method = 'pearson')
# cor.test(log10(data_pippip$mean_bat_pass_duration+1), log10(data_pippip$nb_bat_passes+1), method = 'pearson')


#
# Compare models
# 

m0_a <- glmmTMB(buzz_rate~ 1 + (1|localite), data = data_pippip, family = "poisson")
summary(m0_a)

m0_b <- glmmTMB(buzz_rate~ 1 + (1|localite), data = data_pippip, family = "nbinom2")
summary(m0_b)

m0_c <- glmmTMB(buzz_rate~ 1 + (1|localite), zi=~1, data = data_pippip, family = "nbinom2")
summary(m0_c)

m1_a <- glmmTMB(buzz_rate~mean_bat_pass_duration  + (1|localite), data = data_pippip, family = "poisson")
summary(m1_a)

m1_b <- glmmTMB(buzz_rate~mean_bat_pass_duration + (1|localite), data = data_pippip, family = "nbinom2")
summary(m1_b)

m1_c <- glmmTMB(buzz_rate~mean_bat_pass_duration  + (1|localite), zi=~1, data = data_pippip, family = "nbinom2")
summary(m1_c)

m2_a <- glmmTMB(buzz_rate~st_deviation +  (1|localite), data = data_pippip, family = "poisson")
summary(m2_a)

m2_b <- glmmTMB(buzz_rate~st_deviation +  (1|localite), data = data_pippip, family = "nbinom2")
summary(m2_b)

m2_c <- glmmTMB(buzz_rate~st_deviation +  (1|localite), zi=~1, data = data_pippip, family = "nbinom2")
summary(m2_c)

m3_a <- glmmTMB(buzz_rate~mean_bat_pass_duration + st_deviation +  (1|localite), data = data_pippip, family = "poisson")
summary(m3_a)

m3_b <- glmmTMB(buzz_rate~mean_bat_pass_duration + st_deviation +  (1|localite), data = data_pippip, family = "nbinom2")
summary(m3_b)

m3_c <- glmmTMB(buzz_rate~mean_bat_pass_duration + st_deviation +  (1|localite), zi=~1, data = data_pippip, family = "nbinom2")
summary(m3_c)

AIC(m0_a, m0_b, m0_c, m1_a, m1_b, m1_c, m2_a, m2_b, m2_c, m3_a, m3_b, m3_c)


# Plot
pr1 = ggpredict(m1_c, "mean_bat_pass_duration")

setwd("C:/Users/croemer01/Documents/Publications/Bat pass duration")
png(filename=paste("Bat_pass_duration_buzz_rate.png",sep=""), height=1000, width=1500,res=300)
plot1=ggplot(pr1, aes(x, predicted)) +
  geom_line(linewidth=1, col = "#003333")  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#003333", alpha = .1)+
  labs(x = "Mean bat pass duration (s)",
       y = "Number of \nfeeding buzzes/hour") +
  scale_fill_discrete(guide=FALSE)+
  scale_y_continuous(trans='log10') +
  theme_bw(base_size = 15)+
  geom_rug(data=data_pippip,aes(x=mean_bat_pass_duration, y=NULL),sides="b")
print(plot1)
dev.off()

#
# Residuals
#

library(DHARMa)

res_m1_c = simulateResiduals(m1_c)
plot(res_m1_c)
