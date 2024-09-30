
# Load libraries
library(tidyr)

data_trackrev <- read.csv("./Data/Tracking.csv", sep =";", header = T)

#Year
frequency_trasm_year <- data_trackrev %>%
  distinct(Title, Transmitters, .keep_all = TRUE) %>%
  group_by(Year, Transmitters) %>%
  summarise(Frequency = n()) %>%
  spread(Transmitters, Frequency, fill = 0)

sum_radio_telemetry <- sum(frequency_trasm_year$`Radio-telemetry`)
sum_GPS <- sum(frequency_trasm_year$GPS)
sum_HDF <- sum(frequency_trasm_year$HDF)

#Continents
frequency_cont_class <- data_trackrev %>%
  distinct(Title, .keep_all = TRUE) %>%
  group_by(Long, Lat, Continents, Class) %>%
  summarise(Frequency = n()) %>%
  spread(Class, Frequency, fill = 0)

sum_amphibians_cont <- sum(frequency_cont_class$Amphibians)
sum_reptiles_cont <- sum(frequency_cont_class$Reptiles)

#IUCN
data_specie_continent <- data_trackrev %>%
  distinct(Species, Continents, .keep_all =TRUE)

frequency_IUCN_cont <- data_specie_continent %>%
  group_by(Long, Lat, Continents, IUCN) %>%
  summarise(Frequency = n()) %>%
  spread(IUCN, Frequency, fill = 0)

frequency_table_totIUCN <- frequency_IUCN_cont
frequency_table_totIUCN$CR <- c(403, 375, 11, 142, 252 , 74)
frequency_table_totIUCN$DD <- c(251, 579, 3, 417, 1052 , 183)
frequency_table_totIUCN$EN <- c(494, 601,23, 323, 551 , 125)
frequency_table_totIUCN$LC <- c(1978, 2711,164, 2080, 3357, 1597)
frequency_table_totIUCN$LR <- c(1, 2,0, 0, 0, 0)
frequency_table_totIUCN$Nas <- c(0, 0, 0, 0, 0, 0)
frequency_table_totIUCN$NT <- c(204, 239, 24, 205, 319, 66)
frequency_table_totIUCN$VU <- c(282, 418, 26, 234, 441, 113)

frequency_table_prop <- frequency_IUCN_cont
frequency_table_prop[, -c(1:3)] <- frequency_table_prop[, -c(1:3)] / rowSums(frequency_table_prop[, -c(1:3)])

frequency_table_totIUCN_prop <- frequency_table_totIUCN
frequency_table_totIUCN_prop[, -c(1:3)] <- frequency_table_totIUCN_prop[, -c(1:3)] / rowSums(frequency_table_totIUCN_prop[, -c(1:3)], na.rm=TRUE)

prop_tracked <- frequency_table_prop / frequency_table_totIUCN_prop

#Order
data_specie_transm <- data_trackrev %>%
  distinct(Title, Species, Transmitters, .keep_all = TRUE)
contingency_table_transm_class <- table(data_specie_transm$Transmitters, data_specie_transm$Class)
hdf_table <- contingency_table_transm_class["HDF", ]
chisq.test(hdf_table)

#Brand
data_tit_brand <- data_trackrev %>%
  distinct(Title, Brand_transmitter, .keep_all = TRUE)

brand_frequencies <- as.data.frame(table(data_tit_brand$Brand_transmitter))

#Device/Animal weight freq
data_title_trans.anim <- data %>%
  distinct(Title, Transmitter.Animal, .keep_all = TRUE)
rate_transm_frequencies <- as.data.frame(table(data_title_trans.anim$Transmitter.Animal))

#Attachment method
data_title_attach <- data_trackrev %>%
  distinct(Title, Attachment_method, .keep_all = TRUE)
attach_class_frequencies<- xtabs(~ Attachment_method + Class, data = data_title_attach)

#Controll test
data_title_controlltest <- data_trackrev %>%
  distinct(Title, Controll_Test, .keep_all = TRUE)
table(data_title_controlltest$Controll_Test)

#Sample size
data_tit_samplesize <- data_trackrev %>%
  distinct(Title, Sample_size_tot, .keep_all = TRUE)
summary_stats <- data_tit_samplesize %>%
  summarise(
    Sample_size_mean = mean(Sample_size_tot, na.rm = TRUE),
    Sample_size_min = min(Sample_size_tot, na.rm = TRUE),
    Sample_size_max = max(Sample_size_tot, na.rm = TRUE)
  )
data_tit_samplesize <- data_tit_samplesize %>%
  group_by(Title) %>%
  mutate(Sample_size_mean = mean(Sample_size_tot, na.rm = TRUE))
data_tit_samplesize <- data_tit_samplesize %>%
  distinct(Title, Sample_size_mean, .keep_all = TRUE)
shapiro.test(data_tit_samplesize$Sample_size_mean)
wilcox.test(Sample_size_mean ~ Class, data = data_tit_samplesize)

#Tracking days
data_tit_daytrack <- data_trackrev %>%
  distinct(Title, Tracking_days, .keep_all = TRUE)
data_tit_daytrack <- data_tit_daytrack %>%
  group_by(Title) %>%
  mutate(Tracking_days_mean = mean(Tracking_days, na.rm = TRUE))
data_tit_daytrack <- data_tit_daytrack %>%
  distinct(Title, Tracking_days_mean, .keep_all = TRUE)
mean(data_tit_daytrack$Tracking_days_mean, na.rm = TRUE)
min(data_tit_daytrack$Tracking_days_mean, na.rm = TRUE)
max(data_tit_daytrack$Tracking_days_mean, na.rm = TRUE)
shapiro.test(data_tit_daytrack$Tracking_days_mean)
wilcox.test(Tracking_days_mean ~ Class, data = data_tit_daytrack)

#NA data
data_tit <- data_trackrev %>%
  distinct(Title, .keep_all = TRUE)
sum(is.na(data_tit$Injury_rate))
sum(is.na(data_tit$Mortality_rate))
non_zero_non_na_count <- sum(data_tit$Mortality_rate != 0 & !is.na(data_tit $Mortality_rate))

#Correlation between and transmitter-to-animal weight ratio
data_tit_trans.anim <- data_trackrev %>%
  distinct(Title, Transmitter.Animal, .keep_all = TRUE)
cor(data_tit_trans.anim$Transmitter.Animal, data_tit_trans.anim$Mortality_rate, use = "complete.obs")

#Mortality rate and family
data_tit_specie <- data_trackrev %>%
  distinct(Title, Species, .keep_all = TRUE)
anova_mor_fam <- aov(Mortality_rate ~ Family, data = data_tit_specie)
summary(anova_mor_fam)

#Lost device
data_tit_lostdevice <- data_trackrev %>%
  distinct(Title, Device_loss_rate, .keep_all = TRUE)
summary_data_tit_lostdevice <- data_tit_lostdevice %>%
  group_by(Title) %>%
  summarise(Device_loss_rate_tot = mean(Device_loss_rate, na.rm = TRUE))
na_count <- sum(is.na(summary_data_tit_lostdevice$Device_loss_rate_tot))
non_na_non_zero_count <- sum(!is.na(summary_data_tit_lostdevice$Device_loss_rate_tot) & summary_data_tit_lostdevice$Device_loss_rate_tot != 0)
mean_device_loss_rate_tot <- mean(summary_data_tit_lostdevice$Device_loss_rate_tot, na.rm = TRUE)
mean_device_loss_rate_tot_nonzero <- mean(summary_data_tit_lostdevice$Device_loss_rate_tot[summary_data_tit_lostdevice$Device_loss_rate_tot != 0], na.rm = TRUE)

anova_lost_attach <- aov(Device_loss_rate ~ Attachment_method, data = data_title_attach)
summary(anova_lost_attach)
TukeyHSD(anova_lost_attach)

mod_lost_day <- lm(Device_loss_rate ~ Tracking_days, data = data_title_attach)
summary(mod_lost_day)

