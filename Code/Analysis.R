# Load libraries
library(dplyr)
library(tidyr)

# Load the dataset
data_trackrev <- read.csv("./Data/Tracking.csv", sep =";", header = T)

#### Section 1: Transmitter Frequency by Year ####
frequency_trasm_year <- data_trackrev %>%          # Calculate the frequency of distinct Titles by Year and Transmitter type
  distinct(Title, Transmitters, .keep_all = TRUE) %>%
  group_by(Year, Transmitters) %>%
  summarise(Frequency = n()) %>%
  spread(Transmitters, Frequency, fill = 0)

# Summing up the total number of occurrences for different transmitter types
sum_radio_telemetry <- sum(frequency_trasm_year$`Radio-telemetry`)
sum_GPS <- sum(frequency_trasm_year$GPS)
sum_HDF <- sum(frequency_trasm_year$HDF)

#### Section 2: Frequency by Continents and Class ####
frequency_cont_class <- data_trackrev %>%     # Calculate the frequency of distinct Titles by geographic location, Continents, and Class
  distinct(Title, .keep_all = TRUE) %>%
  group_by(Long, Lat, Continents, Class) %>%
  summarise(Frequency = n()) %>%
  spread(Class, Frequency, fill = 0)

# Summing the occurrences for amphibians and reptiles across continents
sum_amphibians_cont <- sum(frequency_cont_class$Amphibians)
sum_reptiles_cont <- sum(frequency_cont_class$Reptiles)

#### Section 3: IUCN Classification ####
data_specie_continent <- data_trackrev %>%   # Subset data with distinct species and continents
  distinct(Species, Continents, .keep_all =TRUE)

frequency_IUCN_cont <- data_specie_continent %>%    # Calculate the frequency of species by IUCN classification across continents
  group_by(Long, Lat, Continents, IUCN) %>%
  summarise(Frequency = n()) %>%
  spread(IUCN, Frequency, fill = 0)

# Adapt the IUCN frequency table with specific values taken from https://www.iucnredlist.org/
frequency_table_totIUCN <- frequency_IUCN_cont
frequency_table_totIUCN$CR <- c(403, 375, 11, 142, 252 , 74)
frequency_table_totIUCN$DD <- c(251, 579, 3, 417, 1052 , 183)
frequency_table_totIUCN$EN <- c(494, 601,23, 323, 551 , 125)
frequency_table_totIUCN$LC <- c(1978, 2711,164, 2080, 3357, 1597)
frequency_table_totIUCN$LR <- c(1, 2,0, 0, 0, 0)
frequency_table_totIUCN$Nas <- c(0, 0, 0, 0, 0, 0)
frequency_table_totIUCN$NT <- c(204, 239, 24, 205, 319, 66)
frequency_table_totIUCN$VU <- c(282, 418, 26, 234, 441, 113)

# Calculate proportions for IUCN classification frequencies
frequency_table_prop <- frequency_IUCN_cont
frequency_table_prop[, -c(1:3)] <- frequency_table_prop[, -c(1:3)] / rowSums(frequency_table_prop[, -c(1:3)])

# Calculate the proportion of total IUCN values for comparison
frequency_table_totIUCN_prop <- frequency_table_totIUCN
frequency_table_totIUCN_prop[, -c(1:3)] <- frequency_table_totIUCN_prop[, -c(1:3)] / rowSums(frequency_table_totIUCN_prop[, -c(1:3)], na.rm=TRUE)

# Calculate the proportion of tracked species relative to IUCN data
prop_tracked <- frequency_table_prop[, -c(1:3)] / frequency_table_totIUCN_prop[, -c(1:3)] 

#### Section 4: Transmitter Use by Class ####
data_specie_transm <- data_trackrev %>%    # Create a contingency table of Transmitters by Class
  distinct(Title, Species, Transmitters, .keep_all = TRUE)
contingency_table_transm_class <- table(data_specie_transm$Transmitters, data_specie_transm$Class)

# Perform chi-square test for "HDF" transmitters by Class
hdf_table <- contingency_table_transm_class["HDF", ]
chisq.test(hdf_table)

#### Section 5: Brand Analysis ####
data_tit_brand <- data_trackrev %>%    # Calculate the frequency of different transmitter brands
  distinct(Title, Brand_transmitter, .keep_all = TRUE)

brand_frequencies <- as.data.frame(table(data_tit_brand$Brand_transmitter))

#### Section 6: Transmitter-to-Animal Weight Frequency ####
data_title_trans.anim <- data_trackrev %>%   # Calculate frequency of transmitter-to-animal weight ratio categories
  distinct(Title, Transmitter.Animal, .keep_all = TRUE)
rate_transm_frequencies <- as.data.frame(table(data_title_trans.anim$Transmitter.Animal))

#### Section 7: Attachment Method by Class ####
data_title_attach <- data_trackrev %>%    # Cross-tabulate Attachment method by Class
  distinct(Title, Attachment_method, .keep_all = TRUE)
attach_class_frequencies<- xtabs(~ Attachment_method + Class, data = data_title_attach)

##### Section 8: Control Test ####
data_title_controlltest <- data_trackrev %>%  # Calculate frequencies of control tests performed
  distinct(Title, Controll_Test, .keep_all = TRUE)
table(data_title_controlltest$Controll_Test)

#### Section 9: Sample Size Analysi ####
data_tit_samplesize <- data_trackrev %>%
  distinct(Title, Sample_size_tot, .keep_all = TRUE)
summary_stats <- data_tit_samplesize %>%       # Calculate basic summary statistics for sample size
  summarise(
    Sample_size_mean = mean(Sample_size_tot, na.rm = TRUE),
    Sample_size_min = min(Sample_size_tot, na.rm = TRUE),
    Sample_size_max = max(Sample_size_tot, na.rm = TRUE)
  )

# Compute sample size mean for each title and perform statistical tests
data_tit_samplesize <- data_tit_samplesize %>%
  group_by(Title) %>%
  mutate(Sample_size_mean = mean(Sample_size_tot, na.rm = TRUE))
data_tit_samplesize <- data_tit_samplesize %>%
  distinct(Title, Sample_size_mean, .keep_all = TRUE)

shapiro.test(data_tit_samplesize$Sample_size_mean)
wilcox.test(Sample_size_mean ~ Class, data = data_tit_samplesize)

##### Section 10: Tracking Days Analysis ####
data_tit_daytrack <- data_trackrev %>%     # Compute mean tracking days for each title
  distinct(Title, Tracking_days, .keep_all = TRUE)  %>% 
  group_by(Title) %>%
  mutate(Tracking_days_mean = mean(Tracking_days, na.rm = TRUE))  %>% 
  distinct(Title, Tracking_days_mean, .keep_all = TRUE)

# Summary statistics for tracking days
mean(data_tit_daytrack$Tracking_days_mean, na.rm = TRUE)
min(data_tit_daytrack$Tracking_days_mean, na.rm = TRUE)
max(data_tit_daytrack$Tracking_days_mean, na.rm = TRUE)

# Statistical tests for tracking days
shapiro.test(data_tit_daytrack$Tracking_days_mean)
wilcox.test(Tracking_days_mean ~ Class, data = data_tit_daytrack)

##### Section 11: Missing Data for Mortality and Injury Rates ####
data_tit <- data_trackrev %>%      # Count missing values for injury and mortality rates
  distinct(Title, .keep_all = TRUE)
sum(is.na(data_tit$Injury_rate))
sum(is.na(data_tit$Mortality_rate))

# Count non-zero, non-missing values for mortality rate
non_zero_non_na_count <- sum(data_tit$Mortality_rate != 0 & !is.na(data_tit $Mortality_rate))

#### Section 12: Correlation Between Transmitter Weight Ratio and Mortality ####
data_tit_trans.anim <- data_trackrev %>%    # Calculate correlation between Transmitter-to-animal weight ratio and mortality rate
  distinct(Title, Transmitter.Animal, .keep_all = TRUE)
cor(data_tit_trans.anim$Transmitter.Animal, data_tit_trans.anim$Mortality_rate, use = "complete.obs")

#### Section 13: Mortality Rate by Family ####
# Perform ANOVA to assess differences in mortality rate across species families
data_tit_specie <- data_trackrev %>%
  distinct(Title, Species, .keep_all = TRUE)
anova_mor_fam <- aov(Mortality_rate ~ Family, data = data_tit_specie)
summary(anova_mor_fam)

#### Section 14: Device Loss Rate Analysis ####
# Calculate summary statistics for device loss rates and handle missing data
data_tit_lostdevice <- data_trackrev %>%  # Remove duplicate Titles and summarize the device loss rate for each Title
  distinct(Title, Device_loss_rate, .keep_all = TRUE)

# Group by Title and calculate the average device loss rate, ignoring missing values (NA)
summary_data_tit_lostdevice <- data_tit_lostdevice %>%
  group_by(Title) %>%
  summarise(Device_loss_rate_tot = mean(Device_loss_rate, na.rm = TRUE))

na_count <- sum(is.na(summary_data_tit_lostdevice$Device_loss_rate_tot)) # Count the number of missing values in the summarized device loss rates
non_na_non_zero_count <- sum(!is.na(summary_data_tit_lostdevice$Device_loss_rate_tot) & summary_data_tit_lostdevice$Device_loss_rate_tot != 0) # Count the number of non-missing and non-zero device loss rates
mean_device_loss_rate_tot <- mean(summary_data_tit_lostdevice$Device_loss_rate_tot, na.rm = TRUE) # Calculate the mean device loss rate (ignoring missing values)
mean_device_loss_rate_tot_nonzero <- mean(summary_data_tit_lostdevice$Device_loss_rate_tot[summary_data_tit_lostdevice$Device_loss_rate_tot != 0], na.rm = TRUE) # Calculate the mean device loss rate, but only for non-zero values

# Perform an ANOVA to test if the device loss rate varies significantly by attachment method
anova_lost_attach <- aov(Device_loss_rate ~ Attachment_method, data = data_title_attach)
summary(anova_lost_attach)
TukeyHSD(anova_lost_attach) # Perform post-hoc Tukey's Honest Significant Difference (HSD) test to compare pairs of attachment methods

# Fit a linear model to predict device loss rate based on the number of tracking days
mod_lost_day <- lm(Device_loss_rate ~ Tracking_days, data = data_title_attach)
summary(mod_lost_day)

