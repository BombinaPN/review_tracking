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

#Brand
data_tit_brand <- data_trackrev %>%
  distinct(Title, Brand_transmitter, .keep_all = TRUE)

brand_frequencies <- as.data.frame(table(data_tit_brand$Brand_transmitter))

#Device/Animal weight freq
data_titolo_trans.anim <- data %>%
  distinct(Title, Transmitter.Animal, .keep_all = TRUE)

rate_transm_frequencies <- as.data.frame(table(data_titolo_trans.anim$Transmitter.Animal))

