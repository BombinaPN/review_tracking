#negessary libraries to generate figure 2 and figure 4
library(tidyr)
library(dplyr) 
library(ggplot2)

#libraries TO BE ADDED to the previous ones to create figure 3
library(sf)
library(patchwork)
library(purrr)
library(scatterpie)

data_trackrev <- read.csv("./Data/Tracking.csv", sep =";", header = T)

####Figure 2####
data_transmitter <- data_trackrev %>%
  distinct(Title, Transmitters, .keep_all = TRUE)

min_year <- min(data_transmitter$Year)
data_transmitter$Year_Group <- cut(data_transmitter$Year, breaks = seq(min_year - 0.01, max(data_transmitter$Year) + 5, by = 5))

empty_period <- data.frame(
  Year_Group = cut(seq(1976, 1980, by = 1), breaks = seq(1970, 1985, by = 5)),
  Transmitters = "NA_value"
)

data_transmitter <- bind_rows(data_transmitter, empty_period)

ggplot(data_transmitter, aes(x = Year_Group, fill = Transmitters)) +
  geom_bar(position = "stack") +
  labs(x = "Years",
       y = "Frequency",
       fill = "Transmitters") + 
  scale_fill_manual(values = c("GPS" = "#ddcc77ff", "HDF" = "#CC6677", "Radio-telemetry" = "#44AA99", "NA_value" = alpha("white", 0)),
                    breaks = c("GPS", "HDF", "Radio-telemetry"),
                    labels = c("GPS", "HDF", "Radio-transmitter")) +  
  scale_x_discrete(labels = c("1970-1974", "1975-1979", "1980-1984", "1985-1989", "1990-1994", 
                              "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2023")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 35, vjust = 1.3, hjust = 0.8, size=10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 14))

####Figure 3####
world_map <- map_data("world")

frequency_table <- data %>%
  distinct(Title, .keep_all = TRUE) %>%
  group_by(Long, Lat, Continents, Class) %>%
  summarise(Frequency = n()) %>%
  spread(Class, Frequency, fill = 0)

frequency_table <- frequency_table %>%
  mutate(Tot = Amphibians + Reptiles)
frequency_table <- frequency_table %>%
  mutate(radius = 50*(Tot / sum(frequency_table$Tot)))

p <- ggplot(world_map, aes(long, lat)) +
  geom_map(map=world_map, aes(map_id=region), fill="lightgray", color="white") +
  coord_quickmap()
p + geom_scatterpie(aes(x=Long, y=Lat, group=Continents, r=radius),
                    data=frequency_table, cols=c("Amphibians", "Reptiles"), color="black", alpha=0.8) +
  geom_scatterpie_legend(frequency_table$radius, x=-170, y=-50, n=4, labeller = function(x) round(x/50 *sum(frequency_table$Tot)))+
  scale_fill_manual(values = c("Amphibians" = "#cc6677", "Reptiles" = "#88ccee")) + 
  theme_void()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())

####Figure 4####
data_species_transm <- data_trackrev %>%
  distinct(Title, Species, Transmitters, .keep_all = TRUE)

xtabs(~ Order + Transmitters, data = data_species_transm) 
order_ord <- c("Anura", "Urodela", "Crocodilia", "Rhynchocephalia", "Squamata", "Testudines")

data_species_transm <- mutate(data_species_transm, Order = factor(Order, levels = order_ord))

ggplot(data_species_transm, aes(x = Order, fill = Transmitters)) +
  geom_bar(position = "stack") +
  labs(x = "Orders",
       y = "Frequency") +
  scale_fill_manual(values = c("GPS" = "#ddcc77ff", "HDF" = "#CC6677", "Radio-telemetry" = "#44AA99", "NA_value" = alpha("white", 0)),
                    breaks = c("GPS", "HDF", "Radio-telemetry"),
                    labels = c("GPS", "HDF", "Radio-transmitter")) +
  ylim(0, 100) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(vjust = 3.5,size=12),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 14))
