setwd("/Data")

library(tidyr)
library(dplyr) 
library(ggplot2)

data_trackrev <- read.csv("Tracking.csv", sep =";", header = T)

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