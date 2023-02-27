library(lubridate)
library(tidyverse)
library(viridis)
library(ggthemes)
library(ggmap)
library(plotly)

# Read data
numbats <- read_csv("data/numbats.csv")
load("data/all_locations.rda")
load("data/wa.rda")

# Some basic analysis
numbats %>% filter(year < 2000) %>%
  select(year, decimalLongitude, decimalLatitude) %>%
  arrange(year)

# Locations across the country
ggmap(all_loc) +
  geom_point(data=numbats, 
             aes(x=decimalLongitude, 
                 y=decimalLatitude, 
                 colour=year), 
             alpha=0.4) +
  scale_color_viridis(option="magma") +
  theme_map() +
  theme(legend.position="bottom")
ggplotly()

# Focus on the primary location of numbats
ggmap(wa) +
  geom_point(data=numbats, 
             aes(x=decimalLongitude, 
                 y=decimalLatitude,
                 label=year), alpha=0.4) +
  geom_density2d(data=numbats, 
                 aes(x=decimalLongitude, 
                     y=decimalLatitude) )+
  theme_map()
ggplotly()

# Sighting times
ggplot(numbats, aes(x=hour)) +
  geom_density() +
#  geom_histogram() +
  scale_x_continuous(breaks=seq(0, 24, 2))

numbats %>%
  filter(!is.na(wday)) %>%
  ggplot(aes(x=wday)) +
  geom_bar()
  
numbats %>%
  filter(!is.na(month)) %>%
  ggplot(aes(x=month)) +
  geom_bar()

numbats %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x=year)) +
  geom_bar() +
  xlim(c(2005, 2023))

# Weather related sighting
numbats %>% 
  filter(dryandra == "yes") %>%
  filter(!is.na(prcp)) %>%
  count(prcp > 0)

numbats %>% 
  filter(dryandra == "yes") %>%
  ggplot(aes(x=prcp)) +
  geom_histogram()

numbats %>% 
  filter(dryandra == "yes") %>%
  ggplot(aes(x=prcp, y=tmax, label=dataResourceName)) +
    geom_point()
ggplotly()

numbats %>% 
  filter(dryandra == "yes") %>%
  ggplot(aes(x=tmin, y=tmax, label=dataResourceName)) +
  geom_point()
ggplotly()
