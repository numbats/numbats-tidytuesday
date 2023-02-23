library(galah) # API to ALA
library(lubridate)
library(tidyverse)
library(viridis)
library(ggthemes)
library(ggmap)
library(plotly)

# Downloading data is free but you need to 
# create an account https://www.ala.org.au then
# use this email address to pull data.
# galah_config(email = YOUR_EMAIL_ADDRESS)
id <- galah_identify("numbat")

numbats <- atlas_occurrences(identify = id)
numbats <- numbats %>%
  mutate(year = year(eventDate),
         month = month(eventDate, label=TRUE, 
                       abbr=TRUE),
         wday = wday(eventDate, label=TRUE, 
                     abbr=TRUE, week_start=1),
         hour = hour(eventDate),
         day = ymd(as.Date(eventDate)))

# Some basic analysis
numbats %>% filter(year < 2000) %>%
  select(year, decimalLongitude, decimalLatitude) %>%
  arrange(year)

numbats %>%
  ggplot(aes(x=decimalLongitude, y=decimalLatitude, colour=year)) +
  geom_point() +
  scale_color_viridis() +
  theme_map()

# Get a map
all_loc <- get_map(c(115, -39, 151, -22))
wa <- get_map(c(114, -36, 120, -30))

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

write_csv(numbats, file="numbats.csv")

# Potential questions
# What time of day do numbat sightings occur?
# Was the distribution more widespread in the 1900s?
# Do sightings happen more on week days than weekends?
# Where do you find numbats in Australia?
# Are numbats seen more on sunny and warm days than cloudy, wet, cold days? (NEED WEATHER DATA)
# Are they more frequent in the summer or winter?
# Who/what are/is the most diligent collector?

# Distribute
# Code to create or update
# Processed variables
# Australian/WA map
# weather variable indicating cold and dark

