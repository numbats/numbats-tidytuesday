library(galah) # API to ALA
library(lubridate)
library(tidyverse)
library(rnoaa)

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

narrogin <- meteo_pull_monitors(
  monitors = "ASN00010614",
  var = c("PRCP", "TMAX", "TMIN"),
  date_min = "2005-01-01",
  date_max = "2023-02-23")

narrogin %>%
  pivot_longer(cols = prcp:tmin, names_to = "var", values_to = "value") %>%
  mutate(day = lubridate::yday(date), year = lubridate::year(date)) %>%
  ggplot(aes(x = day, y= year, fill = is.na(value))) +
  geom_tile() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  facet_wrap(vars(var), ncol = 1) +
  scale_fill_brewer(palette = "Dark2", name = "missing") +
  xlab("Day of the year")

narrogin_latlon <- tibble(lon = 117.1782, lat = -32.9310)

within_rad <- function(x, y, lon, lat, km) {
  deg <- km/111
  inside <- ifelse(sqrt((lon-x)^2 + (lat-y)^2) < deg, "yes", "no")
  return(inside)
}

# Only sites within 50km radius of Narrogin weather station
# which is Dryandra Woodlands
numbats <- numbats %>%
  mutate(dryandra = within_rad(decimalLongitude, decimalLatitude, 
                               narrogin_latlon$lon, narrogin_latlon$lat,
                               50))

numbats <- numbats %>% 
  left_join(narrogin, by=c("day"="date")) %>%
  mutate(prcp = ifelse(dryandra == "no", NA, prcp),
         tmax = ifelse(dryandra == "no", NA, tmax),
         tmin = ifelse(dryandra == "no", NA, tmin)) %>%
  select(-id)
write_csv(numbats, file="numbats.csv")

# Getting the maps
all_loc <- get_map(c(115, -39, 151, -22))
wa <- get_map(c(114, -36, 120, -30))
save(all_loc, file="all_loc.rda")
save(wa, file="wa.rda")
