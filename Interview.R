"
Open Weather API

Author = Cerys Jones

April 2022
"

# Run API query (but not too often as is rate limited at 2000 a day!) ---------------------------------------------

library(httr)
library(jsonlite)

# User edit parameters
my_API_key <- '960c267292ce4722b03233848223004'

startdate <- "2022-04-18"
enddate <- "2022-04-19"
  
max_lat <- 43
min_lat <- 38.5

max_lon <- -78.5
min_lon <- -70

# API call
lat <- seq(min_lat, max_lat, by = 0.2)
lon <- seq(max_lon, min_lon, by = 0.2)

coord <- expand.grid("lat" = lat, "lon" = lon)

base_url <- "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?"

all_data <- data.frame() 

for (i in 1:nrow(coord)){
  
  params <- list(key = my_API_key,
                 q = paste0(coord$lat[i],",",coord$lon[i]),
                 format = "json",
                 extra = "utcDateTime",
                 date = startdate,
                 enddate = enddate,
                 tp = 1
                 )
  
  response <- GET(url = base_url, query = params)
  
  df <- data.frame(fromJSON(rawToChar(response$content)))
  
  all_data <- rbind(all_data, df)
  
  }

saveRDS(all_data, "C:/Users/jonescer/all_weather_data2.Rda")



# Load master data to avoid crashing API -----------------------------------------------------------------------

library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

master_data <- readRDS(file = "C:/Users/jonescer/all_weather_data2.Rda")

clean_data <- data.frame()

for (j in 1:nrow(master_data)){
  
  lat <- substr(master_data$data.request.query[j], 5, 9)
  lon <- substr(master_data$data.request.query[j], 19, 24)
  
  date <- master_data$data.weather.date[j]
  hourly <- data.frame(master_data$data.weather.hourly[j])
  
  hourly$lat <- lat
  hourly$lon <- lon
  hourly$date <- date
  
  clean_data <- rbind(clean_data, hourly)
  
}

clean_data <- clean_data %>% 
  select(date, lat, lon, everything()) %>%
  unnest_wider(weatherDesc) %>%
  rename("WeatherDesc" = value)

all_desc <- data.frame(unique(clean_data$WeatherDesc))

max_wind <- max(as.numeric(clean_data$windspeedMiles))
min_wind <- min(as.numeric(clean_data$windspeedMiles))

snow_meta <- read_csv("C:/Users/jonescer/Weather_Metadata.csv")

# Get date time as a single object
data <- clean_data %>%
  select(date, time, lat, lon, windspeedKmph, WeatherDesc) %>%
  left_join(snow_meta, by = c("WeatherDesc" = "weather_desc")) %>%
  mutate(date = as.Date(date)) %>%
  mutate(time = str_pad(time, 4, side = "left", pad = "0")) %>%
  mutate(time = paste0(substr(time,1,2), ":", substr(time,3,4))) %>%
  mutate(date_time = ymd_hm(paste(date, time))) %>%
  select(date_time, everything())

my_colors <- palette(c("#603242", "#7F5379", "#8B88B6", "#669EC4", "#EAEAEA"))
library(paletti)
my_colors <- c("#b7352d", "#2a6b8f", "#0f4461", "#26aef8")
scale_fill_my_palette <- get_pal(my_colors) %>%
  get_scale_fill()

run_snow_sim <- function(times, day, data){
  date <- as.Date(day)
  for (i in 1:length(times)){
    datetime <- ymd_hm(paste(day, times[i]))
    test_data <- data %>%
      filter(date_time == datetime) %>%
      mutate(windspeedKmph = round(as.numeric(windspeedKmph), 0))
    g <- ggplot(test_data, aes(as.numeric(lon), as.numeric(lat)))  +
      geom_raster(aes(fill=snow_marker), interpolate = TRUE) + scale_fill_distiller(palette = "BuPu",direction = 1, limits = c(0,4)) +
      theme_void() + theme(legend.position="none")
    ggsave(paste0("snow", substr(day, 9,10), "-", substr(times[i],1,2),".png"))
    g
  }}

run_wind_sim <- function(times, day, data){
  date <- as.Date(day)
  for (i in 1:length(times)){
    datetime <- ymd_hm(paste(day, times[i]))
    test_data <- data %>%
      filter(date_time == datetime) %>%
      mutate(windspeedKmph = round(as.numeric(windspeedKmph), 0))
    g <- ggplot(test_data, aes(as.numeric(lon), as.numeric(lat)))  +
      geom_raster(aes(fill=windspeedKmph), interpolate = TRUE) + scale_fill_distiller(palette = 'BuPu',direction = 1, limits = c(0,70)) +
      theme_void() + theme(legend.position="none")
    ggsave(paste0("wind", substr(day, 9,10), "-", substr(times[i],1,2),".png"))
    g
  }}

times <-  str_pad(paste0((0:7)*3,":00"), 5, side = "left", "0")

run_snow_sim(times, "2022-04-18", data)
run_snow_sim(times, "2022-04-19", data)
