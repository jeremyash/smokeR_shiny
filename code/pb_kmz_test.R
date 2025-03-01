#############################################################################
## Create stitched kml output from PB Piedmont Text File
#############################################################################



##~~~~~~~~~~~~~~
## libraries 
##~~~~~~~~~~~~~~

library(tidyverse)
library(lubridate)
# library(here)
library(lutz)
# library(rsconnect)
# library(shiny)
# library(rmarkdown)
library(sf)
library(ggmap)
library(gganimate)
library(gifski)


##~~~~~~~~~~~~~~
## unzip file
##~~~~~~~~~~~~~~

# unzip the file
unzip(zipfile = "raw_data/hourly_output.zip",
      exdir = "raw_data/hourly_output")

# get rid of zip file
# file.remove(paste(proj,
#                   "/data/pb_hourly.zip",
#                   sep = ""))



##~~~~~~~~~~~~~~
## create rds file for plotting
##~~~~~~~~~~~~~~

# pull one point for timzezone check
pb_ls <- list.files("raw_data/hourly_output",
                    pattern = "\\.txt$")
rx_pt <- read.table(paste("raw_data/hourly_output/",
                          pb_ls[[1]],
                          sep = ""),
                    header = TRUE) %>% 
  slice(1)

rx_lat <- rx_pt$latitude
rx_lon <- rx_pt$longitude



# get timezone of rx burn
rx_tz <- tz_lookup_coords(rx_lat, rx_lon, method = "accurate")

# function to read in each text file
read_pb_time_step <- function(x) {
  
  # file path
  dat_path <- paste("raw_data/hourly_output/", 
                    x,
                    sep = "")
  
  # time step for individual file
  dat_time <- dmy_hm(gsub(".txt", "", x),
                     tz = "UTC") 
  
  # convert to correct time zone
  dat_time <- with_tz(dat_time,
                      tz = rx_tz)
  
  # read into r
  dat_df <- read.table(dat_path,
                       header = TRUE) %>% 
    mutate(start_time = rep(dat_time, n()), 
           end_time = rep(dat_time, n()),
           .before = longitude) %>% 
    mutate(pt_type = ifelse(fog == 1, "Fog", "Smoke"))
}


# read files into a list with columns indicating each time step
pb_time_ls <- lapply(pb_ls, read_pb_time_step)

# combine into single df
pb_time_df <- bind_rows(pb_time_ls)

# create sf object
pb_time_sf <- pb_time_df %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"))

st_crs(pb_time_sf) <- sp::CRS("+init=epsg:4326")


st_write(pb_time_sf, 
         "gis/pb_time_sf.kml",
         driver = "KML")



  
library(readr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(gganimate)
library(gifski)

# dfFires = read_csv("~/Desktop/IntroR/Data/StudyArea_SmallFile.csv", col_names = TRUE) %>%
#   select(STATE, TOTALACRES, DLONGITUDE, DLATITUDE, STARTDATED ) %>%
#   filter(STATE == 'California', TOTALACRES >= 1000) %>%
#   mutate(CATEGORY = ifelse(TOTALACRES >= 1000 & TOTALACRES <= 5000, "SMALL", 
#                            ifelse(TOTALACRES > 5000 & TOTALACRES <= 25000, "MEDIUM", 
#                             ifelse(TOTALACRES > 25000 & TOTALACRES <= 100000, "LARGE", 
#                             ifelse(TOTALACRES > 100000, "SUPER", "UNK"))))) %>%
#   mutate(YEAR = ifelse(year(as.Date(dfFires$STARTDATED, format = '%m/%d/%Y')) > 70, 
#                        year(as.Date(dfFires$STARTDATED, format = '%m/%d/%Y')) + 1900, 
#                        year(as.Date(dfFires$STARTDATED, format = '%m/%d/%Y')) + 2000))

dfFires = read_csv("~/Desktop/IntroR/Data/StudyArea_SmallFile.csv", col_names = TRUE) %>%
  select(STATE, TOTALACRES, DLONGITUDE, DLATITUDE, YEAR = YEAR_ ) %>%
  filter(STATE == 'California') %>%
  mutate(CATEGORY = ifelse(TOTALACRES >= 1000 & TOTALACRES <= 5000, "SMALL", 
                           ifelse(TOTALACRES > 5000 & TOTALACRES <= 25000, "MEDIUM", 
                                  ifelse(TOTALACRES > 25000 & TOTALACRES <= 100000, "LARGE", 
                                         ifelse(TOTALACRES > 100000, "SUPER", "UNK")))))

# mutate(YEAR = ifelse(year(as.Date(dfFires$STARTDATED, format = '%m/%d/%Y')) > 70, 
#                      year(as.Date(dfFires$STARTDATED, format = '%m/%d/%Y')) + 1900, 
#                      year(as.Date(dfFires$STARTDATED, format = '%m/%d/%Y')) + 2000))

#map
# set up google API for ggmap 
api <- readLines("raw_data/google.api")
register_google(key = api)

map <- get_map(location = c(rx_lon, rx_lat), zoom = 6)
map_with_data  <- ggmap(map) + 
  geom_point(data=pb_time_df, aes(x=longitude, y=latitude, color = pt_type))

map_with_animation <- map_with_data +
  shadow_mark() +
  transition_time(pb_time_df$end_time) +
  ggtitle('Hour: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')

num_hours <- max(pb_time_df$end_time) - min(pb_time_df$end_time) + 1
animate(map_with_animation, nframes = num_hours, fps = 2)
anim_save("test_pb_piedmont.gif")




























