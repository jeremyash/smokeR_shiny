## SPATIAL
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(maptools)
library(sf)
library(plotKML)

## DATA MANAGEMENT
library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
# library(zoo)
library(lubridate)
library(lutz)

## PLOTTING
library(scales)
library(units)
library(viridis)
library(extrafont)
library(gtable)
library(grid)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



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

# convert to spatial points
coordinates(pb_time_df) <- ~longitude+latitude
proj4string(pb_time_df) <- CRS("+init=epsg:4326")

# pull date info for time span range
begin <- format(min(pb_time_df$start_time), "%Y-%m-%dT%H:%M:%SZ")
end <- format(max(pb_time_df$start_time), "%Y-%m-%dT%H:%M:%SZ")


kml(pb_time_df, 
    color = pt_type,
    alpha = 0.8,
    file = "test_pb.kmz",
    kmz = TRUE)






































