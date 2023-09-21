#####################################################################
## Downloading environmental data
#####################################################################

# Clear memory ----
rm(list=ls())

# Load the necessary packages
library(rerddap)
library(ncdf4)
library(mapdata)
library(tidyverse)
library(lubridate)
library(MetBrewer)
#require("mapdata")

installed.packages("rerddap")
detach("package:rerddap", unload = TRUE)
library(rerddap)

# Define the ERDDAP server URL and dataset ID for the NOAA 5km data
server_url <- "https://coastwatch.pfeg.noaa.gov/erddap/"
dataset_id <- "NOAA_DHW"  

browse('NOAA_DHW')
info('NOAA_DHW')

# Query parameters for the data you want

latitude = c(-22.37, -22.75)
longitude = c(113.78, 113.62)
time = c("2017-01-01", "2017-12-31")

# create gridapp query 

sstInfo <- info('NOAA_DHW')
SST <- griddap(sstInfo, latitude = c(-22.37, -22.75), longitude = c(113.78, 113.62), time = c("2017-01-01", "2017-12-31"), fields = 'CRW_SST')
head(SST)

# play with daily sst to check spatial extent of data

require("ggplot2")
require("mapdata")
require("rerddap")

sstInfo <- info('NOAA_DHW')
# get latest daily sst
murSST <- griddap(sstInfo, latitude = c(-22.37, -22.75), longitude = c(113.78, 113.62), time = c('last','last'), fields = 'CRW_SST')
mycolor <- colors$temperature
w <- map_data("worldHires", ylim = c(-22.37, -22.75), xlim = c(113.78, 113.62))

ggplot(data = murSST$data, aes(x = longitude, y = latitude, fill = CRW_SST)) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  coord_fixed(1.3, xlim = c(113, 114.5),  ylim = c(-22, -23)) + ggtitle("Latest SST")

# Looks alright :) 

# Check the structure of the data
str(SST)

# Download the data to a dataframe
sst_df <- SST$data

# View the first few rows of the data
head(sst_df)
glimpse(sst_df)

# alrighty, now I guess we want to format date and time into something nice
# then find mean monthly temp 

# Convert the "time" column to a POSIXct datetime object
mean_sst_df <- sst_df %>%
  mutate(
    time = ymd_hms(time),  # Assumes the date string format "YYYY-MM-DDTHH:MM:SSZ"
    date = format(time, "%y-%m-%d"),
    year = year(time),
    month = factor(month(time, label = FALSE))
  )%>%
  
  group_by(month)%>%
  na.omit()%>%
  mutate(mean_sst= mean(CRW_SST))%>%
  ungroup()%>%
  glimpse()


# quick plot

sst_plot <- ggplot(data =mean_sst_df, aes(x = month, y = mean_sst, colour = CRW_SST)) +
  geom_point( size=3)+
  scale_colour_gradientn(colors = met.brewer("Hokusai3"))+
  theme_classic()
sst_plot

#NB this is only for 2017, need to do for all years, but will be easy
