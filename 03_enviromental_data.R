###################################
## Downloading environmental data
##################################

# Clear memory ----
rm(list=ls())

# Study name---
study<-"seasonal.halos.ningaloo"

## Set your working directory ----
working.dir <-  "~/Repositories/Seasonal_Halos_Ningaloo"

## Save these directory names to use later---- 
data.dir<-paste(working.dir,"Data",sep="/")
plots.dir=paste(working.dir,"Plots",sep="/")
tidy.dir<-paste(data.dir,"Tidy",sep="/")
raw.dir<-paste(data.dir,"Raw",sep="/")
error.dir=paste(data.dir,"Errors to check",sep="/")
shapefile.dir <- paste(data.dir,"Shapefiles",sep="/")

# Load the necessary packages
library(rerddap)
library(ncdf4)
library(mapdata)
library(tidyverse)
library(lubridate)
library(MetBrewer)

# Define the ERDDAP server URL and dataset ID ----

server_url <- "https://coastwatch.pfeg.noaa.gov/erddap/"
dataset_id <- "NOAA_DHW"  

#browse('NOAA_DHW')
info('NOAA_DHW')

# Query parameters 

latitude = c(-22.37, -22.75)
longitude = c(113.78, 113.62)
time = c("2017-01-01", "2021-12-31")

# create gridapp query ----

sstInfo <- info('NOAA_DHW')
SST <- griddap(sstInfo, latitude = c(-22.37, -22.75), longitude = c(113.78, 113.62), time = c("2017-01-01", "2021-12-31"), fields = 'CRW_SST')
head(SST)

# play with daily sst to check spatial extent of data

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

# Convert the "time" column to a POSIXct datetime object ----
mean_sst_df <- sst_df %>%
  mutate(
    time = ymd_hms(time),  # Assumes the date string format "YYYY-MM-DDTHH:MM:SSZ"
    date = format(time, "%y-%m-%d"),
    year = factor(year(time)),
    month = factor(month(time, label = FALSE))
  )%>%
  
  group_by(year,month)%>%
  na.omit()%>%
  mutate(mean_sst= mean(CRW_SST))%>%
  dplyr::select(month, year, mean_sst)%>%
  distinct()%>%
  ungroup()%>%
  unite(yearmonth, c(year, month), sep = "-", remove = FALSE)%>%
  glimpse()

# quick plot

sst_plot <- ggplot(data =mean_sst_df, aes(x = month, y = mean_sst, colour = mean_sst)) +
  geom_point( size=3)+
  scale_colour_gradientn(colors = met.brewer("Hokusai3"))+
  theme_classic()
sst_plot

# sick

# Extract chla using dataset id erdMH1chla1day_R2022NRT ----

server_url <- "https://coastwatch.pfeg.noaa.gov/erddap/"
dataset_id <- "erdMH1chla1day"
 
#browse('erdMH1chla1day')
info('erdMH1chla1day')
 
# Query parameters 
 
latitude = c(-22.37, -22.75)
longitude = c(113.78, 113.62)
time = c("2017-01-01", "2021-12-31")

 # create gridapp query ----
 
chlaInfo <- info('erdMH1chla1day')
chla <- griddap(chlaInfo, latitude = c(-22.37, -22.75), longitude = c(113.78, 113.62), time = c("2017-01-01", "2021-12-31"), fields = 'chlorophyll')
 head(chla)
 
# play with daily chla to check spatial extent of data
 
chlaInfo <- info('erdMH1chla1day')
chlaInfo

# get latest daily chla
Cloates_chla <- griddap(chlaInfo, latitude = c(-22.37, -22.75), longitude = c(113.78, 113.62), time = c('2017-01-03','2017-01-03' ), fields = 'chlorophyll')
head(Cloates_chla)
mycolor <- colors$chlorophyll

w <- map_data("worldHires", ylim = c(-22.37, -22.75), xlim = c(113.78, 113.62))
 
ggplot(data = Cloates_chla$data, aes(x = longitude, y = latitude, fill = chlorophyll)) +
   geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
   geom_raster(interpolate = FALSE) +
   scale_fill_gradientn(colours = mycolor, na.value = NA) +
   theme_bw() + ylab("latitude") + xlab("longitude") +
   coord_fixed(1.3, xlim = c(113, 114.5),  ylim = c(-22, -23)) + ggtitle("Latest Chl a")
 
# Looks alright :) 
 
# reformat date, plot spatial extent and calculate monthly values ----
 
# Check the structure of the data
str(chla)

# Download the data to a dataframe
chla_df <- chla$data

# View the first few rows of the data
head(chla_df)
glimpse(chla_df)

# format date and time into something nice then find mean monthly temp 

mean_chla_df <- chla_df %>%
  mutate(
    time = ymd_hms(time),  # Assumes the date string format "YYYY-MM-DDTHH:MM:SSZ"
    date = format(time, "%y-%m-%d"),
    year = factor(year(time)),
    month = factor(month(time, label = FALSE))
  )%>%
  
  group_by(year, month)%>%
  na.omit()%>%
  mutate(mean_chla= mean(chlorophyll))%>%
  ungroup()%>%
  dplyr::select(month, year, mean_chla)%>%
  distinct()%>%
  unite(yearmonth, c(year, month), sep = "-", remove = FALSE)%>%
  glimpse()

head(mean_chla_df)
head(mean_sst_df)

# quick plot

chla_plot <- ggplot(data =mean_chla_df, aes(x = month, y = mean_chla, colour = mean_chla)) +
  geom_point( size=3)+
  scale_colour_gradientn(colors = met.brewer("Hokusai3"))+
  theme_classic()
chla_plot

# combine with sst and write nice csv file ----

sst_chla <- left_join(mean_sst_df, mean_chla_df, by =c("yearmonth", "month", "year"))%>%
            dplyr::select(month:mean_chla)%>%
            glimpse()

setwd(tidy.dir)

write.csv(sst_chla, file="SST_Chla_2017_2021.csv")

#fin