#####################################################################
## Data prep for analysis of halo size per month (2021 as test run)
#####################################################################

# Clear memory ----
rm(list=ls())

# Install and load packages ----

#install.packages("raster")
#install.packages("rgdal")
#install.packages("sp")

library(raster)
library(rgdal)
library(sp)
library(sf)
library(rgeos)
library(tidyverse)
#install.packages("Rmisc")
#library(geosphere)
library(maptools)
library(Rmisc)

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

# Read in the data----


setwd(shapefile.dir)
dir()

# Define a vector with the filenames of the shapefiles
shapefile_filenames <- c("Measurements_20210128_Cloates.shp", "Measurements_20210321_Cloates.shp",
                         "Measurements_20210421_Cloates.shp", "Measurements_20210521_Cloates.shp",
                         "Measurements_20210628_Cloates.shp", "Measurements_20210723_Cloates.shp",
                         "Measurements_20210819_Cloates.shp", "Measurements_20210927_Cloates.shp",
                         "Measurements_20211030_Cloates.shp", "Measurements_20211220_Cloates.shp")

# Create an empty data frame to store mean lengths from all shapefiles
all_mean_lengths <- data.frame()

# Loop through the shapefile filenames
for (filename in shapefile_filenames) {
  cat("Processing:", filename, "\n")
  
  # Read the shapefile
  shapefile <- st_read(filename)
  
  # Extract the date from the filename using regular expressions
  date <- gsub(".*_(\\d{8})_.*", "\\1", filename)
  
  # Convert the shapefile to a dataframe
  shapefile_df <- as.data.frame(shapefile) %>%
    dplyr::select(id, Location, Status, Measure, length) %>%
    glimpse()
  
  # Calculate the length of each line
  shapefile_df$LineLength <- st_length(shapefile)
  
  # Add a "date" column to the dataframe
  shapefile_df$date <- date
  
  # Group by 'date', 'id', and calculate the mean length for each group
  mean_lengths <- shapefile_df %>%
    na.omit() %>%
    filter(Location == "Cloates") %>%
    group_by(date, id) %>%
    dplyr::mutate(MeanLength = mean(LineLength))
  
  # Append the mean lengths to the 'all_mean_lengths' data frame
  all_mean_lengths <- bind_rows(all_mean_lengths, mean_lengths)
  
  cat("Processed:", filename, "\n")
}

glimpse(all_mean_lengths)

setwd(tidy.dir)

# Save all mean lengths as a single CSV file
write.csv(all_mean_lengths, file = "2021_mean_lengths.csv", row.names = FALSE)

