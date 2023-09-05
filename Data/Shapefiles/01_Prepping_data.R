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


