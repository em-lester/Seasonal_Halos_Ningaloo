###################################################
## Some exploratory plots for halo size over time
##################################################

# Clear memory ----
rm(list=ls())

# Install and load packages ----
library(tidyverse)
library(MetBrewer)

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
setwd(tidy.dir)
dir()

dat <- read.csv("2017_2021_mean_lengths.csv")%>%
  dplyr::select(id, Location, Status, date, MeanLength)%>%
  unique()%>%
  glimpse()


# ok, now I guess we want to format the date properly and then have a separate
# month and year column

dat$date <-  as.Date(as.character(dat$date),format = "%Y%m%d") 
str(dat$date)

# Create separate columns for day, month, and year
dat$day <- format(dat$date, "%d")
dat$month <- format(dat$date, "%m")
dat$year <- format(dat$date, "%Y")

dat$month <- as.factor(dat$month)
dat$year <- as.factor(dat$year)
glimpse(dat)
str(dat$year)
# plot monthly mean length?

plot <-  ggplot(aes(x=month,y=MeanLength,  color=year), data=dat)+ facet_wrap(~id, scales='free')+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Month')+
  scale_color_manual(values=met.brewer("Renoir", 4))+
  theme_classic()

plot

plot_combined <-  ggplot(aes(x=month,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Month')+
  scale_color_manual(values=met.brewer("Renoir", 4))+
  theme_classic()

plot_combined

# it is a start :)