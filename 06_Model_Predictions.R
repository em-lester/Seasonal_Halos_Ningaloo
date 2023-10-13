###################################
## Make nice plots of predictions
##################################

# Clear memory ----
rm(list=ls())

# Install and load packages ----

library(tidyverse)
library(maptools)
library(MetBrewer)
library(viridisLite)
library(lubridate)
library(MuMIn)
library(sjPlot)
library(sjmisc)
library(mgcv)
library(patchwork)
library(png)
library(FSSgam)
library(MoMAColors)

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
m.dir <- paste(working.dir, "Model Outputs", sep="/")

# Read in the data----

setwd(tidy.dir)
dir()

dat <- read.csv("halo_patchreef_enviro_data.csv")%>%
  mutate_at(vars(id, Location, Status), list(as.factor)) %>%
  dplyr::rename(meanlength = MeanLength)%>%
  dplyr::rename(status = Status)%>%
  dplyr::rename(patchreefarea = area)%>%
  dplyr::rename(response = meanlength)%>%
  dplyr::mutate(log.total_monthly_rain =log(dat$total_monthly_rain+1)) %>%
  dplyr::mutate(log.mean_chla = log(dat$mean_chla+1))%>%  
  dplyr::mutate(log.mean_sst = log(dat$mean_sst+1))%>% 
  glimpse()

# MODEL  (response ~ log.mean_chla+mean_sst+monthly_PAR) ----

gamm=gam(response~s(log.mean_chla,k=3,bs='cr')+s(mean_sst,k=3,bs='cr')+ monthly_PAR + s(year,bs="re"), family=gaussian,data=dat)
summary(gamm)
gam.check(gamm)

# predict - log.mean_chla ----

mod<-gamm

testdata.log.mean_chla <- expand.grid(log.mean_chla=seq(min(dat$log.mean_chla),max(dat$log.mean_chla),length.out = 20),
                        mean_sst=mean(mod$model$mean_sst),
                        monthly_PAR=mean(mod$model$monthly_PAR),
                        year=(mod$model$year))%>%
  distinct()%>%
  glimpse()


fits.log.mean_chla <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.log.mean_chla = testdata.log.mean_chla%>%data.frame(fits.log.mean_chla)%>%
  group_by(log.mean_chla)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

predicts.log.mean_chla

# predict - mean_sst ----
mod<-gamm
testdata.mean_sst <- expand.grid(mean_sst=seq(min(dat$mean_sst),max(dat$mean_sst),length.out = 20),
                                 log.mean_chla=mean(mod$model$log.mean_chla),
                                 monthly_PAR=mean(mod$model$monthly_PAR),
                                 year=(mod$model$year))%>%
  distinct()%>%
  glimpse()

fits.mean_sst <- predict.gam(mod, newdata=testdata.mean_sst, type='response', se.fit=T)

predicts.mean_sst = testdata.mean_sst%>%data.frame(fits.mean_sst)%>%
  group_by(mean_sst)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - monthly_PAR ----
mod<-gamm

plot(dat$monthly_PAR)

testdata.monthly_PAR <- expand.grid(monthly_PAR=seq(min(dat$monthly_PAR),max(dat$monthly_PAR),length.out = 20),
                                    mean_sst=mean(mod$model$mean_sst),
                                    log.mean_chla=mean(mod$model$log.mean_chla),
                                    year=(mod$model$year))%>%
  distinct()%>%
  glimpse()

fits.monthly_PAR <- predict.gam(mod, newdata=testdata.monthly_PAR, type='response', se.fit=T)

predicts.monthly_PAR = testdata.monthly_PAR%>%data.frame(fits.monthly_PAR)%>%
  group_by(monthly_PAR)%>% #only change here
  dplyr::summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS  ----

# Set a theme ----

Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    text=element_text(size=12),
    strip.text.y = element_text(size = 13,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=13),
    axis.title.y=element_text(vjust=0.6, angle=90, size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# log.mean_chla ----

pal <- c("#85C660", "#4DCCC3", "#AA7C6B", "#135086")
pal

pal1 <- met.brewer("Hokusai3", n=4)
pal1  

pal2 <- met.brewer("Pissaro", n=8)
pal2

ggmod.log.mean_chla<- ggplot(aes(x=log.mean_chla,y=response,fill=log.mean_chla,colour=log.mean_chla), data=predicts.log.mean_chla,show.legend=FALSE) +
  xlab(expression("Log Mean Monthly Chla (mg m"^"-3"*")"))+
  ylab('')+
  scale_colour_gradientn(colours="#85C660")+
  scale_fill_gradientn(colours="#85C660")+
  geom_point(data=dat, aes(x=log.mean_chla,y=response), alpha=0.2)+
  geom_line(data=predicts.log.mean_chla,show.legend=TRUE)+
  geom_ribbon(aes(ymin = response - se.fit, ymax = response + se.fit), alpha = 0.4, fill = "#85C660", colour="#85C660", linetype = 'blank') +
  theme_classic()+
  Theme1

ggmod.log.mean_chla

# mean_sst ----

ggmod.mean_sst <- ggplot(aes(x=mean_sst,y=response,fill=mean_sst,colour=mean_sst), data=predicts.mean_sst) +
  xlab("Mean Monthly SST (°C)")+
  ylab('Mean Halo Size (m)')+
  scale_colour_gradientn(colours="#AA7C6B")+
  scale_fill_gradientn(colours="#AA7C6B")+
  geom_point(data=dat, aes(x=mean_sst,y=response), alpha=0.1)+
  geom_line(data=predicts.mean_sst,show.legend=TRUE)+
  geom_ribbon(aes(ymin = response - se.fit, ymax = response + se.fit), alpha = 0.4, fill = "#AA7C6B", colour="#AA7C6B", linetype = 'blank') +
  theme_classic()+
  Theme1

ggmod.mean_sst

head(predicts.mean_sst)

# predicts.monthly_PAR ----

ggmod.monthly_PAR <- ggplot(aes(x=monthly_PAR,y=response,fill=monthly_PAR,colour=monthly_PAR), data=predicts.monthly_PAR) +
  xlab(expression("Mean Monthly PAR (µmol m"^"-2"*" s"^"-1"*")"))+
  ylab('')+
  scale_colour_gradientn(colours="#4DCCC3")+
  scale_fill_gradientn(colours="#4DCCC3")+
  geom_point(data=dat, aes(x=monthly_PAR,y=response), alpha=0.1)+
  geom_line(data=predicts.monthly_PAR,show.legend=TRUE)+
  geom_ribbon(aes(ymin = response - se.fit, ymax = response + se.fit), alpha = 0.4, fill = "#4DCCC3", colour="#4DCCC3", linetype = 'blank') +
  theme_classic()+
  Theme1

ggmod.monthly_PAR

# Combine with patchwork ----

model_predictions <- (ggmod.log.mean_chla/ggmod.mean_sst/ggmod.monthly_PAR)+ plot_annotation(tag_levels = 'A')
model_predictions

setwd(plots.dir)
?ggsave
ggsave("gamm_predictions.tiff",model_predictions, dpi=300, width=4, height=9)

#Fin