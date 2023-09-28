###################################
## Run some models
##################################

# Clear memory ----
rm(list=ls())

# Install and load packages ----

library(tidyverse)
library(maptools)
library(MetBrewer)
library(viridisLite)
library(lubridate)

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

halo_dat <- read.csv("2017_2021_mean_lengths.csv")%>%
  mutate(
    date=ymd(date),
    year = factor(year(date)),
    month = factor(month(date, label = FALSE)))%>%
  dplyr::select(id, Location, Status, date, MeanLength, month, year)%>%
  mutate_at(vars(id, Location, Status), list(as.factor)) %>%
  unique()%>%
  glimpse()

env_dat <- read.csv("SST_Chla_2017_2021.csv") %>%
  dplyr::select(mean_sst, mean_chla, month, year)%>%
  mutate_at(vars(month, year), list(as.factor)) %>%
  glimpse()

patchreef_dat <- read.csv("patchreef_area_cloates.csv")%>%
  mutate_at(vars(id, Location), list(as.factor)) %>%
  glimpse()

# joining data frames together ----

halo_env_dat <- left_join(halo_dat, env_dat, by=c("month", "year"))%>%
  glimpse()

dat <- left_join(patchreef_dat, halo_env_dat, by=c("id", "Location"))%>%
  na.omit()%>%
  glimpse()                          

# Plot vars and eyeballs the trends

Theme1 <- 
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=25),
    legend.title = element_blank(),
    plot.title=element_text(size=17),
    text=element_text(size=25),
    strip.text.y = element_text(size = 27,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=16),
    axis.title.y=element_text(vjust=0.6, angle=90, size=18),
    axis.text.x=element_text(size=15),
    axis.text.y=element_text(size=15),
    strip.background = element_blank())
  
  
ggplot_sst <-  ggplot(aes(x=mean_sst,y=MeanLength,  color=id), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('SST')+
  scale_color_manual(values=met.brewer("Renoir", 22))+
  theme_classic()
ggplot_sst


ggplot_chla <-  ggplot(aes(x=mean_chla,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Chl a')+
  scale_color_manual(values=met.brewer("Renoir", 4))+
  theme_classic()
ggplot_chla

ggplot_patchreef <-  ggplot(aes(x=area,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Patchreef Area')+
  scale_color_manual(values=met.brewer("Renoir", 4))+
  theme_classic()
ggplot_patchreef

# Check distribution of variables

# Set predictor variables---

pred.vars=c("mean_sst","mean_chla","area") 
response = dat$MeanLength
response

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---

round(cor(dat[,pred.vars]),2)
summary(response)

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i)) 
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
} 

par(mfrow=c(1,1))

# kinda looks ok, chla has that really high value for one year - look into that
# Can sqrt patch reef area if being pedantic

# run some little models for fun, 
# honestly LMMs look fine for these simple relationships. Go Gaussian for now

# Run a quick lm
 library(lme4)

mod1 <- lmer(MeanLength ~ mean_sst + mean_chla + (1 | id) + (1| year), data = dat)

# Print the model summary
summary(mod1)

# Plot the effect size

library(sjPlot)
library(sjmisc)

sjPlot::plot_model(mod1)

sjPlot::plot_model(mod1, 
                   axis.labels=c("Mean SST", "Mean Chla"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of SST and CHla on halo size")

sjPlot:: tab_model(mod1)


effects_sst <- effects::effect(term= "mean_sst", mod= mod1)
summary(effects_sst) #output of what the values are

# Save the effects values as a df:
x_sst <- as.data.frame(effects_sst)

sst_plot <- ggplot() + 
  #2
  geom_point(data=dat, aes(mean_sst, MeanLength), colour="#d8dcff") + 
  #3
  geom_point(data=x_sst, aes(x=mean_sst, y=fit), color="#aeadf0") +
  #4
  geom_line(data=x_sst, aes(x=mean_sst, y=fit), color="#aeadf0") +
  #5
  geom_ribbon(data= x_sst, aes(x=mean_sst, ymin=lower, ymax=upper), alpha= 0.3, fill="#aeadf0") +
  #6
  labs(x="Mean SST", y="Mean Length")+
  theme_classic()

sst_plot

# Try Chla 


effects_chla <- effects::effect(term= "mean_chla", mod= mod1)
summary(effects_chla) #output of what the values are

# Save the effects values as a df:
x_chla <- as.data.frame(effects_chla)

chla_plot <- ggplot() + 
  #2
  geom_point(data=dat, aes(mean_chla, MeanLength), colour="#d8dcff") + 
  #3
  geom_point(data=x_chla, aes(x=mean_chla, y=fit), color="#aeadf0") +
  #4
  geom_line(data=x_chla, aes(x=mean_chla, y=fit), color="#aeadf0") +
  #5
  geom_ribbon(data= x_chla, aes(x=mean_chla, ymin=lower, ymax=upper), alpha= 0.3, fill="#aeadf0") +
  #6
  labs(x="Mean Chla", y="Mean Length")+
  theme_classic()

chla_plot

plot(mod1)

par(mfrow=c(1,1))
qqnorm(resid(mod1))

library(MuMIn)
r.squaredGLMM(mod1)

#maybe transform chla so no gaps
# add in total rainfall
# ask someone about wind driven upwelling

