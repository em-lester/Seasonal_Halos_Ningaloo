###################################
## Run some exploratory models
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

rainfall_PAR_dat <- read.csv("Rainfall_PAR.csv")%>%
  mutate_at(vars(month, year), list(as.factor)) %>%
  dplyr::select(month, year, total_monthly_rain, monthly_PAR)%>%
  glimpse()

# joining data frames together ----

halo_env_dat <- left_join(halo_dat, env_dat, by=c("month", "year"))%>%
  glimpse()

halo_env_dat <- left_join(halo_env_dat, rainfall_PAR_dat, by=c("month", "year"))%>%
  glimpse()

dat <- left_join(patchreef_dat, halo_env_dat, by=c("id", "Location"))%>%
  na.omit()%>%
  glimpse()                          

setwd(tidy.dir)
write.csv( dat, "halo_patchreef_enviro_data.csv")

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
  

ggplot_sst_month <-  ggplot(aes(x=month,y=mean_sst,  color=year), data=dat)+
  geom_point()+
  ylab("Mean SST")+
  xlab('Month')+
  scale_color_manual(values=met.brewer("Renoir", 4))+
  theme_classic()
ggplot_sst_month

setwd(plots.dir)
ggsave("monthly_temp.tiff", ggplot_sst_month, dpi=300 )

ggplot_chla_month <-  ggplot(aes(x=month,y=mean_chla,  color=year), data=dat)+
  geom_point()+
  ylab("Mean Chla")+
  xlab('Month')+
  scale_color_manual(values=met.brewer("Renoir", 4))+
  theme_classic()
ggplot_chla_month

setwd(plots.dir)
ggsave("monthly_chla.tiff", ggplot_chla_month, dpi=300 )

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

ggplot_rain <-  ggplot(aes(x=total_monthly_rain,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Total Rainfall per month (mm)')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()
ggplot_rain

ggplot_PAR<-  ggplot(aes(x=monthly_PAR,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Mean PAR')+
  scale_color_manual(values=met.brewer("Renoir", 4))+
  theme_classic()
ggplot_PAR

ggplot_patchreef <-  ggplot(aes(x=area,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Patchreef Area')+
  scale_color_manual(values=met.brewer("Renoir", 4))+
  theme_classic()
ggplot_patchreef

# Check distribution of variables

# Set predictor variables---

pred.vars=c("mean_sst","mean_chla","area", "monthly_PAR", "total_monthly_rain") 
response = dat$MeanLength

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---

round(cor(dat[,pred.vars]),2)
summary(response)

# rainfall as SST = 0.39
# Rainfall and PAR = -0.49
# Everything else below 0.2. Maybe drop rain?

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
# Log rainfall
# log chla 


dat$sqrtmean_chla <- sqrt(dat$mean_chla)
dat$logmean_chla <- (log(dat$mean_chla))+1
dat$log_rainfall <- (log(dat$total_monthly_rain))+1

plot(dat$sqrtmean_chla)
plot(dat$logmean_chla)
plot(dat$log_rainfall)
plot(dat$monthly_PAR)
summary(dat$sqrtmean_chla)
summary(dat$logmean_chla)


# run some little models for fun, 
# honestly LMMs look fine for these simple relationships. Go Gaussian for now

# Run a quick lm
library(lme4)


mod1 <- lmer(MeanLength ~ mean_sst + monthly_PAR + (1 | id) + (1| year), data = dat)

# Print the model summary
summary(mod1)

# Plot the effect size ----

sjPlot::plot_model(mod1)

sjPlot::plot_model(mod1, 
                   axis.labels=c("Mean SST", "Mean PAR"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of SST and CHla on halo size")

# model table ----

sjPlot:: tab_model(mod1)
table_mod1 <- sjPlot:: tab_model(mod1)
tab_model(mod1, file = "Mod1_table.doc")

effects_sst <- effects::effect(term= "mean_sst", mod= mod1)
summary(effects_sst) #output of what the values are

# Save the effects values as a df:
x_sst <- as.data.frame(effects_sst)

sst_plot <- ggplot() + 
  #2
  geom_point(data=dat, aes(mean_sst, MeanLength), colour="#CAE9FF") + 
  #3
  #geom_point(data=x_sst, aes(x=mean_sst, y=fit), color="#1B4965") +
  #4
  geom_line(data=x_sst, aes(x=mean_sst, y=fit), color="#1B4965") +
  #5
  geom_ribbon(data= x_sst, aes(x=mean_sst, ymin=lower, ymax=upper), alpha= 0.3, fill="#1B4965") +
  #6
  labs(x="Mean SST (°C)", y="Mean Halo Length (m)")+
  theme_classic()

sst_plot

setwd(plots.dir)

ggsave("sst_predictions.tiff", plot=sst_plot, dpi=300)


# Try Chla 


effects_chla <- effects::effect(term= "logmean_chla", mod= mod1)
summary(effects_chla) #output of what the values are

# Save the effects values as a df:
x_chla <- as.data.frame(effects_chla)

chla_plot <- ggplot() + 
  #2
  geom_point(data=dat, aes(logmean_chla, MeanLength), colour="#CAE9FF") + 
  #3
 # geom_point(data=x_chla, aes(x=mean_chla, y=fit), color="#1B4965") +
  #4
  geom_line(data=x_chla, aes(x=logmean_chla, y=fit), color="#1B4965") +
  #5
  geom_ribbon(data= x_chla, aes(x=logmean_chla, ymin=lower, ymax=upper), alpha= 0.3, fill="#1B4965") +
  #6
  labs(x="Mean Chla", y="Mean Length")+
  theme_classic()

chla_plot

# PAR effects

effects_PAR <- effects::effect(term= "monthly_PAR", mod= mod1)
summary(effects_PAR) #output of what the values are

# Save the effects values as a df:
x_PAR <- as.data.frame(effects_PAR)

PAR_plot <- ggplot() + 
  #2
  geom_point(data=dat, aes(monthly_PAR, MeanLength), colour="#CAE9FF") + 
  #3
  #geom_point(data=x_sst, aes(x=mean_sst, y=fit), color="#1B4965") +
  #4
  geom_line(data=x_PAR, aes(x=monthly_PAR, y=fit), color="#1B4965") +
  #5
  geom_ribbon(data= x_PAR, aes(x=monthly_PAR, ymin=lower, ymax=upper), alpha= 0.3, fill="#1B4965") +
  #6
  labs(x="Mean PAR (µmol m-2 s-1)", y="Mean Halo Length (m)")+
  theme_classic()

PAR_plot 
setwd(plots.dir)
ggsave("PAR_predictions.tiff", PAR_plot, dpi=300)

# Model DIagnostics ----

par(mfrow=c(1,1))

plot(mod1)

qqnorm(resid(mod1))


r.squaredGLMM(mod1)

library(DHARMa)

testDispersion(mod1)
simulationOutput <- simulateResiduals(fittedModel = mod1)
plot(simulationOutput)

plotResiduals(mod1)

#maybe transform chla so no gaps - the large value of chla is driving sig eurgh
# add in total rainfall
# ask someone about wind driven upwelling
# PAR?
# or just SST - duh

mod2 <- lmer(MeanLength ~ mean_sst  + (1 | id) + (1| year), data = dat)

# Print the model summary
summary(mod2)

plot(mod2)

par(mfrow=c(1,1))
qqnorm(resid(mod2))

# Fin, you need to run a GAMM and probably a full subsets one. 