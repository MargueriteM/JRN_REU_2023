# Code to import and work with Biomet USJo1 data files
# Load libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)

# set working directory to location where data is saved
setwd("C:/Users/iuribe3/OneDrive - University of Texas at El Paso/Biomet2_20201227")

# import data files one by one, use header = TRUE to assign first row as column names
biomet2010 <- read.csv("Biomet_USJo1_wide_2010_.csv", header=TRUE)
biomet2011 <- read.csv("Biomet_USJo1_wide_2011_.csv", header=TRUE)

# combine multiple files
biomet.10.11 <- rbind(biomet2010, biomet2011)

# read all files and combine them together
biometfiles <- list.files(path="C:/Users/iuribe3/OneDrive - University of Texas at El Paso/Biomet2_20201227", full.names=TRUE,pattern="Biomet") 
# read files and bind them into one file. fill=TRUE because of the missing columns in 2011
biomet_all <- do.call("rbind", lapply(biometfiles[1:11], header = TRUE, fread, sep=","))

# format date/time column to date/object
biomet.10.11 <- biomet.10.11 %>%
  mutate(date_time=ymd_hms(date_time))

# make exploratory graph of rainfall data
ggplot(biomet.10.11, aes(date_time, P_RAIN_1_1_1))+
  geom_point()

# add columns for year/month/date
biomet.10.11 <- biomet.10.11 %>%
  mutate(year=year(date_time),
         month=month(date_time),
         doy=yday(date_time),
         date=date(date_time))

# graph rainfall data divided by years
ggplot(biomet.10.11, aes(date_time, P_RAIN_1_1_1))+
  geom_point()+
  facet_grid(.~year)

# graph by years and months
ggplot(biomet.10.11, aes(doy, P_RAIN_1_1_1))+
  geom_point()+
  facet_grid(year~month, scales="free")

 