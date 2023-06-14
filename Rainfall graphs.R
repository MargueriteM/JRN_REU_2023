# Code to import and work with Biomet USJo1 data files
# Load libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)

# set working directory to location where data is saved
setwd("~/Desktop/Biomet Data")
# import data files one by one, use header = TRUE to assign first row as column names
biomet2010 <- read.csv("Biomet_USJo1_wide_2010_.csv", header=TRUE)
biomet2011 <- read.csv("Biomet_USJo1_wide_2011_.csv", header=TRUE)

# combine multiple files
biomet.10.11 <- rbind(biomet2010, biomet2011)

# read all files and combine them together
biometfiles <- list.files(path="~/Desktop/Biomet Data", full.names=TRUE,pattern="Biomet") 
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

str(biomet.10.11)

# graph rainfall data divided by years
ggplot(biomet.10.11, aes(date_time, P_RAIN_1_1_1))+
 geom_point()+
 facet_grid(.~year)

# graph by years and months
ggplot(biomet.10.11, aes(doy, P_RAIN_1_1_1))+
 geom_point()+
 facet_grid(year~month, scales="free")

#view timestamp column only
column_view <- biomet.10.11$time
# Print the column view
print(column_view)

# graph rainfall data divided by time and years
ggplot(biomet.10.11, aes(date_time, P_RAIN_1_1_1))+
  geom_point()+
  facet_grid(.~year)

# graph by years (2010-2011), months, time
ggplot(biomet.10.11, aes(date_time, P_RAIN_1_1_1))+
  geom_point()+
  facet_grid(year~month, scales="free")

# graph rainfall data divided by month
ggplot(biomet.10.11, aes(month, P_RAIN_1_1_1))+
  geom_point()+
  facet_grid(.~year)

# add columns for year/month/date for biomet_all
biomet_all <- biomet_all %>%
  mutate(year=year(date_time),
         month=month(date_time),
         doy=yday(date_time),
         date=date(date_time))
#total of annual data**
annualdata <- biomet_all%>%
  group_by(year)%>%
  summarize(annual_rain= sum(P_RAIN_1_1_1, na.rm = TRUE))
print(annualdata)

# graph total annual rainfall data divided by year
ggplot(annualdata, aes(factor(year), annual_rain))+
  geom_col()

#total monthly data for all years combined
monthlydata <- biomet_all%>%
  group_by(month)%>%
  summarize(monthly_rain= sum(P_RAIN_1_1_1, na.rm = TRUE))
print(monthlydata)

#graph total monthly rainfall data for all years combined
ggplot(monthlydata, aes(factor(month), monthly_rain))+
  geom_point()

#total monthly data for each year
monthly_annualdata <- biomet_all%>%
  group_by(month, year)%>%
  summarize(monthly_annualrain= sum(P_RAIN_1_1_1, na.rm = TRUE))
print(monthly_annualdata)

#graph total monthly rainfall data for each year
ggplot(monthly_annualdata, aes(factor(month), monthly_annualrain))+
  geom_point()
  

#total daily data for years combined
daily_data <- biomet_all%>%
  group_by(doy)%>%
  summarize(daily_rain= sum(P_RAIN_1_1_1, na.rm = TRUE))
print(daily_data)

str(daily_data)
#graph total daily rainfall data years combined ****
ggplot(daily_data, aes(doy, daily_rain))+
  geom_point()

#total daily data for each year
daily_annualdata <- biomet_all%>%
  group_by(date)%>%
  summarize(daily_annualrain= sum(P_RAIN_1_1_1, na.rm = TRUE))%>%
  mutate(year=year(date),
         month=month(date),
         doy=yday(date))
head(daily_annualdata)
#Categorize daily rain into no rain, small events, and large events
daily_annualdata<- daily_annualdata%>%
  mutate(raincat= case_when(
    daily_annualrain==0~ "no_rain",
    daily_annualrain>0&daily_annualrain<=5~ "small_events",
    daily_annualrain>5~"large_events"))%>%
  mutate(raincat=factor(raincat,levels=c("no_rain", "small_events", "large_events")))

#graph raincat categories
ggplot(daily_annualdata, aes(raincat, daily_annualrain))+
  geom_point()

#graph total daily rainfall data each year ****
ggplot(daily_annualdata, aes(doy, daily_annualrain))+
  geom_point()+
  facet_grid(year~.)


#monthly rain across all years boxplot
ggplot(monthly_annualdata, aes(factor(month), monthly_annualrain))+
  geom_boxplot(aes(group= month))

#daily boxplot
ggplot(daily_annualdata, aes(month, daily_annualrain))+
  geom_boxplot(aes(group= month))


#graph daily rain by category
ggplot(daily_annualdata, aes(doy, daily_annualrain, color=raincat))+
  geom_point()+
  facet_grid(year~.)

#graph rain categories by month
ggplot(daily_annualdata, aes(factor(month), daily_annualrain, color=raincat))+
  geom_boxplot()


#new dataframe with counts of no rain, small, and large events per month and year
rain_eventcount<- daily_annualdata%>%
  group_by(month,year)%>%
  count(raincat)

#dataframe with counts of only small and large events per year ****exclude no rain events
rain_eventcount<- daily_annualdata%>%
  filter(raincat !="no_rain")%>%
  group_by(month,year)%>%
  count(raincat)
  
