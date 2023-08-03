# Code to import and work with Biomet USJo1 data files
# Load libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(cowplot)
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
         date=date(date_time)) %>% 
  filter(date>= as.Date("2010-07-01"))%>%
  filter(P_RAIN_1_1_1 <40)

#Graph Biomet all
ggplot(biomet_all, aes(date_time, P_RAIN_1_1_1))+
  geom_line()+
  labs(x="Date",y="Total Rainfall (mm)")+
  geom_line(color="navy")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Total Rainfall Across Ten Years")
  
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

#Calculate monthly mean across all years
monthly_meandata <- monthly_annualdata %>% 
  group_by(month) %>% 
  summarise(monthlymean= mean(monthly_annualrain),
            monthlysd= sd(monthly_annualrain))

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

# daily cumulative by year

daily_annualdata <- daily_annualdata %>% 
  group_by(year) %>% 
  mutate(daily_cumsum= cumsum(daily_annualrain)) 



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

#graph rain events count with year, month, and rain event category
ggplot(rain_eventcount, aes(factor(month), n, color=factor(year)))+
  geom_point()+
  facet_grid(.~raincat)

#rain event count boxplot
ggplot(rain_eventcount, aes(factor(month), n))+
  geom_boxplot()+
  facet_grid(.~raincat)




# determine rain events that last more than 6 hours (=12 rows)
# https://stackoverflow.com/questions/51371155/r-select-rainfall-events-and-calculate-rainfall-event-total-from-time-series-da
flags <- biomet_all %>% 
  select(date_time, date, year, month, doy, P_RAIN_1_1_1) %>%
  filter((year==2010 & P_RAIN_1_1_1<39)| year >=2011)%>% # &(DoY>150&DoY<250))%>%
  # Set a rain flag if there is rain registered on the gauge
  mutate(rainflag = ifelse(P_RAIN_1_1_1 > 0, 1, 0)) %>% 
  # Create a column that contains the number of consecutive times there was rain or not.
  # Use `rle`` which indicates how many times consecutive values happen, and `rep`` to repeat it for each row.
  ##mutate(rainlength = rep(rle(rainflag)$lengths, rle(rainflag)$lengths)) %>% 
  # MM modify: sequence counts number of days with no rain fore ach individual day rather than the total days of a rain event for all rows
  # https://predictivehacks.com/count-the-consecutive-events-in-r/
  mutate(rainlength = sequence(rle(rainflag)$lengths)) %>%  
  # Set a flag for an event happening, when there is rain there is a rain event, 
  # when it is 0 but not for six consecutive times, it is still a rain event
  mutate(
    eventflag = ifelse(
      rainflag == 1, 
      1, 
      ifelse(
        rainflag == 0 & rainlength < 12, 
        1, 
        0
      )
    )
  ) %>% 
  # Correct for the case when the dataset starts with no rain for less than six consecutive times
  # If within the first six rows there is no rain registered, then the event flag should change to 0
  mutate(eventflag = ifelse(row_number() < 12 & rainflag == 0, 0, eventflag)) %>% 
  # Add an id to each event (rain or not), to group by on the pivot table
  mutate(eventid = case_when(eventflag==1 ~rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths)))

#graph P_RAIN colored by event ID
ggplot(flags, aes(doy, P_RAIN_1_1_1, color=factor(eventid)))+
  geom_point()+
  facet_grid(year~., scales="free_x")+
  theme(legend.position = "none")

#calculate total rainfall per event
eventtotals<-flags%>%
  group_by(year, eventid)%>%
  summarise(eventrain=sum(P_RAIN_1_1_1))%>%
  mutate(raincat=case_when(
    eventrain>0&eventrain<=5~ "small_events",
    eventrain>5~"large_events"))%>%
      mutate(raincat=factor(raincat,levels=c("small_events", "large_events")))

#graph total rainfall per event
ggplot(eventtotals, aes(factor(year), eventrain, color=raincat))+
  geom_point()

ggplot(eventtotals, aes(factor(year), eventrain, color=raincat))+
  geom_boxplot()
#histogram of count and eventrain separated by event type
  ggplot(na.omit(eventtotals), aes(eventrain))+
  geom_histogram()+
  facet_grid(year~raincat)
#calculate total rainfall per event (added month, doy)
eventtotals<-flags%>%
  group_by(year, month, doy, eventid)%>%
  summarise(eventrain=sum(P_RAIN_1_1_1))%>%
  mutate(raincat=case_when(
    eventrain>0&eventrain<=5~ "small_events",
    eventrain>5~"large_events"))%>%
  mutate(raincat=factor(raincat,levels=c("small_events", "large_events")))
#graph total rainfall per event each month
ggplot(eventtotals, aes(factor(month), eventrain, color=raincat))+
  geom_point()
#boxplot
ggplot(eventtotals, aes(factor(month), eventrain, color=raincat))+
  geom_boxplot()

# graph total rainfall per event each day
ggplot(eventtotals, aes(factor(doy), eventrain, color=raincat))+
  geom_point()

ggplot(na.omit(eventtotals), aes(factor(doy), eventrain, color=raincat))+
  geom_point()

ggplot(eventtotals, aes(factor(doy), eventrain, color=raincat))+
  geom_point()+
  facet_grid(year~.)

ggplot(na.omit(eventtotals), aes(factor(doy), eventrain, color=raincat))+
  geom_point()+
  facet_grid(year~.)

#calculate number of events in each month across all years
m_eventrain_count <- eventtotals %>% 
  group_by(month, raincat) %>% 
  summarise(n_rainevent= n())
  
#graph number events in each month across all years
ggplot(na.omit(m_eventrain_count), aes(factor(month), n_rainevent, fill= raincat))+
  geom_col()

ggplot(na.omit(m_eventrain_count), aes(factor(month), n_rainevent, fill= raincat))+
  geom_col(position = "dodge")

#calculate number of events in each year
y_eventrain_count <- eventtotals %>% 
  group_by(year, raincat) %>% 
  summarise(n_rainevent= n())

#graph number events in each year
ggplot(na.omit(y_eventrain_count), aes(factor(year), n_rainevent, fill= raincat))+
  geom_col(position="dodge")

#calculate monthly total rain for each raincat 
totalrain_event_m <- eventtotals %>% 
  group_by(year, month, raincat) %>%
  summarise(eventrain= sum(eventrain), 
            n_rainevent= n())

monthly_mean_event <- na.omit(totalrain_event_m) %>%
  group_by(month, raincat) %>% 
  summarise(event_mean= mean(eventrain, na.rm=TRUE),
            n_mean=mean(n_rainevent, na.rm=TRUE))

#graph monthly total rain for each raincat *check
ggplot(monthly_mean_event, aes(factor(month), event_mean, fill= raincat))+
  geom_col(position = "dodge")

ggplot(monthly_mean_event, aes(factor(month), n_mean, fill= raincat))+
  geom_col(position = "dodge")

#calculate annual total rain for each raincat 
totalrain_event_y <- eventtotals %>% 
  group_by(year, raincat) %>%
  summarise(eventrain= sum(eventrain))

#graph monthly total rain for each raincat *check
ggplot(na.omit(totalrain_event_y), aes(factor(year), eventrain, fill= raincat))+
  geom_col(position = "dodge")


#calculate total annual precipitation and total in rain event category
totalrain_event_y <- na.omit(eventtotals) %>% 
  group_by(year, raincat) %>%
  summarise(n_rainevent= n(),
            eventrain= sum(eventrain))

# Merge total annual rain with annual event rain
totalrain_event_y <- right_join(annualdata, totalrain_event_y, by=join_by(year))


#graph regression of annual and event rainfall
ggplot(totalrain_event_y, aes(annual_rain, eventrain, color= raincat))+
  geom_point()+
  geom_smooth(method="lm", alpha=0)+
  geom_abline(intercept = 0, slope = 1)+
  labs(x= "Annual Total Rainfall (mm)", y= "Total Event Rainfall (mm)")+
  scale_color_discrete(name= "Size Category", labels= c("Small (<5mm)", "Large (>5mm)"))+
  theme_bw()
#*****summary graphs
# Seasonal rain distribution- Daily rainfall per month graph
ggplot(monthly_annualdata, aes(factor(month), monthly_annualrain))+
  geom_boxplot(color="navy")+
  labs(x= "Month", y= "Monthly Rainfall (mm)")+
  theme_bw()

# Monthly mean and sd graph
ggplot(monthly_meandata, aes(factor(month), monthlymean))+
  geom_col(fill="navy")+
  geom_errorbar(aes(ymin=monthlymean-monthlysd, ymax= monthlymean+monthlysd), 
                width= 0.2)+
  labs(x= "Month", y= "Monthly Average Rainfall (mm)")+
  theme_bw()

# Annual rain graph
ggplot(annualdata, aes(factor(year), annual_rain))+
  geom_col(fill="lightblue", color= "darkblue")+
  geom_abline(intercept = 232, slope = 0)+
  labs(x= "Year", y= "Total Annual Rainfall (mm)")+
  theme_bw()

# Month and event rain graph
ggplot(na.omit(totalrain_event_m), aes(factor(month), eventrain, color= raincat))+
  geom_boxplot()+
  labs(x= "Month", y= "Event Rainfall (mm)")+
  scale_color_discrete(name= "Size Category", labels= c("Small (<5mm)", "Large (>5mm)"))+
  scale_color_manual(name = "Size Category",
                     labels = c("Small (<5mm)", "Large (>5mm)"),
                     values = c("deepskyblue", "navy")) +
  theme_bw()

# Month and event count graph
ggplot(na.omit(totalrain_event_m), aes(factor(month), n_rainevent, color= raincat))+
  geom_boxplot()+
  labs(x= "Month", y= "Event Count")+
  scale_color_discrete(name= "Size Category", labels= c("Small (<5mm)", "Large (>5mm)"))+
  scale_color_manual(name = "Size Category",
                     labels = c("Small (<5mm)", "Large (>5mm)"),
                     values = c("deepskyblue", "navy"))+ 
  theme_bw()



# Year and Event Rainfall graph
ggplot(na.omit(totalrain_event_y), aes(factor(year), eventrain, fill= raincat))+
  geom_col(position = "dodge")+
  labs(x= "Year", y= "Event Rainfall (mm)")+
  scale_fill_discrete(name= "Size Category", labels= c("Small (<5mm)", "Large (>5mm)"))+
  scale_fill_manual(name = "Size Category",
                     labels = c("Small (<5mm)", "Large (>5mm)"),
                     values = c("deepskyblue", "navy"))+
  theme_bw()

# Year and Event Count graph
ggplot(na.omit(totalrain_event_y), aes(factor(year), n_rainevent, fill= raincat))+
  geom_col(position = "dodge")+
  labs(x= "Year", y= "Event Count")+
  scale_fill_discrete(name= "Size Category", labels= c("Small (<5mm)", "Large (>5mm)"))+
  scale_fill_manual(name = "Size Category",
                    labels = c("Small (<5mm)", "Large (>5mm)"),
                    values = c("deepskyblue", "navy"))+
  theme_bw()

# Daily cumulative rainfall per year
ggplot(daily_annualdata, aes(doy, daily_cumsum, color= factor(year)))+
  geom_line()+
  labs(x= "DOY", y= "Daily Cumulative Rainfall (mm)")+
  scale_color_discrete(name= "Year")+
  theme_bw()

# soil moisture and events graphs based on small and large event count, 2014- most large event count, 2015- most small event count
# graph timeseries 2014 and 2015 rainfall
head(flags)
flags %>% 
  filter(year== 2014| year== 2015) %>% 
  ggplot(., aes(date_time, P_RAIN_1_1_1), color= factor(eventid))+
  geom_col()+
  theme(legend.position = "none")

p.rainevent <- na.omit(eventtotals) %>% 
  filter(year== 2013) %>% 
  ggplot(., aes(doy, eventrain, fill= raincat))+
  geom_col(position = "dodge")+
  labs(x= "DOY", y= "Event Rainfall (mm)")+
  theme(legend.position = c(0.1, 0.9),   # Set the legend position (left top corner)
       legend.justification = c(0, 1),  # Set the justification for the legend position
       legend.background = element_rect(color = "black", fill = "white"))+
  scale_fill_discrete(name= "Size Category", labels= c("Small (<5mm)", "Large (>5mm)"))+
  scale_fill_manual(name = "Size Category",
                    labels = c("Small (<5mm)", "Large (>5mm)"),
                    values = c("deepskyblue", "navy"))
  #facet_grid(year~.)

p.soilmoisture <-biomet_all %>% 
  filter(year==2013) %>% 
  ggplot(., aes(x=date_time))+
  labs(x= "Date", y= "Soil Water Content (%)")+
  geom_line(aes(y=SWC_1_1_1), color= "red")+
  geom_line(aes(y=SWC_1_2_1), color= "blue")+
  geom_line(aes(y=SWC_1_3_1), color= "purple")+
  geom_line(aes(y=SWC_1_4_1), color= "black")


p.soilmoisture2 <-biomet_all %>% 
  filter(year==2013) %>% 
  ggplot(., aes(x=date_time))+
  labs(x= "Date", y= "Soil Water Content")+
  geom_line(aes(y=SWC_1_1_1), color= "red")+
  geom_line(aes(y=SWC_1_4_1), color= "black")

#graph rainfall and soil mositure together
plot_grid(p.rainevent, p.soilmoisture, ncol = 1)



#Air temperature 

dailyairtemp <- biomet_all %>% 
  group_by(date) %>% 
  summarise(maxTA = max(TA_1_1_1),
            meanTA= mean(TA_1_1_1, na.rm=TRUE) ,
            minTA= min(TA_1_1_1)) %>% 
  mutate(year=year(date),
         doy= yday(date))

#graph daily air temp


  ggplot(dailyairtemp, aes(x=doy))+
  labs(x= "Date", y= "Air Temperature (C)")+
  geom_line(aes(y= maxTA), color= "red", linewidth= 0.2)+
  geom_line(aes(y= meanTA), color= "black")+
  geom_line(aes(y=minTA), color= "blue", linewidth= 0.2)+
  facet_wrap(year~.)

  
    






