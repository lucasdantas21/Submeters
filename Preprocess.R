#packages
pacman::p_load(readr, dplyr, tidyr, lubridate, data.table, chron, ggplot2, lattice, grid, zoo)

#set working directory
setwd("D:/Lucas/UBIQUM/Modulo_3")

#database upload
Consumption <-read_delim("Consumption.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)
View(Consumption)


#create new column, combining date & time
HPC <- Consumption
HPC$DateTime <- dmy_hms(paste(HPC$Date, HPC$Time))

#Changing names
setnames(HPC, old=c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), 
         new=c("Kitchen", "Laundry", "Air & Water"))

#placing datetime column to the beginning
HPC <- HPC[,c(ncol(HPC), 1:(ncol(HPC)-1))]

#changing data format
HPC$Date <- dmy(HPC$Date)
HPC$Time <- hms(HPC$Time)


#change submeteres metric to Kilowatt
HPC <- HPC %>%
  mutate(`Air & Water`= `Air & Water`/1000)%>%
  mutate(Laundry=Laundry/1000)%>%
  mutate(Kitchen=Kitchen/1000)

#Identify NA
source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
calendarHeat(dates=HPC$Date,
             values=HPC$Global_active_power,
             varname="Global Active Power")

#NAs replaced by last value known
HPC<-na.locf(HPC, na.rm = FALSE, maxgap = 1440)

#NAs by zero
HPC[is.na(HPC)] <- 0

#change global power per hour
HPC <- HPC %>%
  mutate(Global_active_power=Global_active_power/60)%>%
  mutate(Global_reactive_power=Global_reactive_power/60)

#new columns
HPC <- HPC %>%
  mutate(Hour=lubridate::hour(DateTime))%>%
  mutate(Year=lubridate::year(DateTime))%>%
  mutate(Month=lubridate::month(DateTime,label = T, abbr = F,locale = "english"))%>%
  mutate(Day=lubridate::day(DateTime))%>%
  mutate(Weekday=lubridate::wday(DateTime, label = T, abbr = F, locale = "english"))%>%
  mutate(Global_Power=Global_active_power+Global_reactive_power)%>%
  mutate(Cost=Global_Power*0.1472)

    
#Filter per day
HPC.day <- HPC%>%
  select(Year, Month, Day, Weekday, Global_Power, Global_active_power, 
         Global_reactive_power, Kitchen, Laundry, `Air & Water`, Cost)%>%
  group_by(Year, Month, Day, Weekday)%>%
  summarise_at(vars(Global_Power, Global_active_power, 
                    Global_reactive_power, Kitchen, Laundry, `Air & Water`, Cost), funs(sum))

#Filter por mes
HPC.month <- HPC%>%
select(Year, Month,Global_Power, Global_active_power, 
       Global_reactive_power, Kitchen, Laundry, `Air & Water`, Cost)%>%
  group_by(Year, Month)%>%
  summarise_at(vars(Global_Power, Global_active_power, 
                    Global_reactive_power, Kitchen, Laundry, `Air & Water`, Cost), funs(sum))


#Filter per year
HPC.year <- HPC%>%
  select(Year, Global_Power, Global_active_power, Global_reactive_power, 
         Kitchen, Laundry, `Air & Water`, Cost)%>%
  group_by(Year)%>%
  summarise_at(vars(Global_Power, Global_active_power, 
                    Global_reactive_power, Kitchen, Laundry, `Air & Water`, Cost), funs(sum))

#Filter per hour
HPC.hour <- HPC%>%
  select(Year, Month, Day, Hour, Global_Power, Global_active_power, 
         Global_reactive_power, Kitchen, Laundry, `Air & Water`, Cost)%>%
  group_by(Year, Month, Day, Hour)%>%
  summarise_at(vars(Global_Power, Global_active_power, 
                    Global_reactive_power, Kitchen, Laundry, `Air & Water`, Cost), funs(sum))




#NEW <- HPC %>%
  select(Date, Global_active_power, Global_reactive_power) %>%
  filter(Date >= as.Date("2008-01-05") & Date <= as.Date("2008-01-10"))

#Send to excel file
write.csv(HPC.month, "1.csv")


#Plots
ggplot(data=HPC.season, aes(x=Season, y=Global_power, group=Year,colour=Year)) +
  geom_line()+theme_bw()+
  geom_point()+facet_grid(facets = Year ~ ., margins = FALSE)


