
### Load Packages###
pacman::p_load(dplyr, tidyr, ggplot2,lubridate, labeling, scales, tseries, forecast)

####Load Data####
setwd("~/Desktop/Ubiqum/R_task5_electricity")
Consumption <- read.csv("household_power_consumption.txt", TRUE, sep = ";", na.strings = c("NA","?"), stringsAsFactors = FALSE)  

#### Exploring the data ####

####Time Conversion####
Consumption$DateTime <-paste(Consumption$Date,Consumption$Time)
Consumption <- Consumption[,c(ncol(Consumption), 1:(ncol(Consumption)-1))]

#We convert data column in data format: 
Consumption$Date <- as.Date(Consumption$Date, "%d/%m/%Y") 
Consumption$DateTime <- as.POSIXct(strptime(Consumption$DateTime, "%d/%m/%Y %H:%M:%S", tz = "GMT"))  # function to change date and time.

#Descriptive statistics of the data:
summary(Consumption)
summarise(Consumption, meanV = mean(Voltage, na.rm = TRUE))

##---- TIME HOUR CHANGE:------- ## 
Consumption <- Consumption %>% 
  mutate(DateTime = ifelse(between(DateTime, as_datetime("2007-03-25 02:00:00"),
                                   as_datetime("2007-10-28 01:59:00")),
                                   (DateTime + 3600) ,
                                   (DateTime))) # we write 1:59 because we know it is already adding +1, the time actually changes at 2:59. 
Consumption <- Consumption %>%
mutate(DateTime = ifelse(between(DateTime, as_datetime("2008-03-30 02:00:00"),
                                 as_datetime("2008-10-26 01:59:00")),
                         (DateTime + 3600) ,
                         (DateTime)))

Consumption <- Consumption %>%
mutate(DateTime = ifelse(between(DateTime, as_datetime("2009-03-29 02:00:00"),
                                 as_datetime("2009-10-25 01:59:00")),
                         (DateTime + 3600) ,
                         (DateTime)))

Consumption <- Consumption %>%
mutate(DateTime = ifelse(between(DateTime, as_datetime("2010-03-28 02:00:00"),
                                 as_datetime("2010-10-31 01:59:00")),
                         (DateTime + 3600) ,
                         (DateTime)))

Consumption$DateTime<- as.POSIXct(Consumption$DateTime, origin="1970-01-01", tz="GMT")  # It has been transformed as numeric, so we had to change it again writing this origin ( origin of data in R)
                                                                                                                                                    
# We have replaced all NA's with 0: 

Consumption_ <- replace(Consumption, is.na(Consumption), 0)
Consumption_[!complete.cases(Consumption_),]

#### Creating new variables ####

#We add a new column with Watts/ hour in global consumption, so we have it on the same metrics than submeter:
Consumption<-cbind(Consumption, Consumption$Global_active_power*1000/60)
colnames(Consumption)[11]<- "ActivePower_WH"

#New column Active_energy is the energy consumed by the house not measured by any sub_metering:
Consumption<- cbind(Consumption, Consumption$ActivePower_WH-Consumption$Sub_metering_1-Consumption$Sub_metering_2-Consumption$Sub_metering_3)
colnames(Consumption)[12]<- "RestConsumption"
#New colum with day of the week:
Consumption <- cbind(Consumption,weekdays(as.Date(Consumption$Date, '%d/%m/%Y')), stringsAsFactors=FALSE)
colnames(Consumption)[13]<-"week_days"
#New column with the month:
Consumption <- cbind(Consumption,month(as.Date(Consumption$Date, '%d/%m/%Y')), stringsAsFactors=FALSE)
colnames(Consumption)[14]<-"month"

#New column with month and year:
MonthYear <- separate(Consumption, Date, into=c("year", "month", "day"))
MonthYear <- paste(MonthYear$year, MonthYear$month, sep="/")
Consumption <-cbind(Consumption, MonthYear)  

#new column by minutes:
Minute <- separate(Consumption, Time, into = c("Hour", "Minute","Seconds" ))
Consumption<- cbind(Consumption, Minute$Minute)

#we create a subset of 1 minute:
colnames(Consumption)[16]<-"Minute"
Sample_data<- subset(Consumption, Minute==31)  # Instead of subset, we can also use "filter"

#New column with the years 
Consumption <- cbind(Consumption, year(as.Date(Consumption$Date, '%d/%m/%Y')), stringsAsFactors = FALSE) 
colnames (Consumption)[17]<- "Year"

# New column with Reactive Power Watts/Hour:
Consumption<-cbind(Consumption, Consumption$Global_reactive_power*1000/60)
colnames(Consumption)[18]<-"Reactive_WH"

#New column with IntxVolt in HOURS:
Consumption<-cbind(Consumption, (Consumption$Voltage*Consumption$Global_intensity)/60)
colnames(Consumption)[19]<- "VoltxInt_WH"

#We change the order of the columns Reactive_WH and VoltxInt_WH:
Consumption<-Consumption[,c(1:12,18,19,13:17)]

colnames(Consumption)[8]<-"S1.Kitchen"
colnames(Consumption)[9]<-"S2.Laundry_room"
colnames(Consumption)[10]<-"S3.WaterHeater_AirConditioner"


####Febrary vs July####
#We analize the consumoption of Febrary and July ( Cold and warm month - no holidays):
Febrary <- subset(Consumption, month == 2)
July <- subset(Consumption, month == 7)


#We analize average consumption between week days and weekends in February:

anual.week <- Febrary %>%
     group_by(week_days )%>% 
     summarise(Mean_S1 = mean(S1.Kitchen, na.rm=TRUE),
                  Mean_S2 = mean(S2.Laundry_room, na.rm=TRUE),
                  Mean_S3 = mean(S3.WaterHeater_AirConditioner, na.rm= TRUE),
                  Mean_GC = mean(ActivePower_WH, na.rm=TRUE),
                  Mean_AE = mean(RestConsumption, na.rm=TRUE))

#We order the table by week days:
anual.week$week_days <- factor(anual.week$week_days, levels= c( "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

anual.week[order(anual.week$week_days), ]


#We analize average consumption between week days and weekends in July:

July_means <- July %>%
  group_by(week_days )%>% 
  summarise(Mean_S1 = mean(S1.Kitchen, na.rm=TRUE),
            Mean_S2 = mean(S2.Laundry_room, na.rm=TRUE),
            Mean_S3 = mean(S3.WaterHeater_AirConditioner, na.rm= TRUE),
            Mean_GC = mean(ActivePower_WH, na.rm=TRUE),
            Mean_AE = mean(RestConsumption, na.rm=TRUE))


#We order the table by week days:
July_means$week_days <- factor(July_means$week_days, levels= c( "Monday", 
                                                              "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
July_means[order(July_means$week_days), ]

plot(factor(July_means$week_days), July_means$Mean_GC, main = "July total electricity consumption by day week")


### Years Consumption  Mean global#### 

tbl_year<-Consumption %>%
  group_by(Year)%>%
  summarise(Mean_Consumption = mean(ActivePower_WH, na.rm = TRUE))

#plot with the consumptions by years:
tbl_year<-tbl_year[-c(1),]
ggplot() + geom_col(data=tbl_year, aes(x=Year, y=Mean_Consumption), fill=heat.colors(4)) + labs(x="Year", y="Average Consumption", title="Average Energy Consumption by Year")

#Playing with GGPLOT with July_means dataable:
ggplot() + geom_col(data=July_means, aes(x=week_days, y=Mean_GC))

# unite both tables: July_means and anal_week:
#Idea: Add a colum  on the table saying the month and then join both table by day_week. 
Month_ <- c("Febrary","Febrary", "Febrary","Febrary","Febrary","Febrary","Febrary" )
anual.week<-cbind(anual.week, Month_)
Month_ <- c("July","July","July","July","July","July","July")
July_means<-cbind(July_means, Month_)
anual.week<-rbind(anual.week, July_means) # We unite both datatables by row 
colnames(anal_week)[7]<- "Month"

#Two graph with the Means_GC
ggplot() + 
  geom_col(data=anual.week, aes(x=week_days, y=Mean_GC)) 
+ facet_grid(Month ~ .)

#The same but one behind the other:
ggplot() + 
  geom_col(data=anual.week, aes(x=week_days, y=Mean_GC)) + 
  facet_grid(. ~ Month)

#July and Febrary on the same column fill= Month: 
ggplot() + 
  geom_col(data=anual.week, aes(x=week_days, y=Mean_GC, fill=Month))

# We add position = "dodge": 
ggplot() + 
  geom_col(data=anual.week, aes(x=week_days, y=Mean_GC, fill=Month), position= "dodge")

#Done with the labels! :D 
ggplot() + geom_col(data=anual.week, aes(x=week_days, y=Mean_GC, fill=Month), position= "dodge") + labs(x="Week Days", y="Average Global Consumption", title="Average Consumption by Weekdays ")


#Now we will see how the consumption by Sub-meters has changed over the years: 
tbl_year2<-Consumption %>%
  group_by(Year)%>%
  summarise(ActivePower_WHn = sum(ActivePower_WH, na.rm = TRUE),
            S1.Kitchen = sum(S1.Kitchen, na.rm=TRUE),
            S2.Laundry_room = sum(S2.Laundry_room, na.rm=TRUE),
            S3.WaterHeater_AirConditioner = sum(S3.WaterHeater_AirConditioner, na.rm= TRUE),
            RestConsumption = sum(RestConsumption, na.rm=TRUE))

tbl_year2<-tbl_year2[-c(1),] # We delete 2006 for lack of data


# We reshape the data creating a new table - we also choose the names of the columns Submeter and Consumption:
tbl_year3<- tbl_year2 %>%
  gather(Submeter, Consump, S1.Kitchen:RestConsumption)

ggplot() +
  geom_col(data=tbl_year3, aes(x=Year, y=Consump, fill=Submeter), position="dodge") + 
  labs(x="Year", y="Consumption Watt/Hour", title="Total Consumption by Sub-meters") + 
  scale_y_continuous(labels=scales::comma) + 
  theme_linedraw(base_size = 11, base_family = "")

# Plot without dodge position:
ggplot() + 
  geom_col(data=tbl_year3, aes(x=Year, y=Consump, fill=Submeter)) + 
  labs(x="Year", y="Watt/hour", title="Total Consumption by Sub-meters") + 
  theme_linedraw(base_size = 11, base_family = "")  

 
#### 2007 #### Second: Create subset by year and averages
x2007<- subset(Consumption, Year == 2007)

tbl_2007<- x2007 %>%
  group_by(month)%>%
  summarise(ActiveActivePower_WH = mean(ActivePower_WH, na.rm = TRUE),
            S1.Kitchen = mean(S1.Kitchen, na.rm=TRUE),
            S2.Laundry_room = mean(S2.Laundry_room, na.rm=TRUE),
            S3.WaterHeater_AirConditioner = mean(S3.WaterHeater_AirConditioner, na.rm= TRUE),
            RestConsumption = mean(RestConsumption, na.rm=TRUE))
            
# Reshape the data and create the plot:
tbl_2007[1,1] <- "Jan"
tbl_2007[2,1] <- "Feb"
tbl_2007[3,1] <- "Mar"
tbl_2007[4,1] <- "Apr"
tbl_2007[5,1] <- "May"
tbl_2007[6,1] <- "Jun"
tbl_2007[7,1] <- "Jul"
tbl_2007[8,1] <- "Aug"
tbl_2007[9,1] <- "Sep"
tbl_2007[10,1] <- "Oct"
tbl_2007[11,1] <- "Nov"
tbl_2007[12,1] <-"Dec"

tbl2_2007<- tbl_2007%>%gather(Submeter, Consump, S1.Kitchen:RestConsumption)

tbl2_2007$month <- factor(tbl2_2007$month, levels= c( "Jan","Feb","Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 
                                                                
tbl2_2007[order(tbl2_2007$month), ]

ggplot() + 
  geom_col(data=tbl2_2007, aes(x=month, y=Consump, fill=Submeter)) + 
  labs(x="2007", y="Average Consumption", title="Average Consumption 2007 by Month") + 
  theme_linedraw(base_size = 11, base_family = "")

ggplot() + 
  geom_col(data=tbl2_2007, aes(x=month, y=Consump, fill=Submeter), position="dodge" ) + 
  labs(x="2007", y="Average Consumption", title="Average Consumption 2007 by Month") + 
  theme_linedraw(base_size = 11, base_family = "")

#Let's do the same for 2008: 

x2008<- subset(Consumption, Year == 2008)

tbl_2008<- x2008 %>%
  group_by(month)%>%
  summarise(ActivePower_WH = mean(ActivePower_WH, na.rm = TRUE),
            S1.Kitchen = mean(S1.Kitchen, na.rm=TRUE),
            S2.Laundry_room = mean(S2.Laundry_room, na.rm=TRUE),
            S3.WaterHeater_AirConditioner = mean(S3.WaterHeater_AirConditioner, na.rm= TRUE),
            RestConsumption = mean(RestConsumption, na.rm=TRUE))

tbl_2008[1,1] <- "Jan"
tbl_2008[2,1] <- "Feb"
tbl_2008[3,1] <- "Mar"
tbl_2008[4,1] <- "Apr"
tbl_2008[5,1] <- "May"
tbl_2008[6,1] <- "Jun"
tbl_2008[7,1] <- "Jul"
tbl_2008[8,1] <- "Aug"
tbl_2008[9,1] <- "Sep"
tbl_2008[10,1] <- "Oct"
tbl_2008[11,1] <- "Nov"
tbl_2008[12,1] <-"Dec"

tbl2_2008<- tbl_2008%>%gather(Submeter, Consump, S1.Kitchen:RestConsumption)

tbl2_2008$month <- factor(tbl2_2008$month, levels= c( "Jan","Feb","Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 

tbl2_2008[order(tbl2_2008$month), ]

ggplot() + 
  geom_col(data=tbl2_2008, aes(x=month, y=Consump, fill=Submeter)) + 
  labs(x="2008", y="Watt/hour", title="Average Consumption 2008 by Month") + 
  theme_linedraw(base_size = 11, base_family = "")

ggplot() + 
  geom_col(data=tbl2_2008, aes(x=month, y=Consump, fill=Submeter), position="dodge" ) + 
  labs(x="2008", y="Average Consumption", title="Average Consumption 2008 by Month") + 
  theme_linedraw(base_size = 11, base_family = "")

#Again, we try to create a day timeline with the submeters:
x2007_sub<- subset(x2007, Minute==15)
x2007_sub<-subset(x2007_sub, month==1)
x2007_sub<-subset(x2007_sub, week_days=="Monday")
day_<-separate(x2007_sub, Date, into=c("Year", "Month","Day"))
x2007_sub<-cbind(x2007_sub, day_$Day)
colnames(x2007_sub)[20] <- "Day"
x2007_sub<- subset(x2007_sub, Day==15)

#We add this tho cols:
x2007_sub<-cbind(x2007_sub, x2007_sub$Voltage/60)
x2007_sub<-cbind(x2007_sub, x2007_sub$Global_intensity/60)
colnames(x2007_sub)[21]<-"Voltage_WH"
colnames(x2007_sub)[22]<- "Intensity_WH"

x2007_sub_graph<-x2007_sub %>%
gather(Submeter, Consump, S1.Kitchen:ActivePower_WH)

ggplot()+
geom_line(data=x2007_sub_graph,
          aes(x=DateTime, y=Consump,
              color=Submeter),
          alpha = 0.4, size = 4) + 
labs(x="Time", y="Watt/hour",
     title="Consumption by Submeter on January 15th 2007") + 
theme_linedraw(base_size = 11, base_family = "")



x2007_sub_EL<- x2007_sub%>%
gather(Power, Consump,ActivePower_WH, Reactive_WH,VoltxInt_WH)

ggplot() +
geom_line(data=x2007_sub_EL,
          aes(x=DateTime, y=Consump, color=Power),
          alpha = 0.4, size = 4) + 
labs(x="Time", y="Watt/hour",
     title="Power Consumption on January 15th 2007") + 
theme_linedraw(base_size = 11, base_family = "")


### We standarize the data: We will see as intensity and Active Power are the same --> we prove physics

x2007_prova<- x2007_sub%>% select(DateTime, ActivePower_WH, Reactive_WH, Voltage_WH, Intensity_WH )
X2007NORM <- lapply(x2007_prova, function(x) if(is.numeric(x)){scale(x, center=T, scale=T)}else x) 
df_norm<-as.data.frame(X2007NORM)
df_norm_graph<- df_norm%>% gather(Power, Consump, ActivePower_WH:Intensity_WH)
ggplot()+geom_line(data=df_norm_graph, aes(x=DateTime, y=Consump, color=Power), alpha = 0.4, size = 4) + labs(x="Time", y="Watt/hour", title="Power Consumption on January 15th 2007") + theme_linedraw(base_size = 11, base_family = "")
# To find out the incredible corr:
x<- df_norm[2:5]
cor(x)

# LET'S TRY IT FOR ANOTHER DAY, NOW IN SUMMER:

x2008_sub<- subset(x2008, Minute==15)
x2008_sub<-subset(x2008_sub, month==8)
x2008_sub<-subset(x2008_sub, week_days=="Sunday")
day_<-separate(x2008_sub, Date, into=c("Year", "Month","Day"))
x2008_sub<-cbind(x2008_sub, day_$Day)
colnames(x2008_sub)[20] <- "Day"
x2008_sub<- subset(x2008_sub, Day==31)
x2008_sub<-cbind(x2008_sub, x2007_sub$Voltage/60)
x2008_sub<-cbind(x2008_sub, x2007_sub$Global_intensity/60)
colnames(x2008_sub)[21]<-"Voltage_WH"
colnames(x2008_sub)[22]<- "Intensity_WH"


x2008_sub_graph<-x2008_sub %>%
                    gather(Submeter, Consump, S1.Kitchen:ActivePower_WH)
ggplot() +
          geom_line(data=x2008_sub_graph,
                    aes(x=DateTime, y=Consump, color=Submeter),
                    alpha = 0.4, size = 4) + 
                    labs(x="Time", y="Watt/hour",
                         title="Consumption by Submeter on August 31st 2008") + 
                    theme_linedraw(base_size = 11, base_family = "")

#We can see how intxvolt == ActivePower again:
x2008_sub_EL<- x2008_sub%>%
                    gather(Power, Consump,ActivePower_WH, Reactive_WH,VoltxInt_WH)
ggplot()+
        geom_line(data=x2008_sub_EL,
                  aes(x=DateTime, y=Consump, color=Power),
                  alpha = 0.4, size = 4) + 
                    labs(x="Time", y="Watt/hour",
                         title="Power Consumption on August 31st 2008") +
                    theme_linedraw(base_size = 11, base_family = "")

                    
# We prove as intensity is 100% corr with active power: 
                    
x2008_prova<- x2008_sub %>% 
                    select(DateTime, ActivePower_WH, Reactive_WH, Voltage_WH, Intensity_WH )
                    
X2008NORM <- lapply(x2008_prova, function(x) if(is.numeric(x)){scale(x, center=T, scale=T)}else x) 
df_norm2008<-as.data.frame(X2008NORM)
                    
df_norm_graph<- df_norm2008%>% 
                    gather(Power, Consump, ActivePower_WH:Intensity_WH)
                    
ggplot() +
       geom_line(data=df_norm_graph, 
                 aes(x=DateTime, y=Consump, color=Power),
                 alpha = 0.4, size = 4) + 
                    labs(x="Time", y="Watt/hour", title="Power Consumption on August 31st 2008") + 
                    theme_linedraw(base_size = 11, base_family = "")

# To find out the incredible corr:
x<- df_norm[2:5]
cor(x)
## Conculsion: Physics are proved --> V*I=Power


## TIME SERIES##

# Prepared some previous info - Mean by years:
#Created a column with Monty and Days:
MonthDay<-separate(JanFeb, Date, into=c("year", "month", "day"))
MonthDay<-as.Date(paste(MonthDay$day, MonthDay$month, sep="/"), format="%d/%m") # hem surt el putu any, pero sino no ho consegueixo transformar a DATE( necesari perl group_by)
JanFeb<-cbind(JanFeb, MonthDay)


tbl_JanFeb<- JanFeb%>% 
   group_by(month)%>%
   group_by(Day)%>%
   summarise(ActivePower_WH = mean(ActivePower_WH, na.rm = TRUE),
              S1.Kitchen = mean(S1.Kitchen, na.rm=TRUE),
              S2.Laundry_room = mean(S2.Laundry_room, na.rm=TRUE),
              S3.WaterHeater_AirConditioner = mean(S3.WaterHeater_AirConditioner, na.rm= TRUE))

tbl_JanFeb<-tbl_JanFeb%>%
  gather(Submeter, Consump, ActivePower_WH:S3.WaterHeater_AirConditioner)

ggplot()+
  geom_line(data=tbl_JanFeb, aes(x=Day, y=Consump, color=Submeter))


# Store the data in a time series with appropriate start, end, and frequency: 
# First, let's group our data:

x2008<- subset(Consumption, Year == 2008)

# medium of every day: 
tbl_2008<- x2008 %>%
  group_by(Date)%>%
  summarise(ActivePower_WH = mean(ActivePower_WH, na.rm = TRUE),
            S1.Kitchen = mean(S1.Kitchen, na.rm=TRUE),
            S2.Laundry_room = mean(S2.Laundry_room, na.rm=TRUE),
            S3.WaterHeater_AirConditioner = mean(S3.WaterHeater_AirConditioner, na.rm= TRUE))

consumption_2008 <- ggplot(year, aes(x=Date, group=1)) + geom_line(aes(y=S1.Kitchen, color="Sub-Metering 1")) + 
  geom_line(aes(y=S2.Laundry_room, color="Sub-Metering 2")) + 
  geom_line(aes(y=S3.WaterHeater_AirConditioner, color="Sub-Metering 3")) + 
  geom_line(aes(y=ActivePower_WH, color="Active Power W")) 


##### TIME SERIES######### 
# we store the data as time series objetct ts()):  
# We need 1 variable by Frequency observation. Hence, in next case, a day by month. 

#### *** BY MONTH: #### 

stat<-function(x) c(mean=mean(x))  
ag_prova<- aggregate(ActivePower_WH ~ MonthYear, Consumption, stat)  # days mean
tsMonth<- ts(ag_prova$ActivePower_WH, frequency = 12, start=c(2006, 12), end = c(2010, 11))
plot.ts(tsMonth)

stat<-function(x) c(mean=mean(x))  
ag_prova1<- aggregate(S1.Kitchen ~ MonthYear, Consumption, stat)  # days mean
tsS1<- ts(ag_prova1$S1.Kitchen, frequency = 12, start=c(2006, 12), end = c(2010, 11))
plot.ts(tsS1)

stat<-function(x) c(mean=mean(x)) 
ag_prova2<- aggregate(S2.Laundry_room ~ MonthYear, Consumption, stat)  
tsS2<- ts(ag_prova2$S2.Laundry_room, frequency = 12, start=c(2006, 12), end = c(2010, 11))
plot(tsS2)

stat<-function(x) c(mean=mean(x)) 
ag_prova3<- aggregate(S3.WaterHeater_AirConditioner ~ MonthYear, Consumption, stat)  
tsS3<- ts(ag_prova3$S3.WaterHeater_AirConditioner, frequency = 12, start=c(2006, 12), end = c(2010, 11))
plot(tsS3)

combind_Sub<- cbind(tsS1, tsS2, tsS3)

ggplot() +
  geom_line(data=df_norm_graph, aes(x=DateTime, y=Consump, color=Power), alpha = 0.4, size = 4) + 
  labs(x="Time", y="Watt/hour", title="Power Consumption on August 31st 2008") + 
  theme_linedraw(base_size = 11, base_family = "")


ggplot() +
  geom_line(data=combind_Sub, aes(x=Time, y=tsS1+tsS2+tsS3))

a<- plot(combind_Sub, plot.type = "single", col=c("blue", "red", "black"), main="Daily average sub-meters consumption prices", ylab="Consumption Watt/Hour")
ab <- a + 
  legend("topright", title="Submeters", legend= c("S1", "S2", "S3"), col= c("blue","red", "black"), lty = 1:3 ) 

#### ***BY DAY ####

# We use the first two years to check it with the following ones: 
lesspoints2<- subset(Consumption, Year==2007 | Year==2008)
lesspoints2$Date <- as.Date(lesspoints2$Date, "%Y/%m/%d")
#stat <- function(x) c(min = min(x), max = max(x), mean = mean(x)) sum=sum(x)  SI VUI MAX I MIN i MEAN - Hem creat una funció
stat<-function(x) c(mean=mean(x))  
ag <- aggregate(ActivePower_WH ~ Date, lesspoints2, stat) # Faig la mitjana del dia

tsDay<-ts(ag$ActivePower_WH, frequency = 365, start=c(2007,01))
plot(tsDay) 

# DECOMPOSING: aim: remove seasonal component

#Case A: by Month
decompose_tsmonth<- decompose(tsMonth)
plot(decompose_tsmonth)

# We adjust by seasons with the "seasonal" component of the decomposing: 
adjusted_decompose_tsmonth<- tsMonth - decompose_tsmonth$seasonal
plot(adjusted_decompose_tsmonth, main="Seasonable adjusted Active Power", ylab="Consumption Watt/hour") # the seasonal variation has been removed, now we just have the trend and the irregular component. 

#Case B: by Day:
decompose_tsday<- decompose(tsDay)
plot(decompose_tsday)
adjusted_decompose_tsday<- tsDay - decompose_tsday$seasonal
plot(adjusted_decompose_tsday, main="Seasonable adjusted Active Power", ylab="Consumption Watt/hour")


#FORECAST: I didn't remove season in month, but removed season in days.  

# For the forecasting, first we have to create a dataset with 1st col the ts and second the Time as numeric:
#By Month:
table_fore<- data.frame(data=tsMonth, as.numeric(time(tsMonth)))
colnames(table_fore)<- c("Active_Energy", "Time")

mymodel<- tslm(Active_Energy~trend+season, table_fore) 

forecast_month<- forecast(mymodel, h=120)  # we forecast the next 10 years
autoplot(forecast_month)
summary(forecast_month) #RMSE= 1,72 MAPE=8,72
forecast_month2<-forecast(mymodel, h=12) # we forecast the next year. 
autoplot(forecast_month2)
summary(forecast_month2) # RMSE=1,72 MAPE=8,72

#By Day: 

table_day<-data.frame(data=tsDay, as.numeric(time(tsDay)))
colnames(table_day)<- c("Active_Energy", "Time")

mymodel2<-tslm(Active_Energy~trend, table_day)
forecast_day<-forecast(mymodel2, h=60)
autoplot(forecast_day)  # Compare with what actually happen 
#we check the error measures of our predictions:
summary(forecast_month)  #RMSE= 2,69 MAPE= 8.77
summary(forecast_month2)  # RMSE 4,83.67 MAPE=27.45  # NO SEASON: RMSE=2.9 MAPE=9.5
summary(forecast_day)  # It does not have season: RMSE= 7,49 and MAPE=50,84

#By Submeters:
table_S1<- data.frame(data=tsS1, as.numeric(time(tsS1)))
colnames(table_S1)<-c("Active_Energy", "Time")
mymodelS1<-tslm(Active_Energy~trend+season, table_S1)
forecastS1<- forecast(mymodelS1, h=12)
autoplot(forecastS1)
summary(forecastS1) #RMSE=0,2 
autoplot(forecastS1) +
                    ggtitle("Forecast 1 Year consumption S1-Kitchen" ) +
                    xlab("Time") +
                    ylab("Consumption Watt/Hour") + 
                    theme_linedraw(base_size = 11, base_family = "") +
                    theme(plot.title = element_text(hjust = 0.5))

table_S2<- data.frame(data=tsS2, as.numeric(time(tsS2)))
colnames(table_S2)<-c("Active_Energy", "Time")
mymodelS2<-tslm(Active_Energy~trend+season, table_S2)
forecastS2<- forecast(mymodelS2, h=12)
autoplot(forecastS2)
summary(forecastS2) #RMSE=0,2 
autoplot(forecastS2) +
                    ggtitle("Forecast 1 Year consumption S2-Laundry Room " ) +
                    xlab("Time")+ylab("Consumption Watt/Hour") + 
                    theme_linedraw(base_size = 11, base_family = "") + 
                    theme(plot.title = element_text(hjust = 0.5))

table_S3<- data.frame(data=tsS3, as.numeric(time(tsS3)))
colnames(table_S3)<-c("Active_Energy", "Time")
mymodelS3<-tslm(Active_Energy~trend+season, table_S3)
forecastS3<- forecast(mymodelS3, h=12)
autoplot(forecastS3)
summary(forecastS3) # 0,73
autoplot(forecastS3) + 
                    ggtitle("Forecast 1 Year consumption S3-Water heater & Air Condtioner" )+
                    xlab("Time") +
                    ylab("Consumption Watt/Hour") + 
                    theme_linedraw(base_size = 11, base_family = "") + 
                    theme(plot.title = element_text(hjust = 0.5))

#### HoltWithers forecast - for additive series: ####
#By Month
holt_forecast_month<- HoltWinters(tsMonth, beta=FALSE, gamma=FALSE, l.start=25.00000) # alpha=0.14 # in thel.start we choose where the predictions point start, if we dont write it it will start with the num of the first observation. 
holt_forecast_month$fitted
plot(holt_forecast_month)  # the red line are the predictions
holt_forecast_month$SSE # the RSS = 609  ( sum of squared error)
prediction_HoltWinters_month<- forecast(holt_forecast_month, h=12)
plot(prediction_HoltWinters_month)
acf(prediction_HoltWinters_month$residuals, na.action=na.pass, lag.max = 20) # we check the corr between forecast errors ( residuals) and succesive predictions. If there is no corr between then, we Can'T imporve the model. Otherwise, we can improve it with another forecasting technique. # We can see how they are corr becaouse the line ACF touches one of the bars of Lag, but lets make sure. 
Box.test(prediction_HoltWinters_month$residuals, lag=20, type="Ljung-Box") # We can see with the P-value that are correlated, hence we could imporve the model. 
plot(prediction_HoltWinters_month$residuals) # we can plot the errors. 
summary(prediction_HoltWinters_month) #RMSE=3,60 MAPE=19,9

#By Days
holt_forecast_day<-HoltWinters(adjusted_decompose_tsday, beta=FALSE, gamma=FALSE)
plot(holt_forecast_day)
holt_forecast_day$SSE 
prediction_HoltWinters_Day<-forecast(holt_forecast_day, h=60)
plot(prediction_HoltWinters_Day)
acf(prediction_HoltWinters_Day$residuals, na.action=na.pass, log.max=20)
Box.test(prediction_HoltWinters_Day$residuals, lag=20, type="Ljung-Box")
plot(prediction_HoltWinters_Day)
summary(prediction_HoltWinters_Day) #RMSE=5,6 MAPE=27,67

#### ARIMA MODEL: ARIMA(p,d,q)####
# we need: uncorrelated forecast errors + contant variance + mean=0 (normally distributed)
# To find manually p,d,q:
a<-diff(tsMonth, differences=1). 
plot.ts(a) # we achieve worst mean, because we already don`t have season.  
acf(a, lag.max=10)
acf(a, lag.max=10, plot= FALSE)  # for the p 
pacf(a, lag.max=10)  # for the q

# to find the ARIMA automatically: - recommended. 
autoarima_month <- auto.arima(tsMonth) # We use this one because we dont want the season 
summary(autoarima_month)  #my ARIMA is (000)(0,1,1)[12] 

foreArimaMonth<-forecast(autoarima_month, h=12)
plot(foreArimaMonth)
Box.test(foreArimaMonth$residuals, lag=20, type="Ljung-Box") # We check the P-value, etc. 
summary(foreArimaMonth) # RMSE=2,10 MAPE=9,74
autoplot(foreArimaMonth) +
                    ggtitle("Forecast 1 Year Consumption Active Energy") +
                    xlab("Time")+ylab("Average Consumption Watt/Hour") + 
                    theme_linedraw(base_size = 11, base_family = "") + 
                    theme(plot.title = element_text(hjust = 0.5))

# Without season: 

autoarima_month2<- auto.arima(adjusted_decompose_tsmonth)
summary(autoarima_month2)
foreArimaMonth2<-forecast(autoarima_month2, h=32)
summary(foreArimaMonth2)
plot(foreArimaMonth2)
autoplot(foreArimaMonth2) +
                    ggtitle("Forecast 3 Years Total Consumption Active Energy") +
                    xlab("Time")+ylab("Average Consumption Watt/Hour") + 
                    theme_linedraw(base_size = 11, base_family = "") + 
                    theme(plot.title = element_text(hjust = 0.5))

# Same Arima for h=2 ( predicting jsut 2 months

foreArimaMonth2<-forecast(autoarima_month, h=2)
plot(foreArimaMonth2)
Box.test(foreArimaMonth2$residuals, lag=20, type="Ljung-Box") # We check the P-value, etc. 
summary(foreArimaMonth2) # Same metrix RMSE=2,10 MAPE =9,7

#Arima model By Days:
autoarima_days<-auto.arima(adjusted_decompose_tsday)
summary(autoarima_days) #Arima(2,1,2)
foreArimaDay<-forecast(autoarima_days, h=60)
plot(foreArimaDay)
Box.test(foreArimaDay$residuals, log=20, type="Ljung-Box")
summary(foreArimaDay) #RMSE=4,9 MAPE=26,55

# Arima by Submeter:
autoarima_S1<-auto.arima(tsS1)
summary(autoarima_S1)
foreArimaS1<-forecast(autoarima_S1, h=12)
plot(foreArimaS1)
summary(foreArimaS1) #RMSE=0,30  (Lineal model is better)


autoarima_S3<-auto.arima(tsS3)
summary(autoarima_S3)
foreArimaS3<-forecast(autoarima_S3, h=12)
plot(foreArimaS3)
summary(foreArimaS3)  ## better lineal model 


#ARMA MODEL: ARMA(p,q) 
install.packages("fArma")
library(fArma)
fit_ARMA_Month<-armaFit(~arma(0,0)(0,1), data=tsMonth)  # We use (0,0) because of the resoults in ARIMA (0,0,0)

# WINNER MODELS:
#BY MONTH== ARIMA
#BY DAY == HOLTWINTERS
# SUBMETERS == LINEAL MODEL 

