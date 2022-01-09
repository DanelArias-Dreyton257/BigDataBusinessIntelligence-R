df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

library(ggplot2)                         # Plots
library(ggrepel)                         # Nice labels
library(gganimate)                       # Animations
library(ggspatial);library(sf)           # Map plots
library(maps);library(rnaturalearth)     # Map data
theme_set(theme_bw())                    # Set theme for all plots
library("sf")
library("tools")
library("rnaturalearthdata")
library(dplyr)

#1 agrupar por (mes o semanana) sumando en cada tipo tipo de delay

df1<-na.omit(df1)

df1$DelayCRS = df1$ActualElapsedTime - df1$CRSElapsedTime

df1$Month = as.POSIXlt(df1$Date)$mon +1

df2<-aggregate(x=df1 %>% select(ArrDelay,DepDelay,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,TotalDelay, DelayCRS),by=list(df1$Month),FUN=sum)

df2$percNoEsperado = df2$DelayCRS / df2$TotalDelay

df2$ArrDelayReal = df2$ArrDelay * df2$percNoEsperado
df2$DepDelayReal = df2$DepDelay * df2$percNoEsperado
df2$CarrierDelayReal = df2$CarrierDelay  * df2$percNoEsperado
df2$WeatherDelayReal = df2$WeatherDelay  * df2$percNoEsperado
df2$NASDelayReal = df2$NASDelay  * df2$percNoEsperado
df2$SecurityDelayReal = df2$SecurityDelay * df2$percNoEsperado
df2$LateAircraftDelayReal = df2$LateAircraftDelay  * df2$percNoEsperado


df2$ArrDelayPerc = df2$ArrDelayReal / df2$TotalDelay
df2$DepDelayPerc = df2$DepDelayReal / df2$TotalDelay
df2$CarrierDelayPerc = df2$CarrierDelayReal / df2$TotalDelay
df2$WeatherDelayPerc = df2$WeatherDelayReal / df2$TotalDelay
df2$NASDelayPerc = df2$NASDelayReal / df2$TotalDelay
df2$SecurityDelayPerc = df2$SecurityDelayReal / df2$TotalDelay
df2$LateAircraftDelayPerc = df2$LateAircraftDelayReal / df2$TotalDelay

months = df2$"Group.1"

delayTypes <- c("ArrDelay", "DepDelay", "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay")

df3<-data.frame(NA,NA)
names(df3)<-c("Month","DelayType")

for(i in 1:length(months)) {
  for(j in 1:length(delayTypes)) {
    print(months[i])
    print(delayTypes[j])
    print("-----------")
    row <- c(months[i], delayTypes[j])
    df3<-rbind(df3,row)
  }
}
df3<-na.omit(df3)

df3$Month[1]

for (i in 1:dim(df2)[1]){
  df3$Perc[df3$Month==i & df3$DelayType=="ArrDelay"] <- df2$ArrDelayPerc[df2$"Group.1"==i]
  df3$Perc[df3$Month==i & df3$DelayType=="DepDelay"] <- df2$DepDelayPerc[df2$"Group.1"==i]
  df3$Perc[df3$Month==i & df3$DelayType=="CarrierDelay"] <- df2$CarrierDelayPerc[df2$"Group.1"==i]
  df3$Perc[df3$Month==i & df3$DelayType=="WeatherDelay"] <- df2$WeatherDelayPerc[df2$"Group.1"==i]
  df3$Perc[df3$Month==i & df3$DelayType=="NASDelay"] <- df2$NASDelayPerc[df2$"Group.1"==i]
  df3$Perc[df3$Month==i & df3$DelayType=="SecurityDelay"] <- df2$SecurityDelayPerc[df2$"Group.1"==i]
  df3$Perc[df3$Month==i & df3$DelayType=="LateAircraftDelay"] <- df2$LateAircraftDelayPerc[df2$"Group.1"==i]
}

df3$Month <- as.Date(paste(paste("2008-",df3$Month,sep=""),"-01",sep=""))

#Buscar una forma de por cada uno de los delays crear una columna
 
 
 ggplot(data = df3) +
   geom_bar(aes(x = Month, y = Perc, fill = DelayType),
            position = 'stack', stat = 'identity') +
   scale_x_date(date_breaks = '1 month', date_labels = '%b') +
   scale_y_continuous(labels = function(x) paste0(x*100, '%')) +
   labs(x = 'Time', y = 'Percentage of weekly flights', fill = 'Delay type',
        title = 'Breakdown of delay type of flights of the US in 2008',
        subtitle = 'Subtitle',
        caption = 'Caption') +
   theme(axis.text.x = element_text(angle = 25, vjust = 0.75), plot.caption = element_text(vjust = 7))
 
 