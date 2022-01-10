df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

library(ggplot2)                         # Plots
library(dplyr)
library(data.table)

df1<-na.omit(df1)

df1$DelayCRS = df1$ActualElapsedTime - df1$CRSElapsedTime

df2<-aggregate(x=df1 %>% select(ArrDelay,DepDelay,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,TotalDelay, DelayCRS),by=list(cut(as.Date(df1$Date), "week")),FUN=sum)

df2 <- df2 %>% rename(Week = Group.1)

df2$DelayCRS[df2$DelayCRS<0]=0

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

dat <- data.table(ArrDelay = df2$ArrDelayPerc,
                  DepDelay = df2$DepDelayPerc,
                  CarrierDelay = df2$CarrierDelayPerc,
                  WeatherDelay = df2$WeatherDelayPerc,
                  NASDelay = df2$NASDelayPerc,
                  SecurityDelay = df2$SecurityDelayPerc,
                  LateAircraftDelay = df2$LateAircraftDelayPerc,
                  Week = df2$Week)

dat1 = melt(dat, id.vars= c("Week"))

dat1$Week = as.Date(dat1$Week)

ggplot(data = dat1) +
  geom_bar(aes(x = Week, y = value, fill = variable),
           position = 'stack', stat = 'identity') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b %d') +
  scale_y_continuous(labels = function(x) paste0(x*100, '%')) +
  labs(x = 'Time', y = 'Percentage of weekly flights', fill = 'Delay type',
       title = 'Breakdown of delay type of flights in the northern part of the US in 2015',
       subtitle = 'States that are part of the analysis include AK, IL, IN, MA, ME, MI, MN, NH, NY, VT',
       caption = 'Source: publicly available data from DoT') +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.75), plot.caption = element_text(vjust = 7))
#____________________________________________________________________________________________
df3<-df1

df3$DelayCRS = df3$ActualElapsedTime - df3$CRSElapsedTime

df4<-aggregate(x=df3 %>% select(ArrDelay,DepDelay,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,TotalDelay, DelayCRS),by=list(cut(as.Date(df3$Date), "week")),FUN=sum)

df4 <- df4 %>% rename(Week = Group.1)

df4$DelayCRS[df4$DelayCRS<0]=0

df4$percNoEsperado = df4$DelayCRS / df4$TotalDelay

df4$ArrDelayReal = df4$ArrDelay * df4$percNoEsperado
df4$DepDelayReal = df4$DepDelay * df4$percNoEsperado
df4$CarrierDelayReal = df4$CarrierDelay  * df4$percNoEsperado
df4$WeatherDelayReal = df4$WeatherDelay  * df4$percNoEsperado
df4$NASDelayReal = df4$NASDelay  * df4$percNoEsperado
df4$SecurityDelayReal = df4$SecurityDelay * df4$percNoEsperado
df4$LateAircraftDelayReal = df4$LateAircraftDelay  * df4$percNoEsperado


df4$ArrDelayPerc = df4$ArrDelayReal / df4$TotalDelay
df4$DepDelayPerc = df4$DepDelayReal / df4$TotalDelay
df4$CarrierDelayPerc = df4$CarrierDelayReal / df4$TotalDelay
df4$WeatherDelayPerc = df4$WeatherDelayReal / df4$TotalDelay
df4$NASDelayPerc = df4$NASDelayReal / df4$TotalDelay
df4$SecurityDelayPerc = df4$SecurityDelayReal / df4$TotalDelay
df4$LateAircraftDelayPerc = df4$LateAircraftDelayReal / df4$TotalDelay

dat <- data.table(ArrDelay = df4$ArrDelayPerc,
                  DepDelay = df4$DepDelayPerc,
                  CarrierDelay = df4$CarrierDelayPerc,
                  WeatherDelay = df4$WeatherDelayPerc,
                  NASDelay = df4$NASDelayPerc,
                  SecurityDelay = df4$SecurityDelayPerc,
                  LateAircraftDelay = df4$LateAircraftDelayPerc,
                  Week = df4$Week)

dat1 = melt(dat, id.vars= c("Week"))

dat1$Week = as.Date(dat1$Week)

ggplot(data = dat1) +
  geom_bar(aes(x = Week, y = value, fill = variable),
           position = 'stack', stat = 'identity') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b %d') +
  scale_y_continuous(labels = function(x) paste0(x*100, '%')) +
  labs(x = 'Time', y = 'Percentage of weekly flights', fill = 'Delay type',
       title = 'Breakdown of delay type of flights in the northern part of the US in 2015',
       subtitle = 'States that are part of the analysis include AK, IL, IN, MA, ME, MI, MN, NH, NY, VT',
       caption = 'Source: publicly available data from DoT') +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.75), plot.caption = element_text(vjust = 7))

