df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")


df1$DelayCRS = df1$ActualElapsedTime - df1$CRSElapsedTime

df1<-na.omit(df1)

df2<-aggregate(x=df1 %>% select(ArrDelay,DepDelay,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,TotalDelay, DelayCRS),by=list(df1$Date),FUN=sum)

df3<-aggregate(x=df1 %>% select(Date),by=list(df1$Date),FUN=length)

colnames(df2)<-c("Date", "ArrDelay","DepDelay","CarrierDelay","WeatherDelay","NASDelay","SecurityDelay","LateAircraftDelay","TotalDelay", "DelayCRS")
colnames(df3)<-c("Date", "Count")

df4 = merge(x=df2, y=df3, by = c("Date"), all= TRUE,sort = FALSE)

df4$percNoEsperado = df4$DelayCRS / df4$TotalDelay

df4$Date = as.Date(df4$Date)

ggplot(data = df4) +
  geom_line(aes(x = Date, y = Count), color = 'green4') +
  geom_line(aes(x = Date, y = percNoEsperado * 20000), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b %d') +
  scale_y_continuous(sec.axis = sec_axis(~ . / 20000,
                                         labels = function(y){paste0(y*100, '%')},
                                         name = 'Percentage of delayed flights')) +
  labs(x = 'Time', y = 'Number of daily flights', caption = 'Source: publicly available data from DoT',
       title = 'Number of flights in the US in 2015') +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.75), plot.caption = element_text(vjust = 7),
        axis.title.y.left = element_text(color = 'green4'),
        axis.title.y.right = element_text(color = 'red'))
