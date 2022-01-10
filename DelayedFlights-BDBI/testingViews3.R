df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

library("dplyr")
library(ggplot2)

start = as.Date("2008-11-08")
end = as.Date("2008-12-25")

filtro = df1$Date >= start & df1$Date<=end

df1<-na.omit(df1)

df1$DelayCRS = df1$ActualElapsedTime - df1$CRSElapsedTime

df1$Delayed = df1$DelayCRS > 0

df2<-aggregate(x=df1 %>%select(Delayed), by=list(df1$Delayed, df1$Date), FUN=length)

df3<-aggregate(x=df1 %>% select(Date),by=list(df1$Date),FUN=length)

colnames(df2)<-c("Delayed", "Date", "Count")

df2<-df2[df2$Delayed==TRUE,]%>%select(Date, Count)

df2$Date <- as.Date(df2$Date)
  
colnames(df3)<-c("Date", "Count")

df3$Date <- as.Date(df3$Date)

df4 = merge(x=df2, y=df3, by = c("Date"), all= TRUE,sort = FALSE)

colnames(df4)<-c("Date", "DelayedCount", "TotalCount")

df4$Date = as.Date(df4$Date)

df4<-na.omit(df4[filtro,])

ggplot(data = df4) +
  geom_line(aes(x = Date, y = TotalCount), color = 'green4') +
  geom_line(aes(x = Date, y = DelayedCount), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b %d') +
  scale_y_continuous(sec.axis = sec_axis(~ . / 8000,
                                         labels = function(y){paste0(y*100, '%')},
                                         name = 'Percentage of delayed flights')) +
  labs(x = 'Time', y = 'Number of daily flights', caption = 'Source: publicly available data from DoT',
       title = 'Number of flights in the US in 2015') +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.75), plot.caption = element_text(vjust = 7),
        axis.title.y.left = element_text(color = 'green4'),
        axis.title.y.right = element_text(color = 'red'))
#____________________________________________________________________________________________________-

start = as.Date("2008-01-12")
end = as.Date("2008-02-25")

dtt<-na.omit(df1)

dtt$Delayed = dtt$TotalTimeDifference > 0

dtt2<-aggregate(x=dtt %>%select(Delayed), by=list(dtt$Delayed, dtt$Date), FUN=length)

df3<-aggregate(x=dtt %>% select(Date),by=list(dtt$Date),FUN=length)

colnames(dtt2)<-c("Delayed", "Date", "Count")

dtt2<-dtt2[dtt2$Delayed==TRUE,]%>%select(Date, Count)

dtt2$Date <- as.Date(dtt2$Date)

colnames(df3)<-c("Date", "Count")

df3$Date <- as.Date(df3$Date)

df4 = merge(x=dtt2, y=df3, by = c("Date"), all= TRUE,sort = FALSE)

colnames(df4)<-c("Date", "DelayedCount", "TotalCount")

df4$Date = as.Date(df4$Date)

filtro = df4$Date >= start & df4$Date<=end

df4<-df4[filtro,]

ggplot(data = df4) +
  geom_line(aes(x = Date, y = TotalCount), color = 'green4') +
  geom_line(aes(x = Date, y = DelayedCount), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b %d') +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(df4$TotalCount),
                                         labels = function(y){paste0(y*100, '%')},
                                         name = 'Percentage of delayed flights')) +
  labs(x = 'Time', y = 'Number of daily flights', caption = 'Source: publicly available data from DoT',
       title = 'Number of flights in the US in 2015') +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.75), plot.caption = element_text(vjust = 7),
        axis.title.y.left = element_text(color = 'green4'),
        axis.title.y.right = element_text(color = 'red'))
