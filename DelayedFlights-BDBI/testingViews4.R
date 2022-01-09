df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

library(data.table)
library(ggplot2)

start = as.Date("2008-11-08")
end = as.Date("2008-12-25")

filtro = df1$Date >= start & df1$Date<=end

dat <- data.table(ArrDelay = df1$ArrDelay[filtro],
                  DepDelay = df1$DepDelay[filtro],
                  CarrierDelay = df1$CarrierDelay[filtro],
                  WeatherDelay = df1$WeatherDelay[filtro],
                  NASDelay = df1$NASDelay[filtro],
                  SecurityDelay = df1$SecurityDelay[filtro],
                  LateAircraftDelay = df1$LateAircraftDelay[filtro])
dat1 = melt(dat)

ggplot(na.omit(dat1), aes(x = variable, y = value,fill = variable)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = 'Likelihood weather is causing delay', y = 'Percentage of delayed flights',
       title = 'Percentage of delayed flights vs likelihood of weather causing the delay',
       subtitle = "Subtitle",
       caption = 'Source: publicly available data from DoT') +
  theme(plot.caption = element_text(vjust = 7))



ggplot(data=df1[filtro,], aes(x=as.Date(Date), group=CancellationCode, color=CancellationCode)) +
  geom_density() +
  scale_x_date(date_breaks = '1 week', date_labels = '%b %d') +
  labs(x = 'Likelihood weather is causing delay', y = 'Percentage of delayed flights',
       title = 'Percentage of delayed flights vs likelihood of weather causing the delay',
       subtitle = "Subtitle",
       caption = 'Source: publicly available data from DoT') +
  theme(plot.caption = element_text(vjust = 7))
  