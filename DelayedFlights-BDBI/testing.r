#Testing
df1 <- read.csv('vuelosLimpio2.csv', header=TRUE, sep=",")
df1$Date = as.Date(df1$Date)

dim(df1)
head(df1)

#librerias
library("ggplot2")
#aggreg
library(dplyr)
library(lubridate)
#mapas
library(ggrepel)                         # Nice labels
library(gganimate)                       # Animations
library(ggspatial);library(sf)           # Map plots
library(maps);library(rnaturalearth)     # Map data
theme_set(theme_bw())                    # Set theme for all plots


#visualizaciones
quantile(df1$TotalDelay[is.na(df1$TotalDelay)==FALSE])

filtro = (is.na(df1$TotalDelay)==FALSE) & (df1$TotalDelay >= -7) & (df1$TotalDelay<=10)
delay_filtrado = df1$TotalDelay[filtro]
summary(delay_filtrado)

#violin
ggplot(df1[filtro,], aes(x=FlightNum, y=TotalDelay, fill=FlightNum)) + geom_violin() #revisar

#histograma
hist(df1$TotalDelay[filtro])

#densidad
ggplot(df1[filtro,], aes(x=Date, group=UniqueCarrier, color=UniqueCarrier)) + geom_density()

#
df1 %>% group_by(Month=floor_date(Date, "month")) %>%
  summarize(TotDel=sum(TotalDelay))



ggplot(data = df1) +
  geom_bar(aes(x = Date, y = Distance, fill = UniqueCarrier),
           position = 'stack', stat = 'identity') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b %d') +
  scale_y_continuous(labels = function(x) paste0(x*100, '%')) +
  labs(x = 'Time', y = 'Y Label', fill = 'Fill',
       title = 'Title',
       subtitle = 'Subtitle',
       caption = 'Caption') +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.75), plot.caption = element_text(vjust = 7))
