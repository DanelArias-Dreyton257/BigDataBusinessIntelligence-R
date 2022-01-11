df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

library(ggplot2)
library(dplyr)
library(ggalt)

df2<-na.omit(df1)

dfheat<-aggregate(x=df2 %>%select(X), by=list(df2$OriginState, df2$DestState), FUN=length)

dfheat<-dfheat %>% rename(
  OriginState = Group.1,
  DestState = Group.2,
  Count = X
)

ggplot(data = dfheat) +
  geom_tile(aes(x = OriginState, y = DestState, fill=Count), color = 'black') +
  scale_fill_distiller(palette = 'Spectral') +
  labs(x = 'Estado de destino', y = 'Estado de origen', fill = 'Numero de vuelos',
       title = 'Mapa de calor de los vuelos relizados',
       subtitle = 'Según estado de origen y estado de destino',
       caption = 'caption') +
  theme(panel.grid.major = element_blank(),
        plot.caption = element_text(vjust = 7), axis.text.x = element_text(angle = 90, hjust=1), )

dfheat<-dfheat[order(-dfheat$Count),]

#Mostrar directamente los valores más altos (3) primeros

dfLoli<- dfheat[1:5,]

df3<-aggregate(x=df2 %>%select(CRSElapsedTime,ActualElapsedTime), by=list(df2$OriginState, df2$DestState), FUN=mean)

df3<-df3 %>% rename(
  OriginState = Group.1,
  DestState = Group.2,
)

dfLoli$Vuelo = paste(paste(dfLoli$OriginState,"-"), dfLoli$DestState)
df3$Vuelo = paste(paste(df3$OriginState,"-"), df3$DestState) 

dfLoli <- dfLoli %>% left_join(df3, by=c("Vuelo"))
dfLoli <- dfLoli %>%select(Vuelo, CRSElapsedTime, ActualElapsedTime)

dfLoli<-dfLoli[order(dfLoli$ActualElapsedTime),]

ggplot(dfLoli, aes(y=Vuelo, x=CRSElapsedTime, xend=ActualElapsedTime)) +
  geom_dumbbell(size=3, color="#e3e2e1",
                colour_x = "#5b8124", colour_xend = "#bad744") +
  labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05))
