df1 <- read.csv('vuelosLimpio.csv', header=TRUE, sep=",")
df2 <- read.csv('table-1-states2.csv', header=TRUE, sep=",")

head(df2)

df2$Role = NULL
df2$Enplanements = NULL

df2[df2$FAA == "",] = NA

filtro = is.na(df2$FAA)

head(df2)

df2Final = df2[filtro == FALSE,]

write.csv(df2Final,"IATANames.csv", row.names = FALSE)

df3 <- read.csv("IATANames.csv", header=TRUE, sep=",")

df3$ICAO=NULL
df3$FAA = NULL

head(df3)

origenes = merge(x=df1, y=df3, by.x=c('Origin'),by.y=c('IATA'), all.x= TRUE,sort = FALSE)
head(origenes)

library(tidyverse)

origenes <- origenes %>% 
  rename(
    OriginCode = Origin,
    OriginCity = City,
    OriginAirport = Airport,
    OriginState = State
  )

destinos = merge(x=origenes, y=df3, by.x=c('Dest'),by.y=c('IATA'), all.x= TRUE,sort = FALSE)
head(destinos)


destinos <- destinos %>% 
  rename(
    DestCode = Dest,
    DestCity = City,
    DestAirport = Airport,
    DestState = State
  )
df4 = destinos

head(df4)
summary(df4)

write.csv(df4,"vuelosLimpio2.csv", row.names = FALSE)
