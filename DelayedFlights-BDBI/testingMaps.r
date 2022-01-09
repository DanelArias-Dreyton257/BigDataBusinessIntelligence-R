df1 <- read.csv('vuelosLimpio2.csv', header=TRUE, sep=",")
df1$Date = as.Date(df1$Date)


library("sf")
library("maps")
library("tools")

states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
head(states)
sf::sf_use_s2(FALSE)
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)
head(states)

write.csv(states,"stateCoord.csv", row.names = FALSE)

library(ggplot2)

ggplot() +
  geom_sf(data = states, aes(fill = ID), color = 'black') +
  coord_sf(xlim = c(-125, -67), ylim = c(24, 50)) +
  labs(title = 'Title', subtitle = 'Sub',
       caption = 'Capt', fill = 'ID') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_blank())

states$geom = NULL

origenes = merge(x=df1, y=states, by.x=c('OriginState'),by.y=c('ID'), all.x= TRUE,sort = FALSE)
head(origenes)

library(tidyverse)

origenes <- origenes %>% 
  rename(
    X = X.x,
    OriginX = X.y,
    OriginY = Y,
  )

destinos = merge(x=origenes, y=states, by.x=c('DestState'),by.y=c('ID'), all.x= TRUE,sort = FALSE)
head(destinos)


destinos <- destinos %>% 
  rename(
    X = X.x,
    DestX = X.y,
    DestY = Y,
  )
df4 = destinos

head(df4)
summary(df4)

write.csv(df4,"vuelosLimpioMap.csv", row.names = FALSE)



