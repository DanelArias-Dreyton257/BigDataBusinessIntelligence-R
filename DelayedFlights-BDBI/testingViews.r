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

states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
sf::sf_use_s2(FALSE)
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)

df1 = df1[order(df1$X),]

df3<-na.omit(df1)

stateName = "Ohio"

df2<-df3[df3$OriginState==stateName & df3$DestState!=stateName,]

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = states, aes(fill=ID)) +
  geom_curve(data = df2 %>% select(OriginState, OriginX, OriginY, DestX, DestY, DestState) %>% unique(), aes(x = OriginX, y = OriginY, xend = DestX, yend = DestY)) +
  geom_point(data = df2 %>% select(DestState, DestX, DestY) %>% unique(), aes(x = DestX, y = DestY), size = 3) +
  geom_point(data = df2 %>% select(OriginState, OriginX, OriginY) %>% unique(), aes(x = OriginX, y = OriginY), size = 3, color = 'red') +
  geom_label(data = df2 %>% select(OriginState, OriginX, OriginY) %>% unique(), nudge_x = 0, nudge_y = 0, size=2, color = 'red', aes(x = OriginX, y = OriginY, label = OriginState)) +
  geom_label(data = df2 %>% select(DestState, DestX, DestY) %>% unique(), nudge_x = 0, nudge_y =0,  size=2,aes(x = DestX, y = DestY, label = DestState)) +
  annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering,
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in")) +
  annotation_scale(location = 'bl', width_hint = 0.5) +
  coord_sf(xlim = c(-125, -64), ylim = c(24, 50)) +
  labs(title = 'Title',
       caption = 'Caption') +
  theme(panel.background = element_rect(fill = 'azure'), axis.title = element_blank(),legend.position = "None")

