
df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

library(ggplot2)
library(dplyr)
# install.packages("treemapify")
library(treemapify)
# install.packages("ggplot2")
library(ggplot2)

dftot<-df1

dftot$Vuelo = paste(paste(dftot$OriginState,"-"), dftot$DestState) 

dfcan <- dftot[dftot$Cancelled==1,]

dfcan<-aggregate(x=dfcan %>% select(X),by=list(dfcan$Vuelo),FUN=length)

dfret<-dftot[dftot$TotalTimeDifference > 0,]

dfret<-aggregate(x=dfret %>%select(X), by=list(dfret$Vuelo), FUN=length)

dfcan <- dfcan %>% rename(Vuelo = Group.1,
                          Count=X)
dfret <- dfret %>% rename(Vuelo = Group.1,
                          Count=X)

dfcan<-dfcan[order(-dfcan$Count),]
dfret<-dfret[order(-dfret$Count),]

ggplot(dfcan[1:10,], aes(area = Count, fill = Vuelo,
               label = paste(Vuelo, Count, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none")

ggplot(dfret[1:10,], aes(area = Count, fill = Vuelo,
                         label = paste(Vuelo, Count, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none")
