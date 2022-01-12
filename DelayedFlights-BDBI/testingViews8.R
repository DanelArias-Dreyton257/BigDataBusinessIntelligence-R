
df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

library(ggplot2)
library(dplyr)
# install.packages("treemapify")
library(treemapify)
# install.packages("ggplot2")
library(ggplot2)


numMin=10

dftot<-df1

dftot$Vuelo = paste(paste(dftot$OriginState,"-"), dftot$DestState)



dfmax<-aggregate(x=dftot %>% select(X),by=list(dftot$Vuelo),FUN=length)

dfmax <- dfmax %>% rename(Vuelo = Group.1,
                          Count=X)





dfcan <- dftot[dftot$Cancelled==1,]

dfcan<-aggregate(x=dfcan %>% select(X),by=list(dfcan$Vuelo),FUN=length)

dfret<-dftot[dftot$TotalTimeDifference > 0,]

dfret<-aggregate(x=dfret %>%select(X), by=list(dfret$Vuelo), FUN=length)

dfcan <- dfcan %>% rename(Vuelo = Group.1,
                          Count=X)
dfret <- dfret %>% rename(Vuelo = Group.1,
                          Count=X)



dfcan <- dfcan %>% left_join(dfmax, by=c("Vuelo"))

dfcan<-dfcan[dfcan$'Count.y'>=numMin,]

dfcan$Perc=dfcan$'Count.x'/dfcan$'Count.y'



dfret <- dfret %>% left_join(dfmax, by=c("Vuelo"))

dfret<-dfret[dfret$'Count.y'>=numMin,]

dfret$Perc=dfret$'Count.x'/dfret$'Count.y'


dfcan<-dfcan[order(-dfcan$Perc),]
dfret<-dfret[order(-dfret$Perc),]

ggplot(dfcan[1:10,], aes(area = Perc, fill = Vuelo,
               label = paste(Vuelo, paste(round(Perc*100,2),"%",sep=""), sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none")


ggplot(dfret[1:10,], aes(area = Perc, fill = Vuelo,
                         label = paste(Vuelo, paste(round(Perc*100,2),"%",sep=""), sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none")
