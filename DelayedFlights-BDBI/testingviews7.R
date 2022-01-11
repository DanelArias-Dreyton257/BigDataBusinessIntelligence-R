df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

library(ggplot2)
library(dplyr)

start = as.Date("2008-11-22")
end = as.Date("2008-12-22")

dfCode <-df1[df1$Cancelled==1,]

filtro = dfCode$Date >= start & dfCode$Date<=end

dfCode<-dfCode[filtro,]

dfCode$CancellationCode[dfCode$CancellationCode=="A"]="A-Carrier"
dfCode$CancellationCode[dfCode$CancellationCode=="B"]="B-Weather"
dfCode$CancellationCode[dfCode$CancellationCode=="C"]="C-NAS"
dfCode$CancellationCode[dfCode$CancellationCode=="D"]="D-Security"

dfCode<-aggregate(x=dfCode %>%select(X), by=list(dfCode$CancellationCode), FUN=length)

dfCode<-dfCode %>% rename(
  CancellationCode = Group.1,
  Count = X
)

ggplot(dfCode, aes(x="", y=Count, fill=CancellationCode)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()
