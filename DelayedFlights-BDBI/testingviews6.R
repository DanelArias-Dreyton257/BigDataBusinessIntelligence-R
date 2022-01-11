df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(data.table)

dfpata<-aggregate(x=df1 %>% select(Cancelled),by=list(cut(as.Date(df1$Date), "week"),df1$Cancelled),FUN=length)

dfpata <- dfpata %>% rename(Week = Group.1,
                            Cancelled=Group.2,
                            Count=Cancelled)

ggplot(data = dfpata) +
  geom_bar(aes(x = as.Date(Week), y = Count, fill = factor(Cancelled)),
           position = 'dodge', stat = 'identity')+
  scale_x_date(date_breaks = '1 month', date_labels = '%b %d') +
  labs(x = 'Time', y = 'Percentage of weekly flights', y="y label", fill = 'Delay type',
       title = 'Breakdown of delay type of flights in the northern part of the US in 2015',
       subtitle = 'States that are part of the analysis include AK, IL, IN, MA, ME, MI, MN, NH, NY, VT',
       caption = 'Source: publicly available data from DoT') +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.75), plot.caption = element_text(vjust = 7))+
  scale_fill_discrete(name = "Cancelado", labels = c("No Cancelado", "Cancelado"))
