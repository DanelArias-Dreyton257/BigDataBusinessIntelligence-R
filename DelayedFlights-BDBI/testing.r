#Testing
df1 <- read.csv('vuelosLimpio.csv', header=TRUE, sep=",")

dim(df1)
head(df1)

#librerias
library("ggplot2")


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
