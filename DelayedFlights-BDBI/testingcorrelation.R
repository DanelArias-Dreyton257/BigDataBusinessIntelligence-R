df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library("dplyr")

df1<- df1[sample(nrow(df1), size=500000), ] #coger una muetsra para que shiny no tarde tanto

data<-df1 %>%select(ActualElapsedTime,
                    CRSElapsedTime,
                    AirTime,
                    ArrDelay,
                    DepDelay,
                    Distance,
                    TaxiIn,
                    TaxiOut,
                    CarrierDelay,
                    WeatherDelay,
                    NASDelay,
                    SecurityDelay,
                    LateAircraftDelay
                    )

chart.Correlation(data, histogram = TRUE, method = "pearson")
