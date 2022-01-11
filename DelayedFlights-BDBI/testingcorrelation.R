df1 <- read.csv("vuelosLimpioMap.csv", header=TRUE, sep=",")

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
install.packages("fansi")
install.packages("dplyr")
library("dplyr")

df1<- df1[sample(nrow(df1), size=100000), ] #coger una muetsra para que shiny no tarde tanto

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
pdf(file="correlationPlot.pdf")
chart.Correlation(data, histogram = TRUE, method = "pearson")
dev.off()

data2<-df1 %>%select(ArrDelay,
                    DepDelay,
                    TaxiIn,
                    TaxiOut,
                    CarrierDelay,
                    WeatherDelay,
                    NASDelay,
                    SecurityDelay,
                    LateAircraftDelay
)
pdf(file="correlationPlot2.pdf")
chart.Correlation(data2, histogram = TRUE, method = "pearson")
dev.off()
