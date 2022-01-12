df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

colnames(df1)

colorder <- c("X","Date",
              "FlightNum",
              "TailNum",
              "UniqueCarrier",
              "CRSElapsedTime",
              "ActualElapsedTime",
              "CRSDepTime",
              "DepTime",
              "CRSArrTime",
              "ArrTime",
              "AirTime",
              "Distance",
              "TaxiIn",
              "TaxiOut",
              "Cancelled",
              "CancellationCode",
              "OriginCode",
              "OriginState",
              "OriginCity",
              "OriginAirport",
              "DestCode",
              "DestState",
              "DestCity",
              "DestAirport",
              "ArrDelay",
              "DepDelay",
              "CarrierDelay",
              "WeatherDelay",
              "NASDelay",
              "SecurityDelay",
              "LateAircraftDelay",
              "TotalDelay",
              "TotalTimeDifference",
              "OriginX",
              "OriginY",
              "DestX",
              "DestY")

df1<-df1[,colorder]

df1<-df1[order(df1$X),]

write.csv(df1,"vuelosLimpioMap.csv", row.names = FALSE)
