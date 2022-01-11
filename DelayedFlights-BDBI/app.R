#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library("sf")
library("maps")
library("tools")
library(ggplot2)                         # Plots
library(ggrepel)                         # Nice labels
library(gganimate)                       # Animations
library(ggspatial);library(sf)           # Map plots
library(maps);library(rnaturalearth)     # Map data
theme_set(theme_bw())                    # Set theme for all plots
library("rnaturalearthdata")
library("dplyr")
library(data.table)
library(ggalt)

df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

df1<- df1[sample(nrow(df1), size=10000), ] #coger una muetsra para que shiny no tarde tanto

df2<-na.omit(df1)

states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
sf::sf_use_s2(FALSE)
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)

world <- ne_countries(scale = "medium", returnclass = "sf")

# Define UI for application that draws a histogram
ui <- navbarPage("Delayed Flights",
                 tabPanel("Introducción",
                          titlePanel("Visualizacion del mapa"),
                          verticalLayout(
                            h3("VIVA ESPAÑA")  
                          )
                 ),
                 tabPanel("Análisis por localización",
                          titlePanel("Visualizacion del mapa"),
                          verticalLayout(
                            plotOutput("heatmap"),
                            sidebarLayout(
                              sidebarPanel(tableOutput("fTable")),
                              mainPanel(plotOutput("lolipop"))
                            ),
                            plotOutput("mapPlot"),
                            wellPanel(
                              selectInput("state",
                                          "Elige un estado de origen:",
                                          states$ID
                              )
                            )
                            
                          )
                  ),
                 tabPanel("Visión general de 2008",
                          titlePanel("Visualizacion del Bar Plot"),
                          verticalLayout(
                            plotOutput("weekBarPlot"),
                            plotOutput("cancelledPlot")
                          )
                 ),
                 tabPanel("Análisis en periodos específicos",
                          titlePanel("Visualizacion del Date Slider"),
                          verticalLayout(
                            wellPanel(
                              dateRangeInput("daterange", "Elige el rango de fechas a mostrar:",
                                             language = "es",
                                             weekstart = 1,
                                             start="2008-01-01",
                                             end = "2008-12-31"),
                            ),
                            plotOutput("greenRedPlot"),
                            plotOutput("cancellationPlot"),
                            plotOutput("delayBoxPlot"),
                          )
                  ),
                 tabPanel("Correlación",
                          titlePanel("Visualizacion del mapa"),
                          verticalLayout(
                            h3("VIVA ESPAÑA")  
                          )
                 ),
                 tabPanel("Vuelos sensibles",
                          titlePanel("Visualizacion del mapa"),
                          verticalLayout(
                            plotOutput("greenRedPlot"),
                            plotOutput("cancellationPlot"),
                            plotOutput("delayBoxPlot"),
                          )
                 ),
                 tabPanel("Conclusiones",
                          titlePanel("Visualizacion del mapa"),
                          verticalLayout(
                            h3("VIVA ESPAÑA")  
                          )
                 ),
                 
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$mapPlot <- renderPlot({
      
      stateName <- input$state
      
      df3<-df2[df2$OriginState==stateName & df2$DestState!=stateName,]
      
      ggplot() +
        geom_sf(data = world) +
        geom_sf(data = states, aes(fill=ID)) +
        geom_curve(data = df3 %>% select(OriginState, OriginX, OriginY, DestX, DestY, DestState) %>% unique(), aes(x = OriginX, y = OriginY, xend = DestX, yend = DestY)) +
        geom_point(data = df3 %>% select(DestState, DestX, DestY) %>% unique(), aes(x = DestX, y = DestY), size = 3) +
        geom_point(data = df3 %>% select(OriginState, OriginX, OriginY) %>% unique(), aes(x = OriginX, y = OriginY), size = 3, color = 'red') +
        geom_label(data = df3 %>% select(DestState, DestX, DestY) %>% unique(), nudge_x = 0, nudge_y =1.5,  size=4,aes(x = DestX, y = DestY, label = DestState)) +
        geom_label(data = df3 %>% select(OriginState, OriginX, OriginY) %>% unique(), nudge_x = 0, nudge_y = 1.5, size=4, color = 'red', aes(x = OriginX, y = OriginY, label = OriginState)) +
        annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering,
                               pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in")) +
        annotation_scale(location = 'bl', width_hint = 0.5) +
        coord_sf(xlim = c(-125, -64), ylim = c(24, 50)) +
        labs(title = 'Mapa', x="Longitud representada en coordenada X", y="Latitud representada en coordenada Y",
             caption = 'Visualizacion de las coordenadas extraidas de Wikipedia') +
        theme(legend.position = "None")
    })
    
    output$delayBoxPlot <- renderPlot({
      start = as.Date(input$daterange[1])
      end = as.Date(input$daterange[2])
      
      filtro = df1$Date >= start & df1$Date<=end
      
      dat <- data.table(ArrDelay = df1$ArrDelay[filtro],
                        DepDelay = df1$DepDelay[filtro],
                        CarrierDelay = df1$CarrierDelay[filtro],
                        WeatherDelay = df1$WeatherDelay[filtro],
                        NASDelay = df1$NASDelay[filtro],
                        SecurityDelay = df1$SecurityDelay[filtro],
                        LateAircraftDelay = df1$LateAircraftDelay[filtro])
      dat1 = melt(dat)
      
      ggplot(na.omit(dat1), aes(x = variable, y = value,fill = variable)) +
        geom_boxplot() +
        coord_flip() +
        labs(x = 'Tipos de retrasos', y = 'Distribución de la cantidad de retraso en minutos',
             subtitle = 'Subtitulo',
             title = 'Visualización de la distribución de los diferentes tipos de retrasos',
             caption = 'Los datos representados corresponden con el rango de fechas elegido') +
        theme(plot.caption = element_text(vjust = -10))
    })
    
    output$cancellationPlot <- renderPlot({
      start = as.Date(input$daterange[1])
      end = as.Date(input$daterange[2])
      
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
        theme_void()+
        labs(title = paste('BlaBlaBla',sum(dfCode$Count)), x="Longitud representada en coordenada X", y="Latitud representada en coordenada Y",
             caption = 'Visualizacion de las coordenadas extraidas de Wikipedia')
      
    })
    
    output$cancelledPlot <- renderPlot({
      
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
      
    })
    
    output$weekBarPlot <- renderPlot({
      df3<-df2
      
      df4<-aggregate(x=df3 %>% select(ArrDelay,DepDelay,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay,TotalDelay, TotalTimeDifference),by=list(cut(as.Date(df3$Date), "week")),FUN=sum)
      
      df4 <- df4 %>% rename(Week = Group.1)
      
      df4$TotalTimeDifference[df4$TotalTimeDifference<0]=0
      
      df4$percNoEsperado = df4$TotalTimeDifference / df4$TotalDelay
      
      df4$ArrDelayReal = df4$ArrDelay * df4$percNoEsperado
      df4$DepDelayReal = df4$DepDelay * df4$percNoEsperado
      df4$CarrierDelayReal = df4$CarrierDelay  * df4$percNoEsperado
      df4$WeatherDelayReal = df4$WeatherDelay  * df4$percNoEsperado
      df4$NASDelayReal = df4$NASDelay  * df4$percNoEsperado
      df4$SecurityDelayReal = df4$SecurityDelay * df4$percNoEsperado
      df4$LateAircraftDelayReal = df4$LateAircraftDelay  * df4$percNoEsperado
      
      
      df4$ArrDelayPerc = df4$ArrDelayReal / df4$TotalDelay
      df4$DepDelayPerc = df4$DepDelayReal / df4$TotalDelay
      df4$CarrierDelayPerc = df4$CarrierDelayReal / df4$TotalDelay
      df4$WeatherDelayPerc = df4$WeatherDelayReal / df4$TotalDelay
      df4$NASDelayPerc = df4$NASDelayReal / df4$TotalDelay
      df4$SecurityDelayPerc = df4$SecurityDelayReal / df4$TotalDelay
      df4$LateAircraftDelayPerc = df4$LateAircraftDelayReal / df4$TotalDelay
      
      dat <- data.table(ArrDelay = df4$ArrDelayPerc,
                        DepDelay = df4$DepDelayPerc,
                        CarrierDelay = df4$CarrierDelayPerc,
                        WeatherDelay = df4$WeatherDelayPerc,
                        NASDelay = df4$NASDelayPerc,
                        SecurityDelay = df4$SecurityDelayPerc,
                        LateAircraftDelay = df4$LateAircraftDelayPerc,
                        Week = df4$Week)
      
      dat1 = melt(dat, id.vars= c("Week"))
      
      dat1$Week = as.Date(dat1$Week)
      
      ggplot(data = dat1) +
        geom_bar(aes(x = Week, y = value, fill = variable),
                 position = 'stack', stat = 'identity') +
        scale_x_date(date_breaks = '1 week', date_labels = '%b %d') +
        scale_y_continuous(labels = function(x) paste0(x*100, '%')) +
        labs(x = 'Semanas de 2008', y = 'Porcentaje de vuelos', fill = 'Delay type',
             title = 'Distribución de los tipos de retraso en los vuelos retrasados y el porcentaje influido en cada uno.',
             subtitle = '#TODO: Algo sobre como solo tenemos en cuenta los vuelos que han sido retrasados, si no hay no ha columna',
             caption = 'Caption') +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.75), plot.caption = element_text(vjust = 7))
      
    })
    
    output$greenRedPlot <- renderPlot({
      start = as.Date(input$daterange[1])
      end = as.Date(input$daterange[2])
      
      dtt<-df2
      
      dtt$Delayed = dtt$TotalTimeDifference > 0
      
      dtt2<-aggregate(x=dtt %>%select(Delayed), by=list(dtt$Delayed, dtt$Date), FUN=length)
      
      df3<-aggregate(x=dtt %>% select(Date),by=list(dtt$Date),FUN=length)
      
      colnames(dtt2)<-c("Delayed", "Date", "Count")
      
      dtt2<-dtt2[dtt2$Delayed==TRUE,]%>%select(Date, Count)
      
      dtt2$Date <- as.Date(dtt2$Date)
      
      colnames(df3)<-c("Date", "Count")
      
      df3$Date <- as.Date(df3$Date)
      
      df4 = merge(x=dtt2, y=df3, by = c("Date"), all= TRUE,sort = FALSE)
      
      colnames(df4)<-c("Date", "DelayedCount", "TotalCount")
      
      df4$Date = as.Date(df4$Date)
      
      filtro = df4$Date >= start & df4$Date<=end
      
      df4<-df4[filtro,]
      
      ggplot(data = df4) +
        geom_line(aes(x = Date, y = TotalCount), color = 'green4') +
        geom_line(aes(x = Date, y = DelayedCount), color = 'red') +
        scale_x_date(date_breaks = '1 week', date_labels = '%b %d') +
        labs(x = 'Semanas de 2008', y = 'Cantidad de vuelos', caption = 'Source: publicly available data from DoT',
             title = 'Comparación de vuelos retrasados respecto al total por cada semana') +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.75), plot.caption = element_text(vjust = 7),
              axis.title.y.left = element_text(color = 'green4'),
              axis.title.y.right = element_text(color = 'red'))
      
    })
    
    dfheat<-aggregate(x=df2 %>%select(X), by=list(df2$OriginState, df2$DestState), FUN=length)
    
    dfheat<-dfheat %>% rename(
      OriginState = Group.1,
      DestState = Group.2,
      Count = X
    )
    dfheat<-dfheat[order(-dfheat$Count),]
    
    output$heatmap<- renderPlot({
      ggplot(data = dfheat) +
        geom_tile(aes(x = OriginState, y = DestState, fill=Count), color = 'black') +
        scale_fill_distiller(palette = 'Spectral') +
        labs(x = 'Estado de destino', y = 'Estado de origen', fill = 'Numero de vuelos',
             title = 'Mapa de calor de los vuelos relizados',
             subtitle = 'Según estado de origen y estado de destino',
             caption = 'caption') +
        theme(panel.grid.major = element_blank(),
              plot.caption = element_text(vjust = 7), axis.text.x = element_text(angle = 90, hjust=1), )
    })
    
    output$fTable <- renderTable(dfheat[1:5,])
    
    output$lolipop <-renderPlot({
      dfLoli<- dfheat[1:5,]
      
      df3<-aggregate(x=df2 %>%select(CRSElapsedTime,ActualElapsedTime), by=list(df2$OriginState, df2$DestState), FUN=mean)
      
      df3<-df3 %>% rename(
        OriginState = Group.1,
        DestState = Group.2,
      )
      
      dfLoli$Vuelo = paste(paste(dfLoli$OriginState,"-"), dfLoli$DestState)
      df3$Vuelo = paste(paste(df3$OriginState,"-"), df3$DestState) 
      
      dfLoli <- dfLoli %>% left_join(df3, by=c("Vuelo"))
      dfLoli <- dfLoli %>%select(Vuelo, CRSElapsedTime, ActualElapsedTime)
      
      dfLoli<-dfLoli[order(dfLoli$ActualElapsedTime),]
      
      ggplot(dfLoli, aes(y=Vuelo, x=CRSElapsedTime, xend=ActualElapsedTime)) +
        geom_dumbbell(size=3, color="#e3e2e1",
                      colour_x = "#5b8124", colour_xend = "#bad744") +
        labs(x="Media del tiempo transcurrido en minutos",title="Tiempo esperado vs Tiempo real en los vuelos mayor realizados de 2008") +
        theme_minimal() +
        theme(panel.grid.major.x=element_line(size=0.05))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
