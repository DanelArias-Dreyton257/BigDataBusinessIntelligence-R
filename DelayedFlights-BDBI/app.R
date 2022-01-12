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
library(treemapify)

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
                          img(src="vuelo.jpg",width="50%"),
                          h1("Vuelos y sus retrasos"),
                          p("Por D. Arias y J.A. de la Puebla"),
                          h4("Esquema de la presentacion"),
                          tags$ol(
                            tags$li("Overview de los datos"),
                            tags$li("Análisis por localización"),
                            tags$li("Vuelos sensibles a cancelaciones y retrasos"),
                            tags$li("Visión general de retrasos y cancelaciones"),
                            tags$li("Análisis en periodos de tiempo específicos"),
                            tags$li("Conclusiones"),
                          )    
                 ),
                 tabPanel("Overview",
                          titlePanel("Overview del dataset"),
                          p("Pequeña visualizacion de los datos usados"),
                          verticalLayout(
                            dataTableOutput("overview")
                          )),
                 tabPanel("Análisis por localización",
                          verticalLayout(
                            h1("Vuelos según estado de origen y estado de destino"),
                            plotOutput("heatmap"),
                            h2("Análisis de vuelos mas frecuentes"),
                            sidebarLayout(
                              sidebarPanel(
                                h3("Ranking de vuelos frecuentes"),
                                tableOutput("fTable")
                              ),
                              mainPanel(
                                plotOutput("lolipop")
                              )
                            ),
                            h2("Visualización en mapa de los vuelos", align="center"),
                            plotOutput("mapPlot"),
                            wellPanel(
                              selectInput("state",
                                          "Elige un estado de origen:",
                                          states$ID
                              )
                            ) 
                          )
                 ),
                 tabPanel("Vuelos sensibles",
                          h1("Vuelos sensibles a cancelaciones y retrasos"),
                          verticalLayout(
                            h2("Vuelos con mayor numero de retrasos"),
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput(
                                  inputId = "delayNum",
                                  label = "Número mínimo de vuelos totales:",
                                  min = 1,
                                  max = 30,
                                  value = 10
                                )
                              ),
                              mainPanel(plotOutput("delayTree"))
                            ),
                            h2("Vuelos con mayor numero de cancelaciones"),
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput(
                                  inputId = "cancelNum",
                                  label = "Número mínimo de vuelos totales:",
                                  min = 1,
                                  max = 30,
                                  value = 10
                                )
                              ),
                              mainPanel(plotOutput("cancelTree"))
                            )
                          )
                 ),
                 tabPanel("Visión general de 2008",
                          h1("Profundizando en las cancelaciones y los retrasos de forma general"),
                          verticalLayout(
                            h2("Retrasos en 2008"),
                            plotOutput("weekBarPlot"),
                            h2("Cancelaciones en 2008"),
                            plotOutput("cancelledPlot")
                          )
                 ),
                 tabPanel("Análisis en periodos específicos",
                          h1("Análisis en el periodo definido"),
                          verticalLayout(
                            wellPanel(
                              dateRangeInput("daterange", "Elige el rango de fechas a mostrar:",
                                             language = "es",
                                             weekstart = 1,
                                             start="2008-01-01",
                                             end = "2008-12-31"),
                            ),
                            h2("Cantidad de vuelos"),
                            plotOutput("greenRedPlot"),
                            h2("Retrasos"),
                            plotOutput("delayBoxPlot"),
                            h2("Cancelaciones"),
                            plotOutput("cancellationPlot"),
                          )
                 ),
                 tabPanel("Conclusiones",
                          h1("Conclusiones"),
                          h2("El origen,destino y la cantidad no afectan a que un vuelo tarde más de lo esperado"),
                          p("Los vuelos mas realizados son sobre el propio estado en California y Texas. Son estados grandes con una gran poblacion, ya sea porque el viaje en avion sea asequible o el transporte disponible en ese estado no lo sea"),
                          p("La diferencia entre el tiempo esperado y el tiempo actual -teniendo en cuenta los retrasos- no parece verse afectada por el numero de vuelos. Vemos que la distancia parece afectar muy poco significativamente."),
                          h2("No parece haber vuelos más sensibles que otros a cancelaciones o retrasos"),
                          p("No parece haber demasiado impacto de los vuelos concretos en el porcentaje de retrasos o cancelaciones de los mismos. O lo que es lo mismo, las variables de localización o cantidad de vuelos con mismo origen y destino no influyen significativamente e los retrasos o cancelaciones de los mismos."),
                          h2("En otoño hay menos retrasos y todas las cancelaciones ocurren a finales de año. Es constante la influencia de los retrasos."),
                          p("Se puede ver una menor cantidad de vuelos retrasados en otoño del 2008 en comparación con el resto del año."),
                          p("No parece que el tipo de retraso que influye varie entre semanas del mismo mes. El porcentaje que influyen sobre el retraso total es relativamente constante."),
                          p("Se sufren muy pocas cancelaciones. Todas ellas ocurren en los últimos meses del año."),
                          h2("Más vuelos es igual más retrasos. NO hay casi vuelos retrasados por temporal. Pocas cancelaciones y poca información "),
                          p("Parece que la cantidad de vuelos con la de retrasos es proporcional y no varía con el tiempo. No parece que haya nada fuera de lo usual salvo que hay menor cantidad de vuelos en ese periodo."),
                          p("La mayoría de vuelos no reciben muchos retrasos. Pero hay ciertos vuelos concretos que llegan a recibir retrasos de varias horas."),
                          p("Se ve que la mayoría de retrasos son a causa de una salida tardía que claramente origina una tardía llegada."),
                          p("Con esto también se puede ver que usualmente no hay muchos retrasos por seguridad o por temporal. Este último siendo un dato sorprendente."),
                          p("Al haber muy pocas cancelaciones es muy dificil averiguar que tipo afecta más, pero parece ser relativamente equitativo y no haber un caso mayoritario de cancelación."),
                          h2("Conclusión final"),
                          p("El sistema no parece sufrir de muchos retrasos por localización o por tipo de retraso. Al contrario de lo que pudiera parecer la metereología no es algo que debiera importar al usuario a la hora de elegir vuelo."),
                          p("Tampoco influye el origen y destino. Eso sí, al analizar vemos que a finales de año es cuando se realizan mayor número cancelaciones y teniendo en cuenta que los meses previos hay menor número de vuelos en general."),
                          p("Sería recomensable hacer esos vuelos entre septiembre y octubre que estarían más baratos y no a finales de año donde el mismo vuelo es más propenso a ser cancelado."),
                          p("En cuanto a los aeropuertos, habría que intentar reducir el retraso de salida y agilizar así las salidas/despegues de los vuelos."),
                          p("Los retrasos que estarían en su mano, los de \"National Airport System\" y Carrier se es tendría que tener un ojo a vizor pues son los siguientes en la lista."),
                 )
                 
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
      labs(title = 'Mapa con los trayectos de un origen concreto', x="Longitud representada en coordenada X", y="Latitud representada en coordenada Y",
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
      labs(x = 'Tipos de retrasos', y = 'Minutos',
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
      labs(title = paste('Numero de vuelos cancelados en el periodo:',sum(dfCode$Count)),
           subtitle = "Separados por tipos",
           caption = 'Solo se tienen en cuenta los vuelos cancelados')
    
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
      labs(x = 'Meses de 2008', y = 'Numero de vuelos',
           title = 'Vuelos cancelados y no cancelados de 2008',
           subtitle = 'Cancelaciones separadas por mes',
           caption = 'Si solo hay un tipo de vuelo se muestra una sola barra') +
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
      labs(x = 'Semanas de 2008', y = 'Porcentaje de vuelos', fill = 'Tipo de retraso',
           title = 'Distribución de los tipos de retraso en los vuelos retrasados y el porcentaje influido en cada uno.',
           subtitle = 'Las columnas muestran el porcentaje de vuelos atrasados -con retrasos no esperados- frente al total',
           caption = 'El porcentaje se calcula a través de la influencia en el retraso general') +
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
      labs(x = 'Semanas de 2008', y = 'Cantidad de vuelos', caption = 'Los cantidad puede variar según la muestra cogida',
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
           caption = 'Los vuelos solo se muestran de estado a estado, puede variar el aeropuerto concreto') +
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
  
  output$cancelTree <-renderPlot({
    numMin<-input$cancelNum
    
    dftot<-df1
    
    dftot$Vuelo = paste(paste(dftot$OriginState,"-"), dftot$DestState)
    
    dfmax<-aggregate(x=dftot %>% select(X),by=list(dftot$Vuelo),FUN=length)
    
    dfmax <- dfmax %>% rename(Vuelo = Group.1,
                              Count=X)
    
    dfcan <- dftot[dftot$Cancelled==1,]
    
    dfcan<-aggregate(x=dfcan %>% select(X),by=list(dfcan$Vuelo),FUN=length)
    
    dfcan <- dfcan %>% rename(Vuelo = Group.1,
                              Count=X)
    
    dfcan <- dfcan %>% left_join(dfmax, by=c("Vuelo"))
    
    dfcan<-dfcan[dfcan$'Count.y'>=numMin,]
    
    dfcan$Perc=dfcan$'Count.x'/dfcan$'Count.y'
    
    dfcan<-dfcan[order(-dfcan$Perc),]
    
    ggplot(na.omit(dfcan[1:10,]), aes(area = Perc, fill = Vuelo,
                             label = paste(Vuelo, paste(round(Perc*100,2),"%",sep=""), sep = "\n"))) +
      geom_treemap() +
      geom_treemap_text(colour = "white",
                        place = "centre",
                        size = 15) +
      labs(title = 'Cantidad de vuelos cancelados por vuelo concreto',
           subtitle = 'Si hay pocos vuelos solo se mostrarán los que haya, máximo mostraremos 10') +
      theme(legend.position = "none")
    
  })
  output$delayTree <-renderPlot({
    
    numMin<-input$delayNum
    
    dftot<-df1
    
    dftot$Vuelo = paste(paste(dftot$OriginState,"-"), dftot$DestState)
    
    dfmax<-aggregate(x=dftot %>% select(X),by=list(dftot$Vuelo),FUN=length)
    
    dfmax <- dfmax %>% rename(Vuelo = Group.1,
                              Count=X)
    
    dfret<-dftot[dftot$TotalTimeDifference > 0,]
    
    dfret<-aggregate(x=dfret %>%select(X), by=list(dfret$Vuelo), FUN=length)
    dfret <- dfret %>% rename(Vuelo = Group.1,
                              Count=X)
    
    dfret <- dfret %>% left_join(dfmax, by=c("Vuelo"))
    
    dfret<-dfret[dfret$'Count.y'>=numMin,]
    
    dfret$Perc=dfret$'Count.x'/dfret$'Count.y'
    
    dfret<-dfret[order(-dfret$Perc),]
    
    ggplot(na.omit(dfret[1:10,]), aes(area = Perc, fill = Vuelo,
                             label = paste(Vuelo, paste(round(Perc*100,2),"%",sep=""), sep = "\n"))) +
      geom_treemap() +
      geom_treemap_text(colour = "white",
                        place = "centre",
                        size = 15) +
      labs(title = 'Cantidad de vuelos retrasados por vuelo concreto',
           subtitle = 'Solo se muestran los 10 vuelos con mayores retrasos') +
      theme(legend.position = "none")
  })
  
  output$overview <-renderDataTable(df1)
}

# Run the application 
shinyApp(ui = ui, server = server)

