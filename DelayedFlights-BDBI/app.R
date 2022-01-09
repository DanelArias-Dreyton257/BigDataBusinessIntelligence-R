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

df1 <- read.csv('vuelosLimpioMap.csv', header=TRUE, sep=",")

df1<- df1[sample(nrow(df1), size=10000), ]

states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
sf::sf_use_s2(FALSE)
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)

world <- ne_countries(scale = "medium", returnclass = "sf")

# Define UI for application that draws a histogram
ui <- navbarPage("Delayed Flights",
                 tabPanel("Histogram",
                          titlePanel("Histogram Panel"),
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("bins",
                                          "Number of bins:",
                                          min = 1,
                                          max = 15,
                                          value = 5)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              plotOutput("distPlot"),
                            )
                          )
                  ),
                 tabPanel("Map",
                          titlePanel("Map Panel"),
                          verticalLayout(
                            plotOutput("mapPlot"),
                            wellPanel(
                              selectInput("state",
                                          "Choose a state:",
                                          states$ID
                              )
                            )
                            
                          )
                  ),
                 tabPanel("DateSlider",
                          titlePanel("Date Slider Panel"),
                          verticalLayout(
                            wellPanel(
                              dateRangeInput("daterange", "Date range:",
                                             language = "es",
                                             weekstart = 1,
                                             start="2008-01-01",
                                             end = "2008-12-31"),
                            ),
                            plotOutput("delayBoxPlot"),
                            plotOutput("cancelCodePlot")
                          )
                  )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
        filtro = (is.na(df1$TotalTimeDifference)==FALSE) & (df1$TotalTimeDifference >= -7) & (df1$TotalTimeDifference<=10)
        
        x <- df1$TotalTimeDifference[filtro]
        
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$mapPlot <- renderPlot({
      
      df2<-na.omit(df1)
      
      stateName <- input$state
      
      df2<-df2[df2$OriginState==stateName & df2$DestState!=stateName,]
      
      ggplot() +
        geom_sf(data = world) +
        geom_sf(data = states, aes(fill=ID)) +
        geom_curve(data = df2 %>% select(OriginState, OriginX, OriginY, DestX, DestY, DestState) %>% unique(), aes(x = OriginX, y = OriginY, xend = DestX, yend = DestY)) +
        geom_point(data = df2 %>% select(DestState, DestX, DestY) %>% unique(), aes(x = DestX, y = DestY), size = 3) +
        geom_point(data = df2 %>% select(OriginState, OriginX, OriginY) %>% unique(), aes(x = OriginX, y = OriginY), size = 3, color = 'red') +
        geom_label(data = df2 %>% select(DestState, DestX, DestY) %>% unique(), nudge_x = 0, nudge_y =1.5,  size=4,aes(x = DestX, y = DestY, label = DestState)) +
        geom_label(data = df2 %>% select(OriginState, OriginX, OriginY) %>% unique(), nudge_x = 0, nudge_y = 1.5, size=4, color = 'red', aes(x = OriginX, y = OriginY, label = OriginState)) +
        annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering,
                               pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in")) +
        annotation_scale(location = 'bl', width_hint = 0.5) +
        coord_sf(xlim = c(-125, -64), ylim = c(24, 50)) +
        labs(title = 'Title',
             caption = 'Caption') +
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
        labs(x = 'Likelihood weather is causing delay', y = 'Percentage of delayed flights',
             title = 'Percentage of delayed flights vs likelihood of weather causing the delay',
             subtitle = "Subtitle",
             caption = 'Source: publicly available data from DoT') +
        theme(plot.caption = element_text(vjust = 7))
    })
    
    output$cancelCodePlot <- renderPlot({
      start = as.Date(input$daterange[1])
      end = as.Date(input$daterange[2])
      
      filtro = df1$Date >= start & df1$Date<=end
      
      ggplot(data=df1[filtro,], aes(x=as.Date(Date), group=CancellationCode, color=CancellationCode)) +
        geom_density() +
        scale_x_date(date_breaks = '1 week', date_labels = '%b %d') +
        labs(x = 'Likelihood weather is causing delay', y = 'Percentage of delayed flights',
             title = 'Percentage of delayed flights vs likelihood of weather causing the delay',
             subtitle = "Subtitle",
             caption = 'Source: publicly available data from DoT') +
        theme(plot.caption = element_text(vjust = 7))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
