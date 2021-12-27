#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

df1 <- read.csv('vuelosLimpio.csv', header=TRUE, sep=",")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Delayed Flights"),

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
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
        filtro = (is.na(df1$TotalDelay)==FALSE) & (df1$TotalDelay >= -7) & (df1$TotalDelay<=10)
        
        x <- df1$TotalDelay[filtro]
        
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
