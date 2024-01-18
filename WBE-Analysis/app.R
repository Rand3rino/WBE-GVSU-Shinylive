#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


## https://debruine.github.io/shinyintro/data.html
library(shiny)
library(tidyverse)
library(plotly)
library(googlesheets4)
gs4_deauth()
sheet_id <- "https://docs.google.com/spreadsheets/d/1Y8HZf93GiC_8XjK7nxqZONcTmi4Ck7EH96hR4q6Kbio"
N1counts <- read_sheet(sheet_id)
N1counts$Date <- as.Date(as.character(N1counts$Date), "%y%m%d")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("WBE-Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("SiteSelect",
                        "Select Site to view",
                        choices = c("CS", "GG", "GO", "GR", "WB", "WK", "WY"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("N1Plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$N1Plot <- renderPlot({
        N1Site <- N1counts %>% filter(Site == input$SiteSelect)
        ggplot(N1Site, aes(x=Date, y=`N1 GC/100mL`)) + geom_line() + geom_point() + theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
