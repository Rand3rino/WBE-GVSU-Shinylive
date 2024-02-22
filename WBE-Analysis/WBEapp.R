







## https://debruine.github.io/shinyintro/data.html
# update.packages(ask = F) # https://stackoverflow.com/questions/68451929/shiny-apps-not-running-hanging-not-working
# library(shiny)
# library(shinylive)
# library(tidyverse)
# library(plotly)
# library(googlesheets4)
# gs4_deauth()
# sheet_id <- "https://docs.google.com/spreadsheets/d/1Y8HZf93GiC_8XjK7nxqZONcTmi4Ck7EH96hR4q6Kbio"
# N1counts <- read_sheet(sheet_id)
# N1counts$Date <- as.Date(as.character(N1counts$Date), "%y%m%d")
# 
# # Define UI for application that draws a line chart
ui <- fluidPage(

    # Application title
    titlePanel("WBE-Analysis"),

    # Sidebar with a dropdown for number site
    sidebarLayout(
        sidebarPanel(
          selectInput("SiteSelect",
                        "Select Site to view",
                        choices = c("CS", "GG", "GO", "GR", "WB", "WK", "WY"))
        ),

        # Show a plot of the n1 counts
        mainPanel(
           plotOutput("N1Plot")
        )
    )
)
# 
# # Define server logic required to draw a line chart
 server <- function(input, output) {
 
     output$N1Plot <- renderPlot({
         N1Site <- N1counts %>% filter(Site == input$SiteSelect)
         ggplot(N1Site, aes(x=Date, y=`N1 GC/100mL`)) + geom_line() + geom_point() + theme_bw()
     })
 }

 # Run the application 
 shinyApp(ui = ui, server = server)
