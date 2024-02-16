

library(shiny)
library(tidyverse)
# library(plotly)

N1counts <- read.csv("https://docs.google.com/spreadsheets/d/1Y8HZf93GiC_8XjK7nxqZONcTmi4Ck7EH96hR4q6Kbio/export?format=csv")
N1counts$Date <- as.Date(as.character(N1counts$Date), format = "%y%m%d")

VariantProportions <- read.csv("https://docs.google.com/spreadsheets/d/19sCocxxppFfBrM0AKEojOzWoN6C6myoZjIA492deetc/export?format=csv")
VariantProportions$Date.by.Week <- as.Date(VariantProportions$Date.by.Week, format ="%m/%d/%Y")
VariantProportions <- VariantProportions %>%
  pivot_longer(!Date.by.Week, names_to = "Variant", values_to = "Proportion", values_drop_na = TRUE)

# # Define UI for application that draws a line chart
ui <- fluidPage(
  
  # Application title
  titlePanel("WBE-Analysis"),
  
  # Sidebar with a dropdown for number site
  # sidebarLayout(
    # sidebarPanel(
      # selectInput("SiteSelect",
      #             "Select Site to view",
      #             choices = c("CS", "GG", "GO", "GR", "WB", "WK", "WY"))
    # ),
    
    # Show a plot of the n1 counts
    mainPanel(
      # plotlyOutput("N1Plot"),
      # plotlyOutput("VariantPlot")
      plotOutput("N1Plot"),
      plotOutput("VariantPlot")
    )
  # )
)
# 
# # Define server logic required to draw a line chart
server <- function(input, output) {
  
  # output$N1Plot <- renderPlotly({
  #   ggplotly(ggplot(N1counts, aes(x=Date, y=log(N1.GC.100mL))) + geom_point())
  # })
  # 
  # output$VariantPlot <- renderPlotly({
  #   ggplotly(ggplot(VariantProportions, aes(x=Date.by.Week, y=Proportion)) + geom_col(aes(fill=Variant)) + theme_bw())
  # })
  
  output$N1Plot <- renderPlot({
    ggplot(N1counts, aes(x=Date, y=log(N1.GC.100mL))) + geom_point() + geom_smooth() + theme_bw()
  })
  
  output$VariantPlot <- renderPlot({
    ggplot(VariantProportions, aes(x=Date.by.Week, y=Proportion)) + geom_col(aes(fill=Variant)) + theme_bw() + theme(legend.position = "bottom")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


#shinylive::export("WBE-Analysis", "docs")
# httpuv::runStaticServer("docs/", port=8008)