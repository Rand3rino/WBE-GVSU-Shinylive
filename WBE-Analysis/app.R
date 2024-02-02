

library(shiny)
library(tidyverse)

#webr::install("googlesheets4")
#require(googlesheets4)
#library(googlesheets4)
#gs4_deauth()
#sheet_id <- "https://docs.google.com/spreadsheets/d/1Y8HZf93GiC_8XjK7nxqZONcTmi4Ck7EH96hR4q6Kbio"
# N1counts <- read_sheet(sheet_id)
# N1counts$Date <- as.Date(as.character(N1counts$Date), "%y%m%d")


# https://lokraj.me/post/download-github-data/
#library(readr)
# github_url <- "https://raw.githubusercontent.com/Rand3rino/WBE-GVSU-Shinylive/main/WBE-Analysis/N%20Counts.csv"
#N1counts <- read_csv(url(github_url))
#N1counts$Date <- as.Date(as.character(N1counts$Date), "%y%m%d")

# https://stackoverflow.com/questions/14441729/read-a-csv-from-github-into-r
#library(RCurl)
#x <- getURL(github_url)
#y <- read.csv(text = x)

# https://rpubs.com/kylewbrown/github-csv-r
# N1counts <- read.csv(github_url)
N1counts <- read.csv("https://docs.google.com/spreadsheets/d/1Y8HZf93GiC_8XjK7nxqZONcTmi4Ck7EH96hR4q6Kbio/export?format=csv")
N1counts$Date <- as.Date(as.character(N1counts$Date), format = "%y%m%d")

VariantProportions <- read.csv("https://docs.google.com/spreadsheets/d/19sCocxxppFfBrM0AKEojOzWoN6C6myoZjIA492deetc/export?format=csv")
VariantProportions$Date <- as.Date(VariantProportions$Date, format ="%m/%d/%Y")
VariantProportions <- VariantProportions %>% 
  pivot_longer(!Date, names_to = "Variant", values_to = "Proportion", values_drop_na = TRUE)

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
      plotOutput("N1Plot"),
      plotOutput("VariantPlot")
    )
  # )
)
# 
# # Define server logic required to draw a line chart
server <- function(input, output) {
  
  output$N1Plot <- renderPlot({
    N1Site <- N1counts #%>% filter(Site == input$SiteSelect)
    ggplot(N1Site, aes(x=Date, y=log(`N1.GC.100mL`))) + geom_point() + geom_smooth() + theme_bw() #+ geom_line() 
  })
  
  output$VariantPlot <- renderPlot({
    Variants <- VariantProportions #%>% filter(Site == input$SiteSelect)
    ggplot(Variants, aes(x=Date, y=Proportion)) + geom_col(aes(fill=Variant)) + theme_bw() + theme(legend.position = "bottom")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

