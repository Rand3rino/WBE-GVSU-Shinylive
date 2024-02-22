

library(shiny)
library(tidyverse)
library(plotly)
options(scipen = 999)
N1counts <- read.csv("https://docs.google.com/spreadsheets/d/1Y8HZf93GiC_8XjK7nxqZONcTmi4Ck7EH96hR4q6Kbio/export?format=csv")
N1counts$Date <- as.Date(as.character(N1counts$Date), format = "%y%m%d")

VariantProportions <- read.csv("https://docs.google.com/spreadsheets/d/19sCocxxppFfBrM0AKEojOzWoN6C6myoZjIA492deetc/export?format=csv")
colnames(VariantProportions) <- paste(colnames(VariantProportions), match(colnames(VariantProportions), colnames(VariantProportions)), sep="_")
VariantProportions <- VariantProportions %>%
  pivot_longer(!Date.by.Week_1, names_to = "Variant", values_to = "Proportion", values_drop_na = TRUE)
VariantProportions$Week <- as.Date(VariantProportions$Date.by.Week_1, format ="%m/%d/%Y")
VariantProportions <- separate_wider_delim(VariantProportions, Variant, delim = "_", names = c("Variant", "Appearance"))
VariantProportions$Appearance <- as.integer(VariantProportions$Appearance)
VariantProportions <- VariantProportions[order(VariantProportions$Appearance),]
VariantProportions$Variant <- factor(VariantProportions$Variant, levels=unique(VariantProportions$Variant))

# # Define UI for application that draws a line chart
ui <- fluidPage(
  
  # Application title
  titlePanel("WBE-Analysis"),
  
    
    mainPanel(
      plotlyOutput("N1Plot"),
      plotlyOutput("VariantPlot")
      # plotOutput("N1Plot"),
      # plotOutput("VariantPlot")
    )
)

# # Define server logic required to draw a line chart
server <- function(input, output) {
  
  output$N1Plot <- renderPlotly({
    ggplotly(ggplot(N1counts, aes(x=Date, y=N1.GC.100mL)) 
       + geom_point() 
       + geom_smooth()
       + theme_bw()
       + scale_y_log10()
       + ylab("Log (N1 Counts)")
       + ggtitle("N1 Counts over Time (Log Scale)", )
       + theme(plot.title = element_text(hjust = 0.5))
    )
  })

  output$VariantPlot <- renderPlotly({         
  ggplotly(ggplot(VariantProportions, aes(x=Week, y=Proportion))
    + geom_col(aes(fill=Variant, color=Variant))
    + theme_bw()
    + scale_y_continuous(labels = scales::percent) # Y-Axis as percents
    + xlab("Date")
    + ggtitle("Variants over Time as Proportions")
    + theme(plot.title = element_text(hjust = 0.5))
  )
  })
  
  # output$N1Plot <- renderPlot({
  #   ggplot(N1counts, aes(x=Date, y=log(N1.GC.100mL))) + geom_point() + geom_smooth() + theme_bw()
  # })
  # 
  # output$VariantPlot <- renderPlot({
  #   ggplot(VariantProportions, aes(x=Date.by.Week, y=Proportion)) + geom_col(aes(fill=Variant)) + theme_bw() + theme(legend.position = "bottom")
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)


#shinylive::export("WBE-Analysis", "docs")
#httpuv::runStaticServer("docs")