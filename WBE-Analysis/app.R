

library(shiny)
library(tidyverse)
library(plotly)

options(scipen = 999)
N1counts <- read.csv("https://docs.google.com/spreadsheets/d/1Y8HZf93GiC_8XjK7nxqZONcTmi4Ck7EH96hR4q6Kbio/export?format=csv")
N1counts$Date <- as.Date(as.character(N1counts$Date), format = "%y%m%d")

# https://matrixify-excelify.medium.com/download-specific-google-sheets-tab-as-csv-file-e805ecef29fc
# VariantProportions <- read.csv("https://docs.google.com/spreadsheets/d/19sCocxxppFfBrM0AKEojOzWoN6C6myoZjIA492deetc/export?format=csv")
VariantProportions <- read.csv("https://docs.google.com/spreadsheets/d/19sCocxxppFfBrM0AKEojOzWoN6C6myoZjIA492deetc/gviz/tq?tqx=out:csv;outFileName:data&sheet=Sheet1")
colnames(VariantProportions) <- paste(colnames(VariantProportions), match(colnames(VariantProportions), colnames(VariantProportions)), sep="_")
VariantProportions <- VariantProportions %>%
  pivot_longer(!Date.by.Week_1, names_to = "Variant", values_to = "Proportion", values_drop_na = TRUE)
VariantProportions$Week <- as.Date(VariantProportions$Date.by.Week_1, format ="%m/%d/%Y")
VariantProportions <- separate_wider_delim(VariantProportions, Variant, delim = "_", names = c("Variant", "Appearance"))
VariantProportions$Appearance <- as.integer(VariantProportions$Appearance)
VariantProportions <- VariantProportions[order(VariantProportions$Appearance),]
VariantProportions$Variant <- factor(VariantProportions$Variant, levels=unique(VariantProportions$Variant))

VariantColors <- read.csv("https://docs.google.com/spreadsheets/d/19sCocxxppFfBrM0AKEojOzWoN6C6myoZjIA492deetc/gviz/tq?tqx=out:csv;outFileName:data&sheet=Sheet2")
VariantColors$Variant <- factor(VariantColors$Variant, levels=unique(VariantColors$Variant))
VariantColors$HexCode <- toupper(VariantColors$HexCode)
VariantColors$Appearance <- 2:(nrow(VariantColors)+1)
VariantProportions <- left_join( VariantProportions, VariantColors, by="Appearance")


# # Define UI for application that draws a line chart
ui <- fluidPage(
  
  # Application title
  titlePanel("WBE-Analysis"),
  
    
    mainPanel(
      # plotlyOutput("N1Plot"),
      plotOutput("N1Plot"),
      plotlyOutput("VariantPlot")
      
      # plotOutput("VariantPlot")
    )
)

# # Define server logic required to draw a line chart
server <- function(input, output) {
  
  output$N1Plot <- renderPlotly({
    ggplotly((ggplot(N1counts, aes(x=Date, y=N1.GC.100mL))
       + geom_point()
       + geom_smooth()
       + theme_bw()
       + scale_y_log10()
       + ylab("Log (N1 Counts)")
       + ggtitle("N1 Counts over Time (Log Scale)", )
       + theme(plot.title = element_text(hjust = 0.5)))
    )
  })

  output$VariantPlot <- renderPlotly({         
    p<-ggplotly((ggplot(VariantProportions, aes(x=Week, y=Proportion, fill=Variant.y))
      + geom_col(aes(fill=HexCode))
      + scale_fill_identity(name = "Variants", labels=levels(VariantProportions$Variant.y), guide="legend")
      + theme_bw()
      + scale_y_continuous(labels = scales::percent) # Y-Axis as percents
      + xlab("Date")
      + ggtitle("Variants over Time as Proportions")
      + theme(plot.title = element_text(hjust = 0.5))
    )) %>% layout(legend = list(orientation = "h", y =-.4))
    
    for (i in 1:nrow(VariantColors)) {
      p$x$data[[i]]$name <- VariantColors$Variant[VariantColors$HexCode==p$x$data[[i]]$name]
    }
    
    p
  })
  
  output$N1Plot <- renderPlot({
    (ggplot(N1counts, aes(x=Date, y=N1.GC.100mL)) + geom_point()
      + geom_smooth()
      + theme_bw()
      + scale_y_log10()
      + ylab("N1 Counts (Log Scale)")
      + ggtitle("N1 Counts over Time (Log Scale)", )
      + theme(plot.title = element_text(hjust = 0.5)))
  })
  # 
  # output$VariantPlot <- renderPlot({
  #   ggplot(VariantProportions, aes(x=Date.by.Week, y=Proportion)) + geom_col(aes(fill=Variant)) + theme_bw() + theme(legend.position = "bottom")
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)


#shinylive::export("WBE-Analysis", "docs")
#httpuv::runStaticServer("docs")