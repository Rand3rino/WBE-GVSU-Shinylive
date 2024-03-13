

library(shiny)
library(tidyverse)
library(plotly)

# N1 Counts, weighted average by flow per day
options(scipen = 999)
N1counts <- read.csv("https://docs.google.com/spreadsheets/d/1Y8HZf93GiC_8XjK7nxqZONcTmi4Ck7EH96hR4q6Kbio/gviz/tq?tqx=out:csv;outFileName:data&sheet=N1%20Counts")
N1counts$Date <- as.Date(as.character(N1counts$Date), format = "%y%m%d")
N1 <- N1counts %>% 
        group_by(Date) %>% 
        summarise( N1 = mean(N1.GC.mL * Flow..mL.Day.))


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
VariantProportions <- select(VariantProportions, -Variant)

VariantColors <- read.csv("https://docs.google.com/spreadsheets/d/19sCocxxppFfBrM0AKEojOzWoN6C6myoZjIA492deetc/gviz/tq?tqx=out:csv;outFileName:data&sheet=Sheet2")
VariantColors$Variant <- factor(VariantColors$Variant, levels=unique(VariantColors$Variant))
VariantColors$HexCode <- toupper(VariantColors$HexCode)
VariantColors$Appearance <- 2:(nrow(VariantColors)+1)
VariantProportions <- left_join( VariantProportions, VariantColors, by="Appearance")


# # Define UI for application that draws a line chart
ui <- fluidPage(
  mainPanel(
    
<<<<<<< HEAD
    # Application title
    titlePanel(h1("COVID-19 Wastewater Based Epidemiology", align = "center")),
    
    # Add GVSU CMB Logo https://www.w3schools.com/html/html_images.asp
    # HTML('<center><img src="CMB Logos 001.jpg" alt="GVSU CMB Logo" width="250" height="80"></center>'),
    
    # Plots
    plotOutput("N1Plot"),
    plotlyOutput("VariantPlot")
  )
=======
    mainPanel(
      # plotlyOutput("N1Plot"),
      plotOutput("N1Plot"),
      plotlyOutput("VariantPlot")
      
      # plotOutput("VariantPlot")
    )
>>>>>>> parent of 7ee091c (Image Code)
)

# # Define server logic required to draw a line chart
server <- function(input, output) {
  
  output$N1Plot <- renderPlotly({
    ggplotly((ggplot(N1, aes(x=Date, y=N1))
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
    p<-ggplotly((ggplot(VariantProportions, aes(x=Week, y=Proportion))
      + scale_fill_identity(name = "Variants", labels=levels(VariantProportions$Variant), guide="legend")
      + geom_col(aes(fill=HexCode, group=Variant))
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