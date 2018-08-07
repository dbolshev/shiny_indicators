#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tibble)
library(ggplot2)
library(ggrepel)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Indicators"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(
      3, 
      selectInput("procedureType",
                  "Procurement type:",
                  choices = c("belowThreshold", "aboveThresholdUA",
                              "aboveThresholdEU", "negotiation", "negotiation.quick")),
      helpText("Choose procurement method to see statistics"),
      
      hr(),
      
      selectInput(inputId = "risked", label = "Is risked:",
                  choices = c("All checked", "With risk",
                              "Without risk")),
      helpText("Procedures with risk?")
    ),
    
    column(
      9, 
      tabsetPanel(
        tabPanel("Indicators", plotOutput("barPlot"), plotOutput("piePlot")),
        tabPanel("Top Risked Procedures", tableOutput("tab3")),
        tabPanel("Tables", 
                 fluidRow(
                   column(
                     4,
                     tableOutput("tab1")
                   ),
                   column(
                     4,
                     tableOutput("tab2")
                   )
                 ))
      )
      
    )
  )
  
)

# Define server logic required to draw a histogram


server <- function(input, output) {
  
  d_history <- read.csv("C:/Users/Administrator/Downloads/Indicators_stat/druid_5000_test.csv")
  
  output$barPlot <- renderPlot({
    
    cols <- c("DASU_1", "DASU_2_1", "DASU_2_2", "DASU_3_1", "DASU_3_2",
              "DASU_3_3", "DASU_4_1", "DASU_4_2", "DASU_5_1", "DASU_5_2",
              "DASU_6_3", "DASU_8_1", "DASU_8_2", "DASU_10_1", "DASU_10_2",
              "DASU_12", "DASU_13_1", "DASU_13_2", "DASU_13_3", "DASU_15", "RISK_DASU_1", "RISK_DASU_3", "RISK_DASU_4",
              "RISK_DASU_6", "RISK_DASU_7", "RISK_DASU_8", "RISK_DASU_9", "RISK_DASU_9_1", "RISK_DASU_11", "RISK_DASU_11_1",
              "RISK_DASU_11_2", "RISK_DASU_11_3", "RISK_DASU_12", "RISK_DASU_12_1", "RISK_DASU_12_2", "RISK_DASU_12_3",
              "RISK_DASU_23", "RISK_DASU_24", "RISK_DASU_25", "RISK_DASU_26", "RISK_DASU_27", "RISK_DASU_28", "RISK_DASU_28_1")
    
    x <- d_history
    
    if(input$risked == "All checked"){
      x[, cols][x[, cols] >= 0] <- 1
      x[, cols][x[, cols] < 0] <- 0
    }
    
    if(input$risked == "With risk"){
      x[, cols][x[, cols] < 0] <- 0
    }
    
    if(input$risked == "Without risk"){
      x[, cols][x[, cols] == -1] <- -2
      x[, cols][x[, cols] == 1] <- -2
      x[, cols][x[, cols] == 0] <- 1
      x[, cols][x[, cols] == -2] <- 0
    }
    
    x <- x[x$procedureType == input$procedureType, cols]
    bar <- colSums(x)
    bar <- data.frame(bar)
    bar <- rownames_to_column(bar)
    colnames(bar) <- c("names", "values")
    ggplot(data = bar, aes(x = names, y = values)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme_light() +
      geom_text(aes(label=values), vjust=0, hjust=1, color="white", size=2) +
      coord_flip()
  })
  
  output$tab1 <- renderTable({
    t <- d_history
    
    if(input$risked == "All checked"){
      t[, cols][t[, cols] >= 0] <- 1
      t[, cols][t[, cols] < 0] <- 0
    }
    
    if(input$risked == "With risk"){
      t[, cols][t[, cols] < 0] <- 0
    }
    
    if(input$risked == "Without risk"){
      t[, cols][t[, cols] == -1] <- -2
      t[, cols][t[, cols] == 1] <- -2
      t[, cols][t[, cols] == 0] <- 1
      t[, cols][t[, cols] == -2] <- 0
    }
    
    t <- t[t$procedureType == input$procedureType, cols]
    piec <- colSums(t)
    piec <- data.frame(piec)
    piec <- rownames_to_column(piec)
    colnames(piec) <- c("names", "values")
    head(piec, n = nrow(piec))
  })
  
  output$piePlot <- renderPlot({
    
    y <- d_history
    
    if(input$risked == "All checked"){
      y[, cols][y[, cols] >= 0] <- 1
      y[, cols][y[, cols] < 0] <- 0
    }
    
    if(input$risked == "With risk"){
      y[, cols][y[, cols] == -2] <- 0
    }
    
    if(input$risked == "Without risk"){
      y[, cols][y[, cols] == 1] <- -2
      y[, cols][y[, cols] == 0] <- 1
      y[, cols][y[, cols] == -2] <- 0
    }
    
    y <- y[d_history$procedureType == input$procedureType, cols]
    piec <- colSums(y)
    piec <- data.frame(piec)
    piec <- rownames_to_column(piec)
    colnames(piec) <- c("names", "values")
    perc <- round(piec$values/sum(piec$values)*100)
    labels <- paste(cols, perc)
    labels <- paste(labels, "%", sep = "")
    piec <- cbind(piec, labels)
    ggplot(data = piec, aes(x="", y=values, fill=labels, width=1))+
      geom_bar(stat = "identity", position = "fill", width = 1) +
      coord_polar("y", start = 0) 
  })
  
  output$tab2 <- renderTable({
    tab <- data.frame(table(d_history$procedureType))
    colnames(tab) <- c("procedureType", "count")
    head(tab, n = nrow(tab))
  })
  
  output$tab3 <- renderTable({
    y <- d_history
    y[, cols][y[, cols] < 0 ] <- 0
    y$indicators_with_risk <- rowSums(y[, cols])
    y <- y[order(-y$indicators_with_risk), ]
    head(y[, c("tenderId", "indicators_with_risk")], n = 20)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

