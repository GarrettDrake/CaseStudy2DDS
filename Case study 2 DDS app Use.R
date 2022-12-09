#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
library(ggplot2)
library(tidyverse)

Case2 <- read.csv("Case2.csv")

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Explatory analysis of attrition by variables"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Copy the line below to make a set of radio buttons
      radioButtons("radio", label = h3("Variables"),
                   choices = list("Overtime" = 1, "Gender" = 2, "Department" = 3, "Age" = 4, "Monthly Income" = 5), 
                   selected = 1),
      
      hr(),
      fluidRow(column(2, verbatimTextOutput("value")))
      
    ),
    
    # Main panel for displaying outputs ----
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
    
  )
)


# Define server logic required to draw a histogram

server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    
    
    
    if(input$radio == 1)
    {
      
      a <- ggplot(Case2, aes(x = OverTime, fill = Attrition)) +
        geom_bar()+
        ggtitle("Attrition by Over Time") + # USE talk about overtime, much higher percentage
        xlab("Over Time")
      print(a)
    }
    
    if(input$radio == 2)
    {
      
      b <- ggplot(Case2, aes(x = Gender, fill = Attrition)) + geom_bar()+
        ggtitle("Attrition by Gender")
      print(b)
      
    }
    
    if(input$radio == 3)
    {
      
      c <- ggplot(Case2, aes(x = Attrition, fill = Department)) + geom_bar() +
        facet_wrap(~Department) +
        ggtitle("Attrition by Department")
      print(c)
    }
    
    if(input$radio ==4)
    {
      d <-ggplot(Case2, aes(x = Age, fill = Attrition)) + geom_histogram()+
        ggtitle("Attrition by Age")
      print(d)
      
         }
    
    if(input$radio ==5)
    {
      e <- ggplot(Case2, aes(x = MonthlyIncome, fill = Attrition)) + geom_histogram() +
        ggtitle("Attrition by Monthly Income") +
        xlab("Monthly Income")
      print(e)
        }
    }
    
    
  
  )
  
}

shinyApp(ui, server)