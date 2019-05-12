#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

accidents <- read.csv("https://raw.githubusercontent.com/wengshian1994/315-Interactive-Project/master/barcelona-data-sets/accidents_2017.csv?token=AI2P3NBRJNC7OMNYFPNTPGS42MGNA")
accidents <- accidents[complete.cases(accidents),]
accidents$Weekday <- as.factor(accidents$Weekday)
accidents$Month <- as.factor(accidents$Month)
accidents$Day_of_Month <- accidents$Day
accidents$Time_of_Day <-accidents$Part.of.the.day



ui <- fluidPage(
   
   # Application title
   titlePanel("Regression Modeling"),
      sidebarPanel( 
        p("Select the input for the Dependent Variable"),
        selectInput(inputId = "DepVar", label = "Dependent Variables",
                    multiple = FALSE,
                    choices =list("Victims", "Vehicles.involved"),
                    selected = "Victims"),
        p("Select the input for the Independent Variable"),
        selectInput(inputId = "IndVar", label = "Independent Variables",
                    multiple = FALSE, choices = list("Weekday", "Month",
                                                     "Vehicles.involved",
                                                     "Day_of_Month", "Time_of_Day"),
                    selected = "Weekday")
         
         
      ),
  
   mainPanel(
     tabsetPanel(type = "tabs", 
                 tabPanel("Distributions",
                          fluidRow(
                   column(6, plotOutput("hist_Dep")),
                   column(6, plotOutput("Ind"))
                   )),
                   tabPanel("Scatterplot", plotOutput("scatterplot"))
                 ),
     verbatimTextOutput(outputId = "RegSum")
     )

)



server <- function(input, output) {
  lm1 <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = accidents)})
  #histograms
  output$hist_Dep <- renderPlot({
    ggplot(accidents, aes(x = accidents[,input$DepVar])) +
      geom_histogram() +labs(x = input$DepVar)
  })
    output$Ind <- renderPlot({
      ggplot()+ aes(x = accidents[,input$IndVar]) + 
        geom_bar() + labs(x = input$IndVar)
    })

  #scatterplot
    output$scatterplot <- renderPlot({
      plot(x = accidents[,input$IndVar], y = accidents[,input$DepVar],
           main = "Scatterplot", xlab = input$IndVar,
           ylab= input$DepVar)
      abline(coef(lm1())[1], coef(lm1())[2], col = "red")
    }, height = 400)
    
    output$RegSum <- renderPrint({summary(lm1())})

}


# Run the application 
shinyApp(ui = ui, server = server)

