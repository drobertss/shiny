library(shiny)
library(ggplot2)



shinyUI(fluidPage(
  
  # Application title
  titlePanel("Stat 515 Final Project"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("graphType",
                  label="Choose a graph to display",
                  choices = c("Histogram" = "histogram",
                              "Rentals by Day of the Week" = "weekday",
                              "Box and Whisker Plot: User vs. Duration" = "boxWhisker",
                              "Scatterplot: Age vs. Duration" = "ageVsDuration"
                              
                  ),
                  selected="histogram"),
      checkboxInput("color", "Add color to the graph?", value=FALSE),
    
      conditionalPanel(
        condition = "input.graphType == 'histogram'",
        checkboxInput("zip","View Zip Code Histogram?", value = FALSE),
       checkboxInput("hist", "View Histogram of Casual vs. Registered Riders?", value = FALSE)

      ),
      conditionalPanel(
        condition = "input.graphType == 'weekday'",
        checkboxInput("percent", "View as percentage?", value=FALSE)


      ),
      conditionalPanel(
        condition = "input.graphType == 'ageVsDuration'",
        sliderInput("age",
                    "Select Age to View:",
                    min=0,
                    max=100,
                    value =c(0,100)),
        sliderInput("time",
                    "Select a max amount of time (in hours)",
                    min=0,
                    max=24,
                    value=12),
        checkboxInput("smooth", "Add Regression Line?", value =FALSE),
        checkboxInput("reverse","Reverse the X and Y Coordinates?", value = FALSE)
      ), 
      
      conditionalPanel(
        condition = "input.graphType == 'boxWhisker'",
        radioButtons("timeUnit", "Select Unit of Time",
                     c("Seconds" = "second",
                       "Minutes" = "minute",
                       "Hours" = "hours"),
                     selected ="second"),
        checkboxInput("outliers","Remove Outliers?", value = FALSE)
      )

    ),
    
    mainPanel(
      plotOutput('plot')
    )
  
)
))

