#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

fluidPage(
  shinyjs::useShinyjs(),  # Initialize shinyjs
  includeHTML("www/index.html"),
  titlePanel("Word Pair Study Phase"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("startStudy", "Start Study Session")
    ),
    
    mainPanel(
      div(id = "wordPairContainer", style = "font-size: 20px; text-align: center;"),
      br(),
      textOutput("timer")
    )
  )
)
