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
  titlePanel("The Task"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("startStudy", "Start Study Session")
    ),
    
    mainPanel(
      mainPanel(
        div(id = "cueContainer", style = "font-size: 20px; text-align: center;"),
        div(id = "targetContainer", style = "font-size: 20px; text-align: center;")
    )
  )
  ),
  div(style = "margin-top: 20px; margin-bottom: 20px; color: #3498db; font-size: 20px; text-align: center;",
      textOutput("timer")
      ),
  sidebarLayout(
    sidebarPanel(
      actionButton("startTest", "Start Test Session")
    ),
    
    mainPanel(
      mainPanel(
        div(id = "cueTestContainer", style = "font-size: 20px; text-align: center;"),
        div(id = "answerTestContainer", style = "font-size: 20px; text-align: center;")
      )
    )
  ),
  mainPanel(
    mainPanel(
      div(id="accuracySummary"),
      div(id="rtSummary"),
      div(id="plotSummary", class="shiny-plot-output", style="width:800px; height:400px;")
    )
  )
)