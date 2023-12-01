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
  includeHTML("www/index.html"),
  navbarPage(
    "Your Shiny App",
    tabPanel("Tab 1",
             fluidRow(
               # Add your Shiny widgets and components here
               # ...
             )
    ),
    tabPanel("Tab 2",
             fluidRow(
               # Add more Shiny widgets and components here
               # ...
             )
    )
  )
)