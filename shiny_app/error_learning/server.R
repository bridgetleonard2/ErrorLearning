#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Data manipulation and analysis
library(dplyr)


# Define server logic required to draw a histogram
function(input, output) {
  cleandata <- read.csv("../../behavioral_data/fig_data.csv")
 
  output$plotlyOutput <- renderUI({
    p1 <- plot_ly(data = cleandata_agg_pred %>% filter(type=="Observed") %>% group_by(participant), x = ~cond, y = ~Accuracy, color = ~condition) %>%
      add_markers(alpha = 0.75, color = I("#D9D6C7"), split = ~participant, marker = list(size=5),
                  text = ~Accuracy,
                  textposition = "auto",
                  hoverinfo = "text",
                  hovertext = ~paste0("Participant: ", participant,
                                      "<br> Observed Accuracy: ", round(Accuracy*100, 2), "%"),
                  legendgroup="participants", name=~paste("Participant -", participant)) %>% 
      add_lines(color = I("#D9D6C7"), line = list(width = 0.3), split = ~participant,
                legendgroup="participants", name=~paste("Participant -", participant),
                showlegend=FALSE) %>%
      add_markers(data = cleandata_summary_pred %>% filter(type =="Observed") %>%  filter(condition=="error"), x = ~cond, y = ~Accuracy, error_y = list(type= "data", array = ~accuracy_se,color = '#e74c3c', thickness=2), type = "scatter", mode = "markers", marker = list(color = "#e74c3c", size = 12), legendgroup="average",
                  name="Average - Error") %>% 
      add_markers(data = cleandata_summary_pred %>% filter(type =="Observed") %>%  filter(condition=="study"), x = ~cond, y = ~Accuracy, error_y = list(type= "data", array = ~accuracy_se,color = '#2ecc71', thickness=2), type = "scatter", mode = "markers", marker = list(color = "#2ecc71", size = 12), legendgroup="average",
                  name="Average - Study") %>% 
      add_text(data = cleandata_summary_pred %>% filter(condition == "error" & type =="Observed"), x = ~cond, y = ~Accuracy, text = ~paste0(round(Accuracy * 100, 2), "%"), hjust = -0.5, textfont=list(color = c("#000000"), size=18), legendgroup="average",
               name="Average - Error", showlegend=FALSE) %>%
      add_text(data = cleandata_summary_pred %>% filter(condition == "study" & type =="Observed"), x = ~cond, y = ~Accuracy, text = ~paste0(round(Accuracy * 100, 2), "%"), hjust = -0.2, textfont=list(color = c("#000000"), size=18), legendgroup="average",
               name="Average - Study", showlegend=FALSE) %>%
      style(textposition = "top right") %>%
      layout(
        title = list(text= "Final Test Performance", size = 18),
        xaxis = list(title = "Condition", autotypenumbers = "strict", range = c(0.5, 2.5), ticktext = list("Study Items", "Error Items"), 
                     tickvals = list(1, 2),
                     tickmode = "array"),
        yaxis = list(title = "Accuracy"),
        legend = list(traceorder='reversed'),
        showlegend = FALSE,
        hovermode = "closest"
      ) 
      # Add JavaScript code to adjust text size dynamically
      p1$dependencies <- list(
        "d3.js",
        htmltools::htmlDependency(
          "custom-plotly-scripts",
          "1.0",
          src = normalizePath(system.file("custom_scripts", package = "shiny")),
          script = "resize_text.js"
        )
      )
      
      p1
    })
  
}
