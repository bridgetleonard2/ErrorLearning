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
library(tidyr)
library(readxl)
library(PairedData)
library(hash)
library(rlang)
library(plotrix)

library(lme4)      # Mixed models
library(lmerTest)  # Summary and p-values for mixed models
library(sjPlot)   # APA-style tables for mixed models
library(broom.mixed)
library(ggsci)
library(reticulate)
# use_python("/Users/bridget/anaconda3/bin/python3")
# Switch python for windows
use_python("C:\\Users\\Bridget Leonard\\AppData\\Local\\Programs\\Python\\Python312")

# Graphics
library(ggplot2)
library(plotly)
library(gridExtra)     
library(knitr)
library(imager)

# Define server logic required to draw a histogram
function(input, output) {
  cleandata <- read.csv("../../behavioral_data/clean_data.csv")
  # Mixed Linear Model
  acc_model <- glmer(correct ~ condition # Fixed effect of condition
                     + (1|participant),  # Interface for participant
                     family=binomial, 
                     cleandata)
  
  cleandata <- cleandata %>% mutate(pred_correct = fitted(acc_model))
  
  cleandata %>% pivot_longer(cols=c("correct", "pred_correct"),
                             values_to = "accuracy",
                             names_to = "type") -> cleandata_long
  
  cleandata_agg_pred <- cleandata_long %>% 
    mutate(type = replace(type, type == "correct",  "Observed")) %>% 
    mutate(type = replace(type, type == "pred_correct", "Predicted")) %>%
    group_by(participant, condition, type) %>%
    summarise(Accuracy = mean(accuracy))
  
  cleandata_summary_pred <- cleandata_agg_pred %>% 
    group_by(condition, type) %>%
    summarise(Accuracy = mean(Accuracy))
  
  cleandata_agg_pred <- cleandata_agg_pred %>% 
    mutate(cond = case_when(condition == "study" ~ 1, condition == "error" ~ 2)) %>% 
    mutate(cond = jitter(cond))
  
  cleandata_summary_pred <- cleandata_summary_pred %>% 
    mutate(cond = case_when(condition == "study" ~ 1, condition == "error" ~ 2))
  
  Ose_data <- cleandata_agg_pred %>% 
    filter(type == "Observed") %>% 
    group_by(condition) %>% 
    summarize(
      accuracy_se = std.error(Accuracy)
    )
  Pse_data <- cleandata_agg_pred %>% 
    filter(type == "Predicted") %>% 
    group_by(condition) %>% 
    summarize(
      accuracy_se = std.error(Accuracy)
    )
  
  Ose_data <- Ose_data %>% mutate(type = 'Observed')
  Pse_data <- Pse_data %>% mutate(type = 'Predicted')
  se_data <- full_join(Ose_data, Pse_data)
  
  cleandata_summary_pred <- cleandata_summary_pred %>% full_join(se_data)
  
  output$plotlyOutput <- renderUI({
    plotlyFigure <- plot_ly(data = cleandata_agg_pred %>% filter(type=="Observed") %>% group_by(participant), x = ~cond, y = ~Accuracy, color = ~condition) %>%
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
    plotly_div <- plotly:::plot_ly_browsable(plotlyFigure)
    
    # Convert the plotly_div to HTML content
    plotlyHtml <- as.character(tags$div(HTML(plotly_div)))
    
    # Include the dynamic Plotly content in the HTML file
    includeHTML(sprintf("includeDynamicPlotlyContent('%s')", plotlyHtml))
  })
  
}
