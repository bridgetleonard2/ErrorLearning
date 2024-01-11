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
library(stringdist)
library(ggplot2)
library(gridExtra)
library(htmlwidgets)
library(reticulate)


# Define server logic required to draw a histogram
# Define server logic required to draw a histogram
function(input, output, session) {
  # Initialize shinyjs to ensure that shinyjs functions are available
  shinyjs::useShinyjs()
  
  # Define a reactiveValues object to store the plot
  reactiveValues <- reactiveValues(plot = NULL)
  
  word_pairs <- data.frame(
    cue = c("PORTRAY", "PRESCRIPTION", "PARCEL", "CANYON", "LATIN", "STERN", "DRACULA", "ROBIN", "WELL", "INTRODUCE",
            "GLIDE", "ORDER", "COURAGEOUS", "HONEYMOON", "LAUNDRY", "MEASUREMENT", "DANCER", "FUGITIVE", "CHIMNEY", "EYES",
            "CRUNCH", "EMPIRE", "SAIL", "TEA", "SIDE", "DIAMETER", "CHUNK", "SAFARI", "ELEVATOR", "SWING",
            "TASTY", "NITROGEN", "MODEL", "FILTHY", "CRITIC", "HUT", "FREEWAY", "ASSISTANCE", "SALES", "ANCESTOR",
            "BISCUIT", "LIE", "BLOCKADE", "HOTEL", "USURP", "NOODLES", "EVICT", "COOKOUT", "UNCOMMON", "NATURE",
            "THEOREM", "VISIT", "ANNUAL", "SINK", "STAR", "GRADUATE", "SOUTH", "VALLEY", "BATTERY", "ENERGY"),
    target = c("SHOW", "PILL", "BOX", "MOUNTAIN", "FRENCH", "BOAT", "SCARY", "RED", "DONE", "PRESENT",
               "HANG", "FORM", "LION", "ROMANCE", "SOAP", "HEIGHT", "MUSIC", "RUN", "BRICK", "COLOR",
               "NOISE", "STATE", "OCEAN", "LEAVES", "ORDER", "LENGTH", "BLOCK", "DESERT", "BUILDING", "TREE",
               "TREAT", "CHEMISTRY", "CAR", "DIRT", "ANALYZE", "ISLAND", "ROAD", "HELPER", "CLOTHES", "FAMILY", 
               "COOKIE", "STEAL", "WALL", "BED", "TAKE", "CHICKEN", "APARTMENT", "HAMBURGER", "UNIQUE", "FOREST",
               "GEOMETRY", "LEAVE", "PICNIC", "HOLE", "NIGHT", "CAP", "WEST", "LOW", "RADIO", "SUN"),
    condition = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2))
  
  # Shuffle the word pairs
  word_pairs <- word_pairs[sample(nrow(word_pairs)), ]
  word_pairs <- head(word_pairs, 10)
  
  responses <- reactiveValues(values = list())
  answers <- reactiveValues(values = list())
  
  responses_data <- reactiveVal(data.frame(cue = character(0), target = character(0), condition = integer(0), study_response = character(0), study_rt = integer(0)))
  answers_data <- reactiveVal(data.frame(cue = character(0), target = character(0), condition = integer(0), test_response = character(0), test_rt = integer(0)))
  
  observeEvent(input$startStudy, {
    for (i in seq_len(nrow(word_pairs))) {
      cue <- word_pairs$cue[i]
      target <- word_pairs$target[i]
      condition <- word_pairs$condition[i]
      shinyjs::runjs(sprintf("shinyjs.updateWordPair('%s', '%s', %d)", cue, target, condition))
      
      start_time <- Sys.time()
      
      while (difftime(Sys.time(), start_time, units = "secs") < 10){}
    }
    
    print('Experiment Complete')
    
    # Call the JavaScript function to send responses to Shiny
    shinyjs::runjs("shinyjs.sendResponsesToShiny();")
    # Start the process
    # run_pair()  # This will start with the first pair
  })

  observeEvent(input$responsesObject, {
    # Do something with the received responsesObject
    responses$values <- input$responsesObject
    
    # Get current value of responses_data
    responses_current <- responses_data()
    n <- length(names(input$responsesObject))
    responses_current <- responses_current[1:n,]
    # Example: Extracting cues and responses
    responses_current$cue <- names(input$responsesObject)
    responses_current$target <- sapply(input$responsesObject, function(x) x[1])
    responses_current$condition <- sapply(input$responsesObject, function(x) x[2])
    responses_current$study_response <- sapply(input$responsesObject, function(x) x[3])
    responses_current$study_rt <- sapply(input$responsesObject, function(x) x[4])
    rownames(responses_current) <- NULL
    
    print(responses_current)
    responses_data(responses_current)
    # Clear last word pair and start timer
    shinyjs::runjs(sprintf("shinyjs.updateWordPair('%s', '%s', %d)", "", "", 2))
    shinyjs::runjs("startTimer(300);")
    # You can process or analyze the responses here
  })
  
  # Now test session
  current_index <- reactiveVal(1)
  
  #Event to start the test
  observeEvent(input$startTest, {
    
    current_index(1)  # Initialize to the first word pair
    if (nrow(word_pairs) > 0) {
      cue <- word_pairs$cue[1]
      target <- word_pairs$target[1]
      condition <- word_pairs$condition[1]
      shinyjs::runjs(sprintf("shinyjs.updateTest('%s', '%s', %d)", cue, target, condition))
    }
  })
  
  # Event to listen for the Enter key press
  observeEvent(input$enterKey, {
    index <- current_index()
    
    if (index < nrow(word_pairs)) {
      current_index(index + 1)  # Increment the index first
      cue <- word_pairs$cue[current_index()]
      target <- word_pairs$target[current_index()]
      condition <- word_pairs$condition[current_index()]
      shinyjs::runjs(sprintf("shinyjs.updateTest('%s', '%s', %d)", cue, target, condition))
    } else {
      # Call the JavaScript function to send responses to Shiny
      shinyjs::runjs("shinyjs.sendAnswersToShiny();")
      # Clear the text box
      shinyjs::runjs('$("#cueTestContainer").empty()')
      shinyjs::runjs('$("#answerTestContainer").empty()')
    }
  })
  
  
  observeEvent(input$answerObject, {
    # Do something with the received responsesObject
    answers$values <- input$answerObject
    
    # Get current value of answers_data
    answers_current <- answers_data()
    
    n <- length(names(input$answerObject))
    answers_current <- answers_current[1:n,]
    
    answers_current$cue <- names(input$answerObject)
    answers_current$target <- sapply(input$answerObject, function(x) x[1])
    answers_current$condition <- sapply(input$answerObject, function(x) x[2])
    answers_current$test_response <- sapply(input$answerObject, function(x) x[3])
    answers_current$test_rt <- sapply(input$answerObject, function(x) x[4])
    rownames(answers_current) <- NULL
    
    print(answers_current)
    
    responses_current <- responses_data()
    
    ### DATA ANALYSIS STARTS HERE
    full_data <- read.csv('www/formatted_data.csv')
    ppt_code <- (max(full_data$participant, na.rm = TRUE) + 1)
    
    # You can process or analyze the responses here
    combined_data <- merge(responses_current, answers_current, by = c("cue", "target", "condition"), all = TRUE)
    # Perform your analysis here...
    print(combined_data)
    print("Begin Cleaning")
    # Start cleaning data:
    # add index column for easy cleaning (identify "bad" indices):
    combined_data <- combined_data %>% mutate(index = row_number())
    
    # 1) A guess becomes NA if: a) it is repeated for >3 items, b) it has >3 characters c) it is the same as the cue
    
    na_response_a <- combined_data %>% 
      filter(condition == 1) %>% 
      group_by(study_response) %>% 
      add_count() %>%
      filter(n > 3) %>% 
      pull(index)
    
    na_response_b <- combined_data %>%
      filter(condition == 1) %>% 
      filter(nchar(study_response) < 3) %>% 
      pull(index)
    
    na_response_c <- combined_data %>%
      filter(condition == 1) %>% 
      filter(stringdist(tolower(study_response), tolower(cue), method = "lv") < 3) %>% 
      pull(index)
    
    
    if (length(na_response_a) != 0 | length(na_response_b) != 0 | length(na_response_c) != 0) {
      remove <- c(na_response_a, na_response_b, na_response_c)
      filtered_data <- combined_data %>%
        filter(!index %in% remove)
    } else{
      filtered_data <- combined_data 
    }
      
    # 2) Miss <10 guesses -- includes misses that were calculated as NA in step 1
    ## - This is where a participant may be excluded and prompted to try again
    if (nrow(filtered_data) < (nrow(word_pairs) - 10)) {
      print("Not enough data due to...")
    } else {
      # 3) Remove items with correct guess
    ## - use stringdist to check typos
      correct_guess <- filtered_data %>% 
        filter(condition == 1) %>% 
        filter(stringdist(tolower(study_response), tolower(target), method = "lv") < 3) %>% 
        pull(index)
      
      clean_data <- filtered_data %>% 
        filter(!index %in% correct_guess) %>% 
        # lastly we want to filter out time outs and short responses
        filter(test_rt > 200, test_rt < 15000)
      
      # Get results
      # print("Participant XXXX's Results")
      # Calculate accuracy: output: "you performed x% better on ___ items than ___"
      ## use stringdist to check typos and case
      clean_data <- clean_data %>% 
        mutate(correct = case_when(
          stringdist(tolower(test_response), tolower(target), method = "lv") < 3 ~ 1,
          TRUE ~ 0  # Default case if the above condition is not met
        ))
      
      print(clean_data)
      str(clean_data)
      clean_data$target <- unlist(clean_data$target)
      clean_data$condition <- as.double(unlist(clean_data$condition))
      # replace null with na
      clean_data$study_response <- lapply(clean_data$study_response, function(x) if (is.null(x)) NA else x)
      clean_data$study_response <- unlist(clean_data$study_response)
      clean_data$study_rt <- lapply(clean_data$study_rt, function(x) if (is.null(x)) NA else x)
      clean_data$study_rt <- unlist(clean_data$study_rt)
      clean_data$test_response <- unlist(clean_data$test_response)
      clean_data$test_rt <- (unlist(clean_data$test_rt))
      
      str(clean_data)
      
      accuracy <- clean_data %>% 
        group_by(condition) %>% 
        summarize(
          accuracy = sum(correct) / n()
        )
      
      error_acc <- accuracy %>% filter(condition == 1) %>% pull(accuracy)
      study_acc <- accuracy %>% filter(condition == 2) %>% pull(accuracy)
      if (error_acc > study_acc) {
        accuracySummary <- paste0("You performed ", round((error_acc - study_acc)*100, digits=2), "% better on error items than study items")
        # print(paste0("You performed ", round((error_acc - study_acc)*100, digits=2), "% better on error items than study items"))
      } else if (study_acc > error_acc) {
        accuracySummary <- paste0("You performed ", round((study_acc - error_acc)*100, digits=2), "% better on study items than error items")
        # print(paste0("You performed ", round((study_acc - error_acc)*100, digits=2), "% better on study items than error items"))
      } else if (study_acc == error_acc) {
        accuracySummary <- "You did just as well on error items as study items"
        # print("You did just as well on error items as study items")
      }
      print(paste("Error accuracy:", round(error_acc*100, digits=2)))
      print(paste("Study accuracy:", round(study_acc*100, digits=2)))
    }
    
    
    # 4) filter for only correct & remove items with rt < 200 and rt > 15000
    # Analyze RT: output: "you responded x% faster on ___ items compared to ___"
    clean_data_rt <- clean_data %>% 
      filter(correct == 1) %>% 
      group_by(condition) %>% 
      summarize(
        avg_rt = mean(as.numeric(test_rt))
      )
    
    error_rt <- clean_data_rt %>% filter(condition == 1) %>% pull(avg_rt)
    study_rt <- clean_data_rt %>% filter(condition == 2) %>% pull(avg_rt)
    
    if (error_rt > study_rt) {
      rtSummary <- paste0("You responded ", (round(((error_rt - study_rt)/error_rt)*100, digits=2)), "% faster on study items than error items")
      # print(paste0("You responded ", (round(((error_rt - study_rt)/error_rt)*100, digits=2)), "% faster on study items than error items"))
    } else if (study_rt > error_rt) {
      rtSummary <- paste0("You responded ", (round(((study_rt - error_rt)/study_rt)*100, digits=2)), "% faster on error items than study items")
      # print(paste0("You responded ", (round(((study_rt - error_rt)/study_rt)*100, digits=2)), "% faster on error items than study items"))
    } else if (study_rt == error_rt) {
      rtSummary <- "You responded just as fast on error items as study items"
      # print("You responded just as fast on error items as study items")
    }
    
    print(paste("Error response time:", error_rt))
    print(paste("Study response time:", study_rt))
    
    ## Show Results
    output$participantID <- renderText({paste0("Hi participant ", ppt_code, "! Let's review your results. To see how you
                                               compare to the rest of our sample, refresh the page and view the interactive figures again.
                                               Look for your specific code: ", ppt_code, " to see your own results!")})
    output$accuracySummary <- renderText({accuracySummary})
    output$rtSummary <- renderText({rtSummary})
    
    output$plotSummary <- renderPlot({
      accuracy$condition <- as.factor(unlist(accuracy$condition))
      clean_data_rt$condition <- as.factor(unlist(clean_data_rt$condition))
      
      print(head(accuracy))
      print(head(clean_data_rt))
      p1 <- ggplot(accuracy, aes(x = as.factor(condition), y = accuracy, fill = as.factor(condition))) +
        geom_bar(stat = "identity", width = 0.6) +
        scale_fill_manual(values = c("#e74c3c", "#2ecc71")) +
        scale_x_discrete(labels = c("Error Items", "Study Items")) +
        xlab("Condition") +  # Label for x-axis
        ylab("Accuracy") +   # Label for y-axis
        theme_minimal() +
        theme(legend.position = "none") +  # Remove legend
        ggtitle("Accuracy by Condition")
      
      
      # Adjustments for the second plot
      p2 <- ggplot(clean_data_rt, aes(x = as.factor(condition), y = avg_rt, fill = as.factor(condition))) +
        geom_bar(stat = "identity", width = 0.6) +
        scale_fill_manual(values = c("#e74c3c", "#2ecc71")) +
        scale_x_discrete(labels = c("Error Items", "Study Items")) +
        xlab("Condition") +  # Label for x-axis
        ylab("Response Time") +   # Label for y-axis (assuming you want to label it as 'Response Time')
        theme_minimal() +
        theme(legend.position = "none") +  # Remove legend
        ggtitle("Response Time by Condition")
      
      
      grid.arrange(p1, p2, ncol = 2)
    }, width = 800, height = 400)
    
    # Run MLE to find learner type: output: "you fit the ___ model x% better than the ___ model"
    
    
    ## Final output -- use your participant code above to see how your results compare to other in the
    # interactive figures above!
    
    # Load in data (load in earlier to get participant ID)
    
    # Append new results
    ## remove index column and add participant
    clean_data$index <- NULL
    clean_data$participant <- ppt_code
    
    
    full_data <- full_join(full_data, clean_data)
    print(tail(full_data, 100))
    
    #### FIRST FIGURE
    # RE-summarize data:
    cleandata_participant <- full_data %>% 
      group_by(participant, condition) %>% 
      summarize(
        performance = (sum(correct) / n())
      )
    
    print(cleandata_participant)
    
    cleandata_summary <- cleandata_participant %>% 
      group_by(condition) %>% 
      summarize(
        mean_performance = mean(performance),
        sd_performance = sd(performance),
        n = n(),
        stError = sd_performance / sqrt(n)
      )
    
    # Update figures
    summary_plot <- plot_ly(data = cleandata_participant, x = ~condition, y = ~performance) %>%
      add_markers(alpha = 0.75, color = I("#D9D6C7"), split = ~participant, marker = list(size=5),
                  text = ~performance,
                  textposition = "auto",
                  hoverinfo = "text",
                  hovertext = ~paste0("Participant: ", participant,
                                      "<br> Observed Accuracy: ", round(performance*100, 2), "%"),
                  showlegend=FALSE) %>% 
      add_lines(color = I("#D9D6C7"), line = list(width = 0.3), split = ~participant,
                showlegend=FALSE) %>%
      add_markers(data = cleandata_summary %>%  filter(condition== 1), x = ~condition, y = ~mean_performance, error_y = list(type= "data", array = ~stError,color = '#e74c3c', thickness=2), type = "scatter", mode = "markers", marker = list(color = "#e74c3c", size = 12), legendgroup="average",
                  name="Average - Error", showlegend=FALSE) %>% 
      add_markers(data = cleandata_summary %>%  filter(condition==2), x = ~condition, y = ~mean_performance, error_y = list(type= "data", array = ~stError,color = '#2ecc71', thickness=2), type = "scatter", mode = "markers", marker = list(color = "#2ecc71", size = 12), legendgroup="average",
                  name="Average - Study", showlegend=FALSE) %>% 
      add_text(data = cleandata_summary %>% filter(condition == 1), x = ~condition, y = ~mean_performance, text = ~paste0(round(mean_performance * 100, 2), "%"), textposition = "bottom right", textfont=list(color = c("#000000"), size = 17), legendgroup="average",
               name="Average - Error", showlegend=FALSE) %>%
      add_text(data = cleandata_summary %>% filter(condition == 2), x = ~condition, y = ~mean_performance, text = ~paste0(round(mean_performance * 100, 2), "%"), textposition = "bottom right", textfont=list(color = c("#000000"), size = 17), legendgroup="average",
               name="Average - Study", showlegend=FALSE) %>%
      style(textposition = "top right") %>%
      layout(
        title = list(text= "Performance Overview", size = "3vmin"),
        xaxis = list(title = "Condition", autotypenumbers = "strict", range = c(0.5, 2.5), ticktext = list("Error Items", "Study Items"), 
                     tickvals = list(1, 2),
                     tickmode = "array",
                     titlefont = list(size = "2.5vmin")),
        yaxis = list(title = "Accuracy",
                     titlefont = list(size = "2.5vmin")),
        showlegend = FALSE,
        hovermode = "closest"
      )
  })
}
