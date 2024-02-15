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
library(tidyr)
library(dplyr)
library(stringdist)
library(ggplot2)
library(gridExtra)
library(htmlwidgets)
library(reticulate)
library(formattable)
library(DT)
library(htmltools)

library(plotly)
library(googlesheets4)

gs4_auth(path = ".secrets/client_secret.json")

# reticulate::py_install("numpy")
# reticulate::py_install("scipy")
# reticulate::py_install("pandas")
# 
# # Import packages
# numpy <- import("numpy")
# scipy <- import("scipy")
# pandas <- import("pandas")
# math <- import("math")

# Define server logic required to draw a histogram
# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Initialize shinyjs to ensure that shinyjs functions are available
  shinyjs::useShinyjs()
  
  # Define a reactiveValues object to store the plot
  reactiveValues <- reactiveValues(plot1 = NULL, plot2 = NULL, plot3 = NULL, table = NULL)
  
  reactiveData <- reactiveValues(full_data = NULL, LL_data = NULL)
  
  reactiveData$full_data <- read_sheet("1pmeYWRJTQvIRFRzK4PjgB83DypA-XXtea_pj1a13WDo") ###########
  reactiveData$LL_data <- read_sheet("1doyHkRu2NTgrlOA6OoTN8RRXkfZDxIkK1fX72VWxhBU") #############
  
  
  # # Find current plot values
  # # Determine which file to write to and which to delete
  # if (file.exists("www/updatingFigs/summary.html")) {
  #   shinyjs::runjs("updateIframe1('updatingFigs/summary.html')")
  # } else {
  #   shinyjs::runjs("updateIframe1('updatingFigs/summary1.html')")
  # }
  # 
  # if (file.exists("www/updatingFigs/accuracy.html")) {
  #   shinyjs::runjs("updateIframe2('updatingFigs/accuracy.html')")
  # } else {
  #   shinyjs::runjs("updateIframe2('updatingFigs/accuracy1.html')")
  # }
  # 
  # if (file.exists("www/updatingFigs/learner.html")) {
  #   shinyjs::runjs("updateIframe3('updatingFigs/learner.html')")
  # } else {
  #   shinyjs::runjs("updateIframe3('updatingFigs/learner1.html')")
  # }
  # 
  # if (file.exists("www/updatingFigs/dt.html")) {
  #   shinyjs::runjs("updateIframe4('updatingFigs/dt.html')")
  # } else {
  #   shinyjs::runjs("updateIframe4('updatingFigs/dt1.html')")
  # }
  
  # # Define function to find the most recent version of a file
  # find_recent_version <- function(base_path) {
  #   max_version <- 0
  #   for (i in 0:1000) {  # Assume max 1000 versions
  #     if (i == 0) {
  #       file_path <- paste0("www/", base_path, ".html")
  #     } else {
  #       file_path <- paste0("www/", base_path, i, ".html")
  #     }
  #     if (file.exists(file_path)) {
  #       max_version <- i
  #     } else {
  #       break
  #     }
  #   }
  #   return(max_version)
  # }
  # 
  # # Function to update iframe based on file name
  # update_iframe <- function(iframe_id, file_path_base) {
  #   most_recent_version <- find_recent_version(file_path_base)
  #   if (most_recent_version == 0) {
  #     file_path <- paste0(file_path_base, ".html")
  #   } else {
  #     file_path <- paste0(file_path_base, most_recent_version, ".html")
  #   }
  #   shinyjs::runjs(paste0("updateIframe", iframe_id, "('", file_path, "')"))
  # }
  # 
  # # Define function to save plot and update iframe
  # save_and_update_iframe <- function(reactive_value, file_path_base) {
  #   most_recent_version <- find_recent_version(file_path_base)
  #   new_version <- most_recent_version + 1
  #   current_file_path <- paste0(file_path_base, ".html")
  #   new_file_path <- paste0(file_path_base, new_version, ".html")
  #   
  #   htmlwidgets::saveWidget(reactive_value, new_file_path, selfcontained = TRUE)
  #   if (file.exists(current_file_path)) {
  #     file.remove(current_file_path)
  #   }
  #   
  #   update_iframe(iframe_id = new_version %% 4 + 1, file_path_base)
  # }
  # 
  # # Observers for each plot
  # observeEvent(reactiveValues$plot1, {
  #   save_and_update_iframe(reactiveValues$plot1, "www/updatingFigs/summary")
  # })
  # 
  # observeEvent(reactiveValues$plot2, {
  #   save_and_update_iframe(reactiveValues$plot2, "www/updatingFigs/accuracy")
  # })
  # 
  # observeEvent(reactiveValues$plot3, {
  #   save_and_update_iframe(reactiveValues$plot3, "www/updatingFigs/learner")
  # })
  # 
  # observeEvent(reactiveValues$table, {
  #   save_and_update_iframe(reactiveValues$table, "www/updatingFigs/dt")
  # })
  # 
  # # update plots:
  # update_iframe(1, "updatingFigs/summary")
  # update_iframe(2, "updatingFigs/accuracy")
  # update_iframe(3, "updatingFigs/learner")
  # update_iframe(4, "updatingFigs/dt")
  
  lock_file <- "lock_file.lock"
  
  # Function to acquire a lock
  acquire_lock <- function(lock_file) {
    while(file.exists(lock_file)) {
      Sys.sleep(0.1)  # Wait for a short interval before trying again
    }
    file.create(lock_file)
    return(TRUE)
  }
  
  # Function to release a lock
  release_lock <- function(lock_file) {
    if (file.exists(lock_file)) {
      file.remove(lock_file)
    }
  }
  
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
  # word_pairs <- head(word_pairs, 10)

  # word_pairs <- word_pairs %>%
  #   group_by(condition) %>%
  #   slice_sample(n = 5)
  
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
    shinyjs::runjs("startTimer(300);") #########################
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
    # Show the loading indicator
    shinyjs::toggle("loading_message", condition = TRUE)
    
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
    # full_data <- read.csv('www/updatingData/formatted_data.csv') ###########
    # ppt_code <- (max(full_data$participant, na.rm = TRUE) + 1)
    
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
      filter(stringdist(tolower(study_response), tolower(cue), method = "lv") < 1) %>% 
      pull(index)
    
    na_response_d <- combined_data %>%
      filter(condition == 1) %>% 
      filter(is.na(study_response)) %>% 
      pull(index)
    
    if (length(na_response_a) != 0 | length(na_response_b) != 0 | length(na_response_c) != 0 | length(na_response_d) != 0){
      remove <- c(na_response_a, na_response_b, na_response_c, na_response_d)
      filtered_data <- combined_data %>%
        filter(!index %in% remove)
    } else{
      filtered_data <- combined_data 
    }
    
    # 2) Miss <10 guesses -- includes misses that were calculated as NA in step 1
    ## - This is where a participant may be excluded and prompted to try again
    if (nrow(filtered_data) < (nrow(word_pairs) - 10)) { ###################
      print("Not enough data due to...")
    } else {
      # 3) Remove items with correct guess
      ## - use stringdist to check typos
      correct_guess <- filtered_data %>% 
        filter(condition == 1) %>% 
        filter(stringdist(tolower(study_response), tolower(target), method = "lv") < 1) %>% 
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
    output$participantID <- renderUI({
      bold_ppt_code <- paste0("<b>", ppt_code, "</b>")
      HTML(paste0("Hi participant ", bold_ppt_code, "! Let's review your results. To see how you compare to the rest of our sample, view the interactive figures again. Look for your specific code: ", bold_ppt_code, " to see your own results in the table!"))
    })
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
    
    ## PYTHON
    reticulate::py_install("numpy")
    reticulate::py_install("scipy")
    reticulate::py_install("pandas")
    
    # Import packages
    numpy <- import("numpy")
    scipy <- import("scipy")
    pandas <- import("pandas")
    math <- import("math")
    
    # # Run MLE to find learner type: output: "you fit the ___ model x% better than the ___ model"
    source_python("www/LLerror.py")
    
    # Append new results
    ## remove index column and add participant
    if (acquire_lock(lock_file)) {
      # full_data <- read.csv('www/updatingData/formatted_data.csv') ###########
      reactiveData$full_data <- read_sheet("1pmeYWRJTQvIRFRzK4PjgB83DypA-XXtea_pj1a13WDo") ###########
      
      full_data <- reactiveData$full_data
      ppt_code <- (max(full_data$participant, na.rm = TRUE) + 1)
      
      clean_data$index <- NULL
      clean_data$participant <- ppt_code
      
      
      # full_data <- full_join(full_data, clean_data)
      # print(tail(full_data, 100))
      
      # write.csv(full_data, "www/updatingData/formatted_data.csv")
      sheet_append("1pmeYWRJTQvIRFRzK4PjgB83DypA-XXtea_pj1a13WDo", clean_data)
      reactiveData$full_data <- read_sheet("1pmeYWRJTQvIRFRzK4PjgB83DypA-XXtea_pj1a13WDo") ###########
      
      # LL_data <- read.csv("www/updatingData/LL_model1.csv", row.names = NULL) ##############
      reactiveData$LL_data <- read_sheet("1doyHkRu2NTgrlOA6OoTN8RRXkfZDxIkK1fX72VWxhBU") #############
      LL_data <- reactiveData$LL_data
      # LL_data <- LL_data[-nrow(LL_data), -1]
      
      LL_results <- ll_participant(clean_data, ppt_code, LL_data)
  
      participant_ll <- LL_results[[1]]
      column_names <- c("Participant", "elab.decay", "elab.temp", "elab.ter", "elab.LL", "med.decay", "med.temp", "med.ter", "med.LL", "best.model", "diff.LL")
      
      ppt_ll_df <- data.frame(matrix(unlist(participant_ll), nrow = 1, byrow = TRUE))
      colnames(ppt_ll_df) <- column_names
      
      print(ppt_ll_df)
      # LL_data <- LL_results[[2]]
      
      #write.csv(LL_data, "www/updatingData/LL_model1.csv")
      sheet_append("1doyHkRu2NTgrlOA6OoTN8RRXkfZDxIkK1fX72VWxhBU", ppt_ll_df)
      reactiveData$LL_data <- read_sheet("1doyHkRu2NTgrlOA6OoTN8RRXkfZDxIkK1fX72VWxhBU") #############
      
      # Release the lock
      release_lock(lock_file)
    } else {
      print("another process occuring, longer wait times may occur")
    }

    ## Final output -- use your participant code above to see how your results compare to other in the
    # interactive figures above!
    output$learnerSummary <- renderUI({
      learner_type <- participant_ll[[10]]
      likelihood <- exp(participant_ll[[11]])
      
      if (learner_type == "Elaborative") {
        styled_learner_type <- tags$span(style = "color:#f39c12;", as.character(learner_type))
      } else {
        styled_learner_type <- tags$span(style = "color:#9b59b6;", as.character(learner_type))
      }
      
      HTML(paste("You are a", styled_learner_type, "learner! This model was", as.character(round(likelihood)), "times more likely to fit your data."))
    })
    
    
    # #### FIRST FIGURE
    # # RE-summarize data:
    # cleandata_participant <- full_data %>% 
    #   group_by(participant, condition) %>% 
    #   summarize(
    #     performance = (sum(correct) / n())
    #   )
    # 
    # print(cleandata_participant)
    # 
    # cleandata_summary <- cleandata_participant %>% 
    #   group_by(condition) %>% 
    #   summarize(
    #     mean_performance = mean(performance),
    #     sd_performance = sd(performance),
    #     n = n(),
    #     stError = sd_performance / sqrt(n)
    #   )
    # 
    # # Update figures
    # summary_plot <- plot_ly(data = cleandata_participant, x = ~condition, y = ~performance) %>%
    #   add_markers(alpha = 0.75, color = I("#D9D6C7"), split = ~participant, marker = list(size=5),
    #               text = ~performance,
    #               textposition = "auto",
    #               hoverinfo = "text",
    #               hovertext = ~paste0("Participant: ", participant,
    #                                   "<br> Observed Accuracy: ", round(performance*100, 2), "%"),
    #               showlegend=FALSE) %>% 
    #   add_lines(color = I("#D9D6C7"), line = list(width = 0.3), split = ~participant,
    #             showlegend=FALSE) %>%
    #   add_markers(data = cleandata_summary %>%  filter(condition== 1), x = ~condition, y = ~mean_performance, error_y = list(type= "data", array = ~stError,color = '#e74c3c', thickness=2), type = "scatter", mode = "markers", marker = list(color = "#e74c3c", size = 12), legendgroup="average",
    #               name="Average - Error", showlegend=FALSE) %>% 
    #   add_markers(data = cleandata_summary %>%  filter(condition==2), x = ~condition, y = ~mean_performance, error_y = list(type= "data", array = ~stError,color = '#2ecc71', thickness=2), type = "scatter", mode = "markers", marker = list(color = "#2ecc71", size = 12), legendgroup="average",
    #               name="Average - Study", showlegend=FALSE) %>% 
    #   add_text(data = cleandata_summary %>% filter(condition == 1), x = ~condition, y = ~mean_performance, text = ~paste0(round(mean_performance * 100, 2), "%"), textposition = "bottom right", textfont=list(color = c("#000000"), size = 17), legendgroup="average",
    #            name="Average - Error", showlegend=FALSE) %>%
    #   add_text(data = cleandata_summary %>% filter(condition == 2), x = ~condition, y = ~mean_performance, text = ~paste0(round(mean_performance * 100, 2), "%"), textposition = "bottom right", textfont=list(color = c("#000000"), size = 17), legendgroup="average",
    #            name="Average - Study", showlegend=FALSE) %>%
    #   style(textposition = "top right") %>%
    #   layout(
    #     title = list(text= "Performance Overview", size = "3vmin"),
    #     xaxis = list(title = "Condition", autotypenumbers = "strict", range = c(0.5, 2.5), ticktext = list("Error Items", "Study Items"),
    #                  tickvals = list(1, 2),
    #                  tickmode = "array",
    #                  titlefont = list(size = "2.5vmin")),
    #     yaxis = list(title = "Accuracy",
    #                  titlefont = list(size = "2.5vmin")),
    #     showlegend = FALSE,
    #     hovermode = "closest"
    #   )
    # reactiveValues$plot1 <- summary_plot
    # 
    # 
    # ### SECOND FIGURE
    # learner_data <- cleandata_participant %>%
    #   mutate(condition = case_when(condition == 1 ~ 'error', condition == 2 ~ 'study')) %>%
    #   pivot_wider(names_from = condition, values_from = performance) %>%
    #   group_by(participant) %>%
    #   summarize(accuracy_diff = error - study) %>%
    #   mutate(Learner = case_when(accuracy_diff > 0 ~ 'Error Items', accuracy_diff <= 0 ~ 'Study Items'))
    # 
    # error_better <- learner_data %>%
    #   group_by(Learner) %>%
    #   summarize(count = n())
    # 
    # colors = c("#e74c3c", "#2ecc71")
    # 
    # # Create pie chart with custom colors
    # accuracy_plot <- plot_ly(
    #   data = error_better,
    #   labels = ~Learner,
    #   values = ~count,
    #   type = 'pie',
    #   textinfo = "label+percent",
    #   hoverinfo = "text+value",
    #   hole = 0.6,
    #   marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))
    # ) %>%
    #   layout(
    #     title = list(text="Best Performance",size=18),
    #     showlegend = FALSE,
    #     font = list(size = "4vmin", color = "#000000")
    #   )
    # 
    # reactiveValues$plot2 <- accuracy_plot
    # 
    # ### THIRD FIGURE
    # colors = c("#f39c12", "#9b59b6")
    # 
    # ll_pie <- LL_data %>%
    #   dplyr::select(best.model) %>%
    #   group_by(best.model) %>%
    #   summarize(count = n())
    # 
    # # Create pie chart with custom colors
    # learner_plot <- plot_ly(
    #   data = ll_pie,
    #   labels = ~best.model,
    #   values = ~count,
    #   type = 'pie',
    #   rotation = 150,
    #   textinfo = "label+percent",
    #   hoverinfo = "text+value",
    #   hole = 0.6,
    #   marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))
    # ) %>%
    #   layout(
    #     title = list(text="Type of Learner",size=18),
    #     showlegend = FALSE,
    #     font = list(size = "4vmin", color = "#000000")
    #   )
    # 
    # reactiveValues$plot3 <- learner_plot
    # 
    # ## TABLE
    # #Create datatable
    # clean_ll <- LL_data %>%
    #   dplyr::select(Participant, elab.LL, med.LL, diff.LL, best.model) %>%
    #   rename('Elaborative Score' = 'elab.LL', 'Mediator Score' = 'med.LL',
    #          'Score Difference' = 'diff.LL', 'Model' = 'best.model')
    # 
    # med_elab_formatter <-
    #   formatter("span",
    #             style = x ~ formattable::style(
    #               font.weight = "bold",
    #               color = ifelse(x == 'Mediator', "#9b59b6", ifelse(x == 'Elaborative', "#f39c12", "white")
    #             )
    #             ))
    # 
    # datatable_ll <- formattable(clean_ll,
    #                             list(
    #                               'Score Difference' = color_tile("#f7c46c", "#b984cc"),
    #                               'Model' = med_elab_formatter
    #                             )) %>%
    #   as.datatable(options = list(
    #     initComplete = JS(
    #       "function(settings, json) {",
    #       "$('body').css({'font-family': 'Calibri'});",
    #       "}"
    #     ),
    #     paging = TRUE,    ## paginate the output
    #     pageLength = 15,  ## number of rows to output for each page
    #     scrollX = TRUE,   ## enable scrolling on X axis
    #     scrollY = TRUE,   ## enable scrolling on Y axis
    #     autoWidth = TRUE, ## use smart column width handling
    #     server = FALSE,   ## use client-side processing
    #     dom = 'Bfrtip',
    #     buttons = c('csv', 'excel')),
    #     extensions = 'Buttons',
    #     selection = 'single', ## enable selection of a single row
    #     filter = 'bottom',              ## include column filters at the bottom
    #     rownames = FALSE                ## don't show row numbers/names
    #   )
    # 
    # # title <- tags$caption(
    # #   style = "caption-side: top; font-size: 18px; font-weight: bold; margin-bottom: 10px;",
    # #   "My Formattable Datatable"
    # # )
    # 
    # # # Combine the title and the datatable using htmltools::tagList
    # # datatable_ll <- htmltools::tagList(title, datatable_ll)
    # 
    # reactiveValues$table <- datatable_ll
    #   
    # # After the analysis is done, hide the loading indicator
    # shinyjs::toggle("loading_message", condition = FALSE)
  })
  
  observeEvent(reactiveData$full_data, {
    full_data <- reactiveData$full_data
    
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
    reactiveValues$plot1 <- summary_plot
    
    
    ### SECOND FIGURE
    learner_data <- cleandata_participant %>%
      mutate(condition = case_when(condition == 1 ~ 'error', condition == 2 ~ 'study')) %>%
      pivot_wider(names_from = condition, values_from = performance) %>%
      group_by(participant) %>%
      summarize(accuracy_diff = error - study) %>%
      mutate(Learner = case_when(accuracy_diff > 0 ~ 'Error Items', accuracy_diff <= 0 ~ 'Study Items'))
    
    error_better <- learner_data %>%
      group_by(Learner) %>%
      summarize(count = n())
    
    colors = c("#e74c3c", "#2ecc71")
    
    # Create pie chart with custom colors
    accuracy_plot <- plot_ly(
      data = error_better,
      labels = ~Learner,
      values = ~count,
      type = 'pie',
      textinfo = "label+percent",
      hoverinfo = "text+value",
      hole = 0.6,
      marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))
    ) %>%
      layout(
        title = list(text="Best Performance",size=18),
        showlegend = FALSE,
        font = list(size = "4vmin", color = "#000000")
      )
    
    reactiveValues$plot2 <- accuracy_plot
    
    # Save new plot to summary1.html and delete summary.html
    htmlwidgets::saveWidget(reactiveValues$plot1, "www/updatingFigs/summary1.html", selfcontained = TRUE)
    shinyjs::runjs("updateIframe1('updatingFigs/summary1.html')")
    
    print("Saved plot1")
    
    # Save new plot to accuracy1.html and delete accuracy.html
    htmlwidgets::saveWidget(reactiveValues$plot2, "www/updatingFigs/accuracy1.html", selfcontained = TRUE)
    shinyjs::runjs("updateIframe2('updatingFigs/accuracy1.html')")
    print("Saved plot2")
  })
  
  observeEvent(reactiveData$LL_data, {
    LL_data <- reactiveData$LL_data
    
    ### THIRD FIGURE
    colors = c("#f39c12", "#9b59b6")
    
    ll_pie <- LL_data %>%
      dplyr::select(best.model) %>%
      group_by(best.model) %>%
      summarize(count = n())
    
    # Create pie chart with custom colors
    learner_plot <- plot_ly(
      data = ll_pie,
      labels = ~best.model,
      values = ~count,
      type = 'pie',
      rotation = 150,
      textinfo = "label+percent",
      hoverinfo = "text+value",
      hole = 0.6,
      marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))
    ) %>%
      layout(
        title = list(text="Type of Learner",size=18),
        showlegend = FALSE,
        font = list(size = "4vmin", color = "#000000")
      )
    
    reactiveValues$plot3 <- learner_plot
    
    ## TABLE
    #Create datatable
    clean_ll <- LL_data %>%
      dplyr::select(Participant, elab.LL, med.LL, diff.LL, best.model) %>%
      rename('Elaborative Score' = 'elab.LL', 'Mediator Score' = 'med.LL',
             'Score Difference' = 'diff.LL', 'Model' = 'best.model')
    
    med_elab_formatter <-
      formatter("span",
                style = x ~ formattable::style(
                  font.weight = "bold",
                  color = ifelse(x == 'Mediator', "#9b59b6", ifelse(x == 'Elaborative', "#f39c12", "white")
                  )
                ))
    
    datatable_ll <- formattable(clean_ll,
                                list(
                                  'Score Difference' = color_tile("#f7c46c", "#b984cc"),
                                  'Model' = med_elab_formatter
                                )) %>%
      as.datatable(options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$('body').css({'font-family': 'Calibri'});",
          "}"
        ),
        paging = TRUE,    ## paginate the output
        pageLength = 15,  ## number of rows to output for each page
        scrollX = TRUE,   ## enable scrolling on X axis
        scrollY = TRUE,   ## enable scrolling on Y axis
        autoWidth = TRUE, ## use smart column width handling
        server = FALSE,   ## use client-side processing
        dom = 'Bfrtip',
        buttons = c('csv', 'excel')),
        extensions = 'Buttons',
        selection = 'single', ## enable selection of a single row
        filter = 'bottom',              ## include column filters at the bottom
        rownames = FALSE                ## don't show row numbers/names
      )
    
    # title <- tags$caption(
    #   style = "caption-side: top; font-size: 18px; font-weight: bold; margin-bottom: 10px;",
    #   "My Formattable Datatable"
    # )
    
    # # Combine the title and the datatable using htmltools::tagList
    # datatable_ll <- htmltools::tagList(title, datatable_ll)
    
    reactiveValues$table <- datatable_ll
    
    
    htmlwidgets::saveWidget(reactiveValues$plot3, "www/updatingFigs/learner1.html", selfcontained = TRUE)
    shinyjs::runjs("updateIframe3('updatingFigs/learner1.html')")
    print("Saved plot3")
    
    htmlwidgets::saveWidget(reactiveValues$table, "www/updatingFigs/dt1.html", selfcontained = TRUE)
    shinyjs::runjs("updateIframe4('updatingFigs/dt1.html')")
    print("Saved plot4")
    
    # After the analysis is done, hide the loading indicator
    shinyjs::toggle("loading_message", condition = FALSE)
  })
  
  # # Each plot refresh changes between two names to avoid caching issues
  # observeEvent(reactiveValues$plot1, {
  #   # Define file paths
  #   file_path1 <- "www/updatingFigs/summary.html"
  #   file_path2 <- "www/updatingFigs/summary1.html"
  #   
  #   # Determine which file to write to and which to delete
  #   if (file.exists(file_path1)) {
  #     # Save new plot to summary1.html and delete summary.html
  #     htmlwidgets::saveWidget(reactiveValues$plot1, file_path2, selfcontained = TRUE)
  #     file.remove(file_path1)
  #     shinyjs::runjs("updateIframe1('updatingFigs/summary1.html')")
  #   } else {
  #     # Save new plot to summary.html and delete summary1.html
  #     htmlwidgets::saveWidget(reactiveValues$plot1, file_path1, selfcontained = TRUE)
  #     if (file.exists(file_path2)) {
  #       file.remove(file_path2)
  #     }
  #     shinyjs::runjs("updateIframe1('updatingFigs/summary.html')")
  #   }
  # })
  # 
  # observeEvent(reactiveValues$plot2, {
  #   # Define file paths
  #   file_path1 <- "www/updatingFigs/accuracy.html"
  #   file_path2 <- "www/updatingFigs/accuracy1.html"
  #   
  #   # Determine which file to write to and which to delete
  #   if (file.exists(file_path1)) {
  #     # Save new plot to accuracy1.html and delete accuracy.html
  #     htmlwidgets::saveWidget(reactiveValues$plot2, file_path2, selfcontained = TRUE)
  #     file.remove(file_path1)
  #     shinyjs::runjs("updateIframe2('updatingFigs/accuracy1.html')")
  #   } else {
  #     # Save new plot to accuracy.html and delete accuracy1.html
  #     htmlwidgets::saveWidget(reactiveValues$plot2, file_path1, selfcontained = TRUE)
  #     if (file.exists(file_path2)) {
  #       file.remove(file_path2)
  #     }
  #     shinyjs::runjs("updateIframe2('updatingFigs/accuracy.html')")
  #   }
  # })
  # 
  # observeEvent(reactiveValues$plot3, {
  #   # Define the file path
  #   file_path1 <- "www/updatingFigs/learner.html"
  #   file_path2 <- "www/updatingFigs/learner1.html"
  #   
  #   # Determine which file to write to and which to delete
  #   if (file.exists(file_path1)) {
  #     # Save new plot to accuracy1.html and delete accuracy.html
  #     htmlwidgets::saveWidget(reactiveValues$plot3, file_path2, selfcontained = TRUE)
  #     file.remove(file_path1)
  #     shinyjs::runjs("updateIframe3('updatingFigs/learner1.html')")
  #   } else {
  #     # Save new plot to accuracy.html and delete accuracy1.html
  #     htmlwidgets::saveWidget(reactiveValues$plot3, file_path1, selfcontained = TRUE)
  #     if (file.exists(file_path2)) {
  #       file.remove(file_path2)
  #     }
  #     shinyjs::runjs("updateIframe3('updatingFigs/learner.html')")
  #   }
  # })
  # 
  # observeEvent(reactiveValues$table, {
  #   # Define the file path
  #   file_path1 <- "www/updatingFigs/dt.html"
  #   file_path2 <- "www/updatingFigs/dt1.html"
  # 
  #   # Determine which file to write to and which to delete
  #   if (file.exists(file_path1)) {
  #     htmlwidgets::saveWidget(reactiveValues$table, file_path2, selfcontained = TRUE)
  #     file.remove(file_path1)
  #     shinyjs::runjs("updateIframe4('updatingFigs/dt1.html')")
  #   } else {
  #     # Save new plot to accuracy.html and delete accuracy1.html
  #     htmlwidgets::saveWidget(reactiveValues$table, file_path1, selfcontained = TRUE)
  #     if (file.exists(file_path2)) {
  #       file.remove(file_path2)
  #     }
  #     shinyjs::runjs("updateIframe4('updatingFigs/dt.html')")
  #   }
  # })
  
}