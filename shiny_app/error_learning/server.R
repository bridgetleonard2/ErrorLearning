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
install.packages("stringdist")
library(stringdist)


# Define server logic required to draw a histogram
# Define server logic required to draw a histogram
function(input, output, session) {
  # Initialize shinyjs to ensure that shinyjs functions are available
  shinyjs::useShinyjs()
  
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
        filter(!index %in% correct_guess)
      
      # Get results
      # print("Participant XXXX's Results")
      # Calculate accuracy: output: "you performed x% better on ___ items than ___"
      ## use stringdist to check typos and case
      clean_data <- clean_data %>% 
        mutate(correct = case_when(stringdist(tolower(test_response), tolower(target), method = "lv") < 3 ~ 1,
                                   stringdist(tolower(test_response), tolower(target), method = "lv") >= ~ 1))
      
      print(clean_data)
      accuracy <- clean_data %>% 
        group_by(condition) %>% 
        summarize(
          accuracy = sum(correct) / nrow(clean_data())
        )
      print(accuracy)
    }
    
    
    # 4) remove items with rt < 200 and rt > 15000
    # Analyze RT: output: "you responded x% faster on ___ items compared to ___"
    
    # Run MLE to find learner type: output: "you fit the ___ model x% better than the ___ model"
    
    
    ## Final output -- use your participant code above to see how your results compare to other in the
    # interactive figures above!
  })
  
}
