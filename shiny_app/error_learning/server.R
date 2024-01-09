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
  
  responses <- reactiveValues(values = list())
  answers <- reactiveValues(values = list())
  
  responses_data <- data.frame(cue = character(0), target = character(0), condition = integer(0), study_response = character(0), study_rt = integer(0))
  answers_data <- data.frame(cue = character(0), target = character(0), condition = integer(0), test_response = character(0), test_rt = integer(0))
  
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
    
    n <- length(names(input$responsesObject))
    responses_data <- responses_data[1:n,]
    # Example: Extracting cues and responses
    responses_data$cue <- names(input$responsesObject)
    responses_data$target <- sapply(input$responsesObject, function(x) x[1])
    responses_data$condition <- sapply(input$responsesObject, function(x) x[2])
    responses_data$study_response <- sapply(input$responsesObject, function(x) x[3])
    responses_data$study_rt <- sapply(input$responsesObject, function(x) x[4])
    rownames(responses_data) <- NULL
    
    print(responses_data)
    # Clear last word pair and start timer
    shinyjs::runjs(sprintf("shinyjs.updateWordPair('%s', '%s', %d)", "", "", 2))
    shinyjs::runjs("startTimer(300);")
    # You can process or analyze the responses here
  })
  
  # Now test session
  current_index <- reactiveVal(1)
  
  #Event to start the test
  observeEvent(input$startTest, {
    # Shuffle the word pairs
    word_pairs <- word_pairs[sample(nrow(word_pairs)), ]
    
    current_index(1)  # Initialize to the first word pair
    if (nrow(word_pairs) > 0) {
      cue <- word_pairs$cue[1]
      target <- word_pairs$target[1]
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
      shinyjs::runjs(sprintf("shinyjs.updateTest('%s', '%s', %d)", cue, target, condition))
    } else {
      # Call the JavaScript function to send responses to Shiny
      shinyjs::runjs("shinyjs.sendAnswersToShiny();")
      # Clear the text box
      shinyjs::runjs('$("#answerTestContainer").empty()')
    }
  })
  
  
  observeEvent(input$answerObject, {
    # Do something with the received responsesObject
    answers$values <- input$answerObject
    
    n <- length(names(input$answerObject))
    answers_data <- answers_data[1:n,]
    
    answers_data$cue <- names(input$answerObject)
    answers_data$target <- sapply(input$answerObject, function(x) x[1])
    answers_data$condition <- sapply(input$answerObject, function(x) x[2])
    answers_data$test_response <- sapply(input$answerObject, function(x) x[3])
    answers_data$test_rt <- sapply(input$answerObject, function(x) x[4])
    rownames(answers_data) <- NULL
    
    print(answers_data)
    # Clear last word pair and start timer
    # shinyjs::runjs(sprintf("shinyjs.updateTest('%s', '%s', %d)", "", "", 2))
    # You can process or analyze the responses here
  })
  
  # Function to analyze combined data
  analyzeData <- function() {
    if (nrow(answers_data) == nrow(word_pairs)) {
      combined_data <- merge(responses_data, answers_data, by = c("cue", "target", "condition"), all = TRUE)
      # Perform your analysis here...
      print(combined_data)
    }
  }
}
