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

# Define word pairs and conditions
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
                2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
)


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
    print(responses$values)
    
    # Example: Extracting cues and responses
    cues <- names(responses$values)
    responses_list <- lapply(responses$values, function(x) x[1])
    response_times_list <- lapply(responses$values, function(x) x[2])
    
    print(cues)
    print(responses_list)
    print(response_times_list)
    
    # You can process or analyze the responses here
  })
}
