library(shiny)
library(shinyjs)

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

# Shiny app UI
ui <- fluidPage(
  titlePanel("Reaction Time Experiment"),
  mainPanel(
    uiOutput("experimentDisplay"),
    textInput("response", label = "Type your response"),
    actionButton("submitBtn", "Submit"),
    textOutput("responseText")
  )
)

# Shiny app server
server <- function(input, output, session) {
  current_trial <- reactiveVal(1)
  
  observe({
    if (current_trial() <= nrow(word_pairs)) {
      output$experimentDisplay <- renderUI({
        pair <- word_pairs[current_trial(), ]
        condition <- pair$condition
        
        if (condition == 1) {
          # Study condition
          renderStudy(pair)
        } else {
          # Test condition
          renderTest(pair)
        }
      })
    } else {
      # End of the experiment
      output$experimentDisplay <- renderText("End of the experiment.")
    }
  })
  
  observe({
    if (input$submitBtn) {
      # Capture response and save data
      saveData()
      
      # Move to the next trial
      current_trial(current_trial() + 1)
    }
  })
  
  renderStudy <- function(pair) {
    # Render study UI
    shinyjs::runjs(sprintf("showStudy('%s', '%s');", pair$cue, pair$target))
    invalidateLater(10000, session)
  }
  
  renderTest <- function(pair) {
    # Render test UI
    shinyjs::runjs(sprintf("showTest('%s');", pair$cue))
    invalidateLater(100000, session)
  }
  
  saveData <- function() {
    # Capture and save response data
    response <- input$response
    condition <- word_pairs$condition[current_trial()]
    
    # Your data saving logic here
    # For example: save to a data frame or external file
    # data <- data.frame(cue = word_pairs$cue[current_trial()], target = word_pairs$target[current_trial()], response = response, condition = condition)
    # Save data to your desired storage
  }
}

# Run the Shiny app
shinyApp(ui, server)
