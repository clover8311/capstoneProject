library(shiny);
source("/Users/yanze/github/capstone project/prepareNGrams.R");
source("/Users/yanze/github/capstone project/testPredictNextWord.R");
source("/Users/yanze/github/capstone project/prepareInputString.R");
shinyServer(
  function(input, output, session) {
    
    output$inputWordsText  <- renderText({
      paste("content: ", as.character(input$inputWords))
    })
    
    observeEvent(input$goButton, {
      cat("Showing", "rows\n")
    })
    output$forecastText <- renderText({
      input$goButton
      
      progress <- Progress$new(session, min=1, max=15)
      on.exit(progress$close())
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...')
      
      returnWord <- isolate(runPredictFunction(input$inputWords))
      
      for (i in 1:5) {
        progress$set(value = i)
        Sys.sleep(0.5)
      }
      as.character(returnWord)
    })
    
  }
)
