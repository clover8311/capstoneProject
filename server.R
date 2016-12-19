library(shiny);
source("/Users/yanze/github/capstone project/prepareNGrams.R");
source("/Users/yanze/github/capstone project/testPredictNextWord.R");
source("/Users/yanze/github/capstone project/prepareInputString.R");
shinyServer(
  function(input, output, session) {
    
    output$inputWordsText  <- renderText({
      paste("content: ", as.character(input$inputWords))
    })
    
    output$forecastText <- renderText({
      input$goButton
      returnWord <- isolate(runPredictFunction(input$inputWords))
      as.character(returnWord)
    })
    
  }
)
