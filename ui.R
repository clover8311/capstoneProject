library(shiny)
shinyUI(
  fluidPage(
    h1("Predict your next word"),
    br(),
    h3("Enter what you want to say, and I will predict your next word"),
    br(),
    br(),
    br(),
    column(6, wellPanel(

      textInput('inputWords', "Your words", value = "", width = '90%', placeholder = "Please enter at least one word"),
      p("Please enter at least one word, then press Predict button to predict. It may take around 10 seconds to get the answer."),
      br(),
      actionButton("goButton", "Predict")
    )),
    column(6,
           p("What you said"),
           verbatimTextOutput("inputWordsText"),
           br(),
           p("The next word is:"),
           verbatimTextOutput("forecastText")
    ))
) 