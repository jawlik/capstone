#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(

  titlePanel("Jawlik Data Science Capstone"),
  h3("Text Prediction Algorithm"),
  
  sidebarLayout(
    sidebarPanel(
       textInput("text1",
                   "Enter text here:",
                   value = "end of the")
    ),
    
    # Return the predicted word
    mainPanel(
      h4("Predicted next word (thanks for your patience):"),
      h3(p(strong(textOutput("matched"))))
      #h5("Next word predicted via:"),
      #h4(p(strong(textOutput("fxn"))))
    )
  )
))
