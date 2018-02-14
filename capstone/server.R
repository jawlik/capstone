#
# This is the server logic for Matt Jawlik's Data Science Capstone Project 
# 

### Setup the environment ####
# Load necessary libraries
library(shiny)
library(splitstackshape)
# Load the data tables
biTrim <- read.table('biTrim.dat', header = T)
triTrim <- read.table('triTrim.dat', header = T)
quadTrim <- read.table('quadTrim.dat', header = T)
# convert from factors to character, so functions don't return factor values
biTrim[] <- lapply(biTrim, as.character)
triTrim[] <- lapply(triTrim, as.character)
quadTrim[] <- lapply(quadTrim, as.character)



### Create functions for predicting the next word ####
# Bigram prediction function
bi_predict<- function(words) {
  words <- trimws(words)
  words <- tolower(words)
  words <- gsub("[^[:alnum:][:space:]']", "", words)
  last_word <- tail(unlist(strsplit(words, " ")), 1)
  matched <- match(last_word,biTrim$Words_1)
  ifelse(is.na(matched), 
         return(NULL),
         return(biTrim[matched, 2])
  )
}

# Trigram prediction function
tri_predict<- function(words) {
  words <- trimws(words)
  words <- tolower(words)
  words <- gsub("[^[:alnum:][:space:]']", "", words)
  last_words <- tail(unlist(strsplit(words, " ")), 2)
  matched <- match(paste(last_words[1],last_words[2]),
                   paste(triTrim$Words_1, triTrim$Words_2))
  ifelse(is.na(matched), 
         return(NULL),
         return(triTrim[matched, 3])
  )
}

# Quadgram prediction function
quad_predict<- function(words) {
  words <- trimws(words)
  words <- tolower(words)
  words <- gsub("[^[:alnum:][:space:]']", "", words)
  last_words <- tail(unlist(strsplit(words, " ")), 3)
  matched <- match(paste(last_words[1],last_words[2], last_words[3]),
                   paste(quadTrim$Words_1, quadTrim$Words_2, quadTrim$Words_3))
  ifelse(is.na(matched), 
         return(NULL),
         return(quadTrim[matched, 4])
  )
}



### Create the Katz backoff algorithm to find the best match ####
# Look first in quadgram, them trigram, then bigram. If none, return "the"
next_predict <- function(testText) {
  if(!is.null(quad_predict(testText))) {
    return(quad_predict(testText))
    ngram<-'quadgram'
  } else if(!is.null(tri_predict(testText))) {
    return(tri_predict(testText))
    ngram<-'trigram'
    } else if(!is.null(bi_predict(testText))) {
      return(bi_predict(testText))
      ngram<-'brigram'
    } else {
        return ("the")
        ngram<-'unigram'
    }
  }



shinyServer(function(input, output) {
  nextWord <- reactive ({
    next_predict(input$text1)
  })
    
    output$matched <- renderText({
      nextWord()
    })
    
})
