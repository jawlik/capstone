
# Perform more cleanup to facilitate modeling ####

# Trim datasets to remove low frequency n-grams
#uniTrim <- dfm_trim(unigrams, min_count = 2)
biTrim <- dfm_trim(bigrams, min_count = 2)
triTrim <- dfm_trim(trigrams, min_count = 1)
quadTrim <- dfm_trim(quadgrams, min_count = 1)

# Sort each dfm descending by frequency
#uniTrim <- dfm_sort(uniTrim, decreasing = T, margin = "features")
biTrim <- dfm_sort(biTrim, decreasing = T, margin = "features")
triTrim <- dfm_sort(triTrim, decreasing = T, margin = "features")
quadTrim <- dfm_sort(quadTrim, decreasing = T, margin = "features")

# Convert each dfm into a df for easier manipulation; split words into separate cols
#uniTrim <- data.frame(Words = featnames(uniTrim), Freq = colSums(uniTrim), 
#                      row.names = NULL, stringsAsFactors = FALSE)
biTrim <- data.frame(Words = featnames(biTrim), Freq = colSums(biTrim), 
                      row.names = NULL, stringsAsFactors = FALSE)
biTrim <- cSplit(biTrim, "Words", "_")
biTrim <- biTrim[,1:3]
triTrim <- data.frame(Words = featnames(triTrim), Freq = colSums(triTrim), 
                      row.names = NULL, stringsAsFactors = FALSE)
triTrim <- cSplit(triTrim, "Words", "_")
triTrim <- triTrim[,1:4]
quadTrim <- data.frame(Words = featnames(quadTrim), Freq = colSums(quadTrim), 
                      row.names = NULL, stringsAsFactors = FALSE)
quadTrim <- cSplit(quadTrim, "Words", "_")
quadTrim <- quadTrim[,1:5]


# Create functions for predicting the next word ####
# bigram function
bi_predict<- function(words) {
  words <- trimws(words)
  words <- tolower(words)
  words <- gsub("[^[:alnum:][:space:]']", "", words)
  last_word <- tail(unlist(strsplit(words, " ")), 1)
  matched <- match(last_word,biTrim$Words_1)
  #matched2 <- biTrim[biTrim$Words_1 == last_word]
  ifelse(is.na(matched), 
         return(NULL),
         return(biTrim[matched, 3])
         #return(matched2)
         )
}

# Trigram function
tri_predict<- function(words) {
  words <- trimws(words)
  words <- tolower(words)
  words <- gsub("[^[:alnum:][:space:]']", "", words)
  last_words <- tail(unlist(strsplit(words, " ")), 2)
  matched <- match(paste(last_words[1],last_words[2]),
                         paste(triTrim$Words_1, triTrim$Words_2))
  #matched3 <- triTrim[(triTrim$Words_1 == last_words[1]) & (triTrim$Words_2 == last_words[2]), ]
  ifelse(is.na(matched), 
         return(NULL),
         return(triTrim[matched, 4])
         #return(matched3)
  )
}

# Quadgram function
quad_predict<- function(words) {
  words <- trimws(words)
  words <- tolower(words)
  words <- gsub("[^[:alnum:][:space:]']", "", words)
  last_words <- tail(unlist(strsplit(words, " ")), 3)
  matched <- match(paste(last_words[1],last_words[2], last_words[3]),
                   paste(quadTrim$Words_1, quadTrim$Words_2, quadTrim$Words_3))
  #matched4 <- quadTrim[(quadTrim$Words_1 == last_words[1]) & (quadTrim$Words_2 == last_words[2]) & 
  #                      (quadTrim$Words_3 == last_words[3]), ]
  ifelse(is.na(matched), 
         return(NULL),
         return(quadTrim[matched, 5])
         #return(matched4)
  )
}



# Create the Katz backoff algorithm to find the best match ####
# Look first in quadgram, them trigram, then bigram. If none, return "the"
ifelse(!is.null(quad_predict(testText)),
       quad_predict(testText),
       ifelse(!is.null(tri_predict(testText)),
              tri_predict(testText),
              ifelse(!is.null(bi_predict(testText)),
                     bi_predict(testText),
                     "the")))



write.table(biTrim[,2:3], 'biTrim.dat', row.names=FALSE)
write.table(triTrim[,2:4], 'triTrim.dat', row.names=FALSE)
write.table(quadTrim[,2:5], 'quadTrim.dat', row.names=FALSE)
