library(stringr)
library(plyr)
library(tm)
library(quanteda)
library(wordcloud)
library(tokenizers)
library(ggplot2)
library(splitstackshape)

### Read in the data ####
# Download the zip to a temp directory and unzip it
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",temp)
fns <- unzip(temp, junkpaths = TRUE, exdir = tempdir())
# Read in just the three English text files
us_twitter <- readLines(fns[7])
us_news <- readLines(fns[8])
us_blog <- readLines(fns[9])


### Exploratory Data Analysis ####
# perform some initial summary analysis on the full datasets
summ <- data.frame(
  fileName = c("us_twitter", "us_news", "us_blog"),
  records = c(length(us_twitter), length(us_news), length(us_blog)),
  minCharacters = c(min(nchar(us_twitter)), min(nchar(us_news)), min(nchar(us_blog))),
  maxCharacters = c(max(nchar(us_twitter)), max(nchar(us_news)), max(nchar(us_blog))),
  maxWords = c(max(str_count(us_twitter, "\\S+")), max(str_count(us_news, "\\S+")),
               max(str_count(us_blog, "\\S+"))),
  avgWords = c(mean(str_count(us_twitter, "\\S+")), mean(str_count(us_news, "\\S+")),
               mean(str_count(us_blog, "\\S+"))),
  medianWords = c(median(str_count(us_twitter, "\\S+")), median(str_count(us_news, "\\S+")),
                  median(str_count(us_blog, "\\S+")))
)
summ
### Sample 2% of records from each file and tokenize ####

set.seed(999)
samp <- c(
  sample(us_twitter, round(length(us_twitter)*.02)), 
  sample(us_news, round(length(us_news)*.02)),
  sample(us_blog, round(length(us_blog)*.02))
  )
# Remove whitespace, non-apostrophe punctuation, and convert to lowercase
samp <- trimws(samp)
samp <- gsub("[^[:alnum:][:space:]']", "", samp)
samp <- tolower(samp)
# remove whitespace, convert to lowercase, and remove stopwords and punctuation
#corp <- VCorpus(VectorSource(samp))
#corp <- tm_map(corp, stripWhitespace)
#corp <- tm_map(corp, content_transformer(tolower))
#corp <- tm_map(corp, removeWords, stopwords("english"))
#corp <- tm_map(corp, content_transformer(removePunctuation))

# Tokenize words, bigrams, trigrams & quadgrams using quanteda package (per instructions here:
#    https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html)
unigrams <- dfm(samp, remove = stopwords("en"), remove_punct = TRUE, 
                ngrams = 1, verbose = FALSE)
bigrams <- dfm(samp, remove = stopwords("english"), ngrams = 2, verbose = FALSE)
trigrams <- dfm(samp, remove = stopwords("english"), ngrams = 3, verbose = FALSE)
quadgrams <- dfm(samp, remove = stopwords("english"), ngrams = 4, verbose = FALSE)

# Perform some more EDA
topfeatures(bigrams, 20)
topfeatures(trigrams, 20)
topfeatures(quadgrams, 20)

top_uni <- topfeatures(unigrams, 20)
barplot(top_uni, main = 'Top Unigrams', xlab = 'Grams', ylab = 'Sample Count', axes = T,
        axisnames = T, las = 2)

textplot_wordcloud(unigrams, min.freq = 1000, random.order = FALSE, rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
textplot_wordcloud(bigrams, min.freq = 500, random.order = FALSE, rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
textplot_wordcloud(trigrams, min.freq = 50, random.order = FALSE, rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))


### Perform more cleanup to facilitate modeling ####

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


### Create functions for predicting the next word ####
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



### Create the Katz backoff algorithm to find the best match ####
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




unlink(temp)
