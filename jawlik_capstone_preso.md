JHU Data Science Capstone
========================================================
author: Matthew Jawlik
date: 2018-02-14
autosize: true

Problem: People don't like to type
========================================================

More and more, our written communication is moving from laptops/desktops to mobile devices. Thumbing on a keypad is slow and inaccurate relative to typing on a keyboard; a tool that saves keystrokes could be a blessing.  

### **Solution:** Predict the next word so users don't have to type

Using a large sample of actual written language, develop an algorithm predicting the likeliest word to follow.  In application, this will save users keystrokes & time.

Shiny app:  <https://jawlik.shinyapps.io/capstone/>.

R Code and documentation: <https://github.com/jawlik/capstone>


Solution Direction / Algorithm
========================================================


### The solution can be summarized by the following steps:
- Download the Swftkey files
- Randomly sample 2% of entries for each of the 3 US files
- Cleanup and normalize the datasets [detail on next slide]
- Tokenize the data into ngrams (1-, 2-, 3-, 4-) with Quanteda package
- Perform the same cleanup steps on inputted data
- Employ a Katz Backoff algorithm to attemp a match, first looking at quadgrams, then trigrams, then bigrams
- Return the matched value

Data Cleanup / Preprocessing
========================================================

### To increase accuracy, I performed the following:
- Increased sample from 30k to 2% of each file (~85k total)
- Trimmed whitespace, removed english stopwords, and converted all characters to lowercase
- Removed all punctuation, EXCEPT apostrophes
- Sorted the datasets descending by frequency

### To improve efficiency I did the following:
- Removed bigrams that didn't appear more than twice, trigrams and quadgrams that appeared only once
- Converted dfm to dataframes, and exported as .dat to minimize filesize
- Limited processing to JUST finding a match; no additional actions are performed 
