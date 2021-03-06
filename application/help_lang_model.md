---
title: "Language Model"
output: html_document
---

#### Objective: Simplify the typing of a text by forecasting the next word that might be typed

- Input sentence is cleaned and tokenized - if there are 3 words or more it will create a trigram, otherwise it goes down to bigram and respective unigram 
- Katz's back-off model: use tri-gram if there is good evidence, otherwise bi-gram, otherwise uni-gram to return the most probable word from data table: 

`["n-gram"; "conditional probability"]` 
- The conditional probability of a word given its history in the `n-gram`. It is computed using `order n` Markov approximation.
- The app uses library `data.table` for speed and it uses reactive functions to avoid a button in UI

Details:

1. When the fist space will be introduced the prediction function is evaluated. 
  + 1.1. For the introduced word: the pattern is grep-ed  within the n-gram frequency matrix elements. The predicted word will be the last word of the match bi-gram. If there is one or more than one match, then rank the options in decreasing order of probabilities. If not - there is no match - it will __backoff__ to uni-grams and the most frequent/probable word will be served as prediction.
  + 1.2. When the fist space is introduced the prediction is based on bi-grams - first, and uni-grams - second. 
  + 1.3. When the second space will be introduced the prediction will be based on trigrams - first and bi-grams - second. 
  + 1.4. When the third space will be introduced the prediction will be based on quad-grams - first and trigrams - second.
2. When the further spaces will be introduced ( >3 ) the prediction function will take into consideration an evaluation of the probabilities in descending order: quad-grams, tri-grams, bi-grams and unigrams. The word with highest probability will be served.
3. The model will be tested tested with sentences from the testing data set.


Main Function


```r
mostProbableWord<- function(inputString){
        
        newString <- trim(inputString)
        theStringVector <- unlist(strsplit(newString, " "))
        lengthStringVector <- length(theStringVector)
        stringOne <- sample(as.character(mostFreqWords), 1)
        
        if(lengthStringVector >= 3){
                
                newString <- paste(tail(theStringVector,3)[1],tail(theStringVector,3)[2],tail(theStringVector,3)[3], sep=" ")
                text_searched <- paste0("^", newString, " ")
                reduced_table <- quadDT[Wordset %like% text_searched][order(-Pmle_MKV_Order_3)]
                theWord <- as.character(reduced_table$word_4[1])
                
                if(is.na(theWord)){
                        newString <- paste(tail(theStringVector,2)[1],tail(theStringVector,2)[2], sep=" ")
                        text_searched <- paste0("^", newString, " ")
                        reduced_table <- triDT[Wordset %like% text_searched][order(-Pmle_MKV_Order_2)]
                        theWord <- as.character(reduced_table$word_3[1])
                        
                        if(is.na(theWord)){
                                newString <- tail(theStringVector,1)
                                text_searched <- paste0("^", newString, " ")
                                reduced_table <- biDT[Wordset %like% text_searched][order(-Pmle_MKV_Order_1)]
                                theWord <- as.character(reduced_table$word_2[1])
                                theWord <- ifelse(!is.na(theWord),theWord,stringOne)
                        }       
                }        
        }else if(lengthStringVector == 2){
                newString <- paste(tail(theStringVector,2)[1],tail(theStringVector,2)[2], sep=" ")
                text_searched <- paste0("^", newString, " ")
                reduced_table <- triDT[Wordset %like% text_searched][order(-Pmle_MKV_Order_2)]
                theWord <- as.character(reduced_table$word_3[1])     
                
                if(is.na(theWord)){
                        newString <- tail(theStringVector,1)
                        text_searched <- paste0("^", newString, " ")
                        reduced_table <- biDT[Wordset %like% text_searched][order(-Pmle_MKV_Order_1)]
                        theWord <- as.character(reduced_table$word_2[1])
                        theWord <- ifelse(!is.na(theWord),theWord,stringOne)
                }        
        }else if(lengthStringVector == 1){
                text_searched <- paste0("^", newString, " ")
                reduced_table <- biDT[Wordset %like% text_searched][order(-Pmle_MKV_Order_1)]
                theWord <- as.character(reduced_table$word_2[1])
                theWord <- ifelse(!is.na(theWord),theWord,stringOne)
        }else{ theWord  <- stringOne  }
        
        return(theWord)
        
}
```

