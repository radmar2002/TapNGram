rm(list=ls())

library("gdata")
library(data.table)

DIR <- "D:/WORK_2014/Certification_Data_Science/Data_Science_Capstone/TapNGram/data/"
setwd(DIR)

load("pivots.RData")

## Clean Function
cleanTheInputText <- function(txt){
        
        txt <- trim(tolower(txt))
        
        # Use the apostrophe ' characters to be consistent cross corpus
        txt <- gsub("\xe2\x80\x99", "'", txt, perl=TRUE)
        txt <- gsub("\u0060|\u0027|\u2019|\u000A|\u0091|\u0092|\u0093|\u0094", "'", txt, perl=TRUE)
        # remove URLs
        txt <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", " ", txt, perl=TRUE)
        # Function to remove nonprintable characters
        txt <- gsub("[^[:print:]]", " ", txt)
        # Remove "RT " and "via "
        txt <- gsub("RT |via ", " ", txt, perl=TRUE)
        # Remove twitter accounts
        txt <- gsub("@[^\\s]+", " ", txt, perl=TRUE)
        # Remove single quotes that are quoted strings but not in the middle of a word e.g. don't, I'm, etc. will remain the same
        txt <- gsub("(?<!\\w)'(?<!\\w)" , " ", txt, perl=TRUE)
        # Strip useless UTF-8 characters
        txt <- iconv(txt, "UTF-8", "ASCII", "")
        # Remove unused characters
        txt <- gsub('\\(|\\)|\"|\\$|/|&|\\*|<|>|\\%|\\-', " ", txt, perl=TRUE)
        # Use a consistent space instead of  tab, newline, 
        # vertical tab, form feed, carriage return, space and possibly other locale-dependent characters.
        txt <- gsub("[[:space:]]+", " ", txt, perl=TRUE)
        # Control characters. In ASCII, these characters have octal codes 000 through 037, and 177 (DEL) are changed with space
        txt <- gsub("[[:cntrl:]]+", " ", txt, perl=TRUE)
        
        txt <- gsub("[[:punct:]]+", "", txt, perl=TRUE)   ## Remove punctuation
        
        return(txt)
}

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



NGramUsedToPredict <- function(inputString){

        newString <- trim(inputString)
        theStringVector <- unlist(strsplit(newString, " "))
        lengthStringVector <- length(theStringVector)
        stringOne <- sample(as.character(mostFreqWords), 1)
        
        if(lengthStringVector >= 3){
                
                newString <- paste(tail(theStringVector,3)[1],tail(theStringVector,3)[2],tail(theStringVector,3)[3], sep=" ")
                text_searched <- paste0("^", newString, " ")
                reduced_table <- quadDT[Wordset %like% text_searched][order(-Pmle_MKV_Order_3)]
                theNGramUsed <- as.character(reduced_table$Wordset[1])
                
                if(is.na(theNGramUsed)){
                        newString <- paste(tail(theStringVector,2)[1],tail(theStringVector,2)[2], sep=" ")
                        text_searched <- paste0("^", newString, " ")
                        reduced_table <- triDT[Wordset %like% text_searched][order(-Pmle_MKV_Order_2)]
                        theNGramUsed <- as.character(reduced_table$Wordset[1])
                        
                        if(is.na(theNGramUsed)){
                                newString <- tail(theStringVector,1)
                                text_searched <- paste0("^", newString, " ")
                                reduced_table <- biDT[Wordset %like% text_searched][order(-Pmle_MKV_Order_1)]
                                theNGramUsed <- as.character(reduced_table$Wordset[1])
                                theNGramUsed <- ifelse(!is.na(theNGramUsed),theNGramUsed,c("<NGRAN-NA>"))
                        }       
                }        
        }else if(lengthStringVector == 2){
                newString <- paste(tail(theStringVector,2)[1],tail(theStringVector,2)[2], sep=" ")
                text_searched <- paste0("^", newString, " ")
                reduced_table <- triDT[Wordset %like% text_searched][order(-Pmle_MKV_Order_2)]
                theNGramUsed <- as.character(reduced_table$Wordset[1])    
                
                if(is.na(theNGramUsed)){
                        newString <- tail(theStringVector,1)
                        text_searched <- paste0("^", newString, " ")
                        reduced_table <- biDT[Wordset %like% text_searched][order(-Pmle_MKV_Order_1)]
                        theNGramUsed <- as.character(reduced_table$Wordset[1])
                        theNGramUsed <- ifelse(!is.na(theNGramUsed),theNGramUsed,c("<NGRAN-NA>"))
                }        
        }else if(lengthStringVector == 1){
                text_searched <- paste0("^", newString, " ")
                reduced_table <- biDT[Wordset %like% text_searched][order(-Pmle_MKV_Order_1)]
                theNGramUsed <- as.character(reduced_table$Wordset[1])
                theNGramUsed <- ifelse(!is.na(theNGramUsed),theNGramUsed,c("<NGRAN-NA>"))
        }else{ theNGramUsed  <- c("<NGRAN-NA>")  }
        
        return(theNGramUsed)
        
}



# mostProbableWord("eyes on the ")
# mostProbableWord("eyes on the side")
# mostProbableWord("lm nv icc")
# mostProbableWord(" i'm the one")
# mostProbableWord("I'll be  what we do in a life echoes an ethernety i")

# NGramUsedToPredict("eyes on the ")
# NGramUsedToPredict("eyes on the side")
# NGramUsedToPredict("lm nv icc")
# NGramUsedToPredict(" i'm the one")
# NGramUsedToPredict("I'll be  what we do in a life echoes an ethernety i")

