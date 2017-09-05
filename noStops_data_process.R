# the purpose of this script is to generate a large (600000 lines) database 
# from existing sets of text data after removing the stop words.

# load essential libraries
library(RWeka)
library(tm) 
library(stringi)
library(dplyr)
library(stringr)
library(qdap)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

cleanInput <- function(inputText, profanityWords) {
  
  doc <- VCorpus(VectorSource(inputText))
  
  doc <- tm_map(doc, removeNumbers) 
  doc <- tm_map(doc, content_transformer(tolower))
  doc <- tm_map(doc, removePunctuation)
  doc <- tm_map(doc, removeWords, profanityWords)
  doc <- tm_map(doc, stripWhitespace) 
  
  
  outputText <- as.character(doc[[1]])
  
  if (nchar(outputText) > 0) {
    return(outputText) 
  } else {
    return("")
  }
}


predictNextWord3 <-function(input, maxResults = 5) {
  
  # clean input and verify that the input is still valid
  input <- str_trim(input,  side= "both")
  
  input <- cleanInput(input, profanityWords)
  
  if(input == ''|input == "na na") return('Warning: Just input something')
  
  # figure out how many words there are in the input
  wordList <- strsplit(input, split=" ")[[1]]
  numWords <- length(wordList)
  
  # Case 1: there are 3 or more words in the input
  # Since our maximum N-gram is 4-gram, then we can match the trigrams against 4-grams and figure out 
  # which match and so on.
  
  # find a subset of four-gram dataset which matches a trigram derived from our input 
  input <- paste(wordList[numWords-2], wordList[numWords-1], wordList[numWords], sep = ' ')
  input <- str_trim(input,  side= "both")
  
  sub4 <- sqldf(paste("SELECT * FROM Quadgrams WHERE trigram LIKE '", input, "'", sep = "", collapse = NULL), 
                dbname ="NGrams3.sqlite")
  
  # find a subset of trigram dataset which matches a bigram derived from our input 
  input2 <- paste(wordList[numWords-1], wordList[numWords], sep = " ")
  input2 <- str_trim(input2,  side= "both")
  
  sub3 <- sqldf(paste("SELECT * FROM Trigrams WHERE bigram LIKE '", input2, "'", sep = "", collapse = NULL), 
                dbname ="NGrams3.sqlite")
  
  
  # find a subset of bigram dataset which matches a unigram derived from our input
  input3 <- wordList[numWords]
  
  sub2 <- sqldf(paste("SELECT * FROM Bigrams WHERE unigram LIKE '", input3, "'", sep = "", collapse = NULL), 
                dbname ="NGrams3.sqlite")
  
  # define scores for the unigram data
  unigrams <- sqldf("SELECT * FROM Unigrams", dbname ="NGrams3.sqlite")
  unigrams$s <- unigrams$freq/nrow(unigrams)*0.16
  
  
  if(nrow(sub4) == 0) {
    if(nrow(sub3) ==0){
      if(nrow(sub2) == 0){
        
        # select top 5 unigrams 
        useuni <- unigrams[order(unigrams$s,decreasing = T),]
        return(useuni[1:5,]$word)
        
      } else{ # the last word matched some bigrams
        
        # get 1-gram data
        input1gram <- sqldf(paste("SELECT * FROM Unigrams WHERE word LIKE '", input3, "'", sep = "", collapse = NULL), 
                            dbname ="NGrams3.sqlite")
        
        if(nrow(input1gram) > 0){
          sub2$s <- 0.4*0.4*sub2$freq/input1gram$freq
        } else{
          sub2$s <- 0.4*0.4*0.4*sub2$freq
        }
        
        # define vectors with the results
        names <- c(word(sub2$word, -1))
        score <- c(sub2$s)
        
      }
    } else { # a bigram from the input matched entry/entries in a 3-gram table
      
      # define the scores for the sub3 hits, which are a subset of freq3, which matched a bigram from the input
      input2gram <- sqldf(paste("SELECT * FROM Bigrams WHERE word LIKE '", input2, "'", sep = "", collapse = NULL), 
                          dbname ="NGrams3.sqlite")
      if(nrow(input2gram) > 0){
        sub3$s <- 0.4*sub3$freq/input2gram$freq
      } else {
        sub3$s <- 0.4*0.4*sub3$freq
      }
      
      # obtain data for the sub2 dataset if there is not enough data already 
      if(nrow(sub3) < maxResults){
        
        input1gram <- sqldf(paste("SELECT * FROM Unigrams WHERE word LIKE '", input3, "'", sep = "", collapse = NULL), 
                            dbname ="NGrams3.sqlite")
        
        if(nrow(input1gram) > 0){
          sub2$s <- 0.4*0.4*sub2$freq/input1gram$freq
        } else{
          sub2$s <- 0.4*0.4*0.4*sub2$freq
        }
        
        # define vectors with the results
        names <- c(word(sub3$word, -1), word(sub2$word, -1))
        score <- c(sub3$s, sub2$s)        
        
      }  else {
        
        # define vectors with the results        
        names <- c(word(sub3$word, -1))
        score <- c(sub3$s)        
        
      }  
      
    }
    
  } else { # a trigram from the input matched entry/entries in a 4-gram table 
    # logically, at least one of trigrams, bigrams and unigrams will exist
    
    # define scores for the sub4 hits, which are a subset of freq4, which matched a trigram from the input
    input3gram <- sqldf(paste("SELECT * FROM Trigrams WHERE word LIKE '", input, "'", sep = "", collapse = NULL), 
                        dbname ="NGrams3.sqlite")
    if(nrow(input3gram) > 0){
      sub4$s <- sub4$freq/input3gram$freq      
    } else{
      sub4$s <- 0.4*sub4$freq
    }
    
    names <- c(word(sub4$word, -1))
    score <- c(sub4$s)
    
    # define the scores for the sub3 hits, which are a subset of freq3, which matched a bigram from the input
    if(nrow(sub4) < maxResults){
      input2gram <- sqldf(paste("SELECT * FROM Bigrams WHERE word LIKE '", input2, "'", sep = "", collapse = NULL), 
                          dbname ="NGrams3.sqlite")
      
      if(nrow(input2gram) > 0){
        sub3$s <- 0.4*sub3$freq/input2gram$freq
      } else {
        sub3$s <- 0.4*0.4*sub3$freq
      }
      
      names <- c(names, word(sub3$word, -1))
      score <- c(score, sub3$s)
    }
    
    # process the next level
    if( (nrow(sub4) + nrow(sub3)) < maxResults){    
      input1gram <- sqldf(paste("SELECT * FROM Unigrams WHERE word LIKE '", input3, "'", sep = "", collapse = NULL), 
                          dbname ="NGrams3.sqlite")
      
      if(nrow(input1gram) > 0){
        sub2$s <- 0.4*0.4*sub2$freq/input1gram$freq
      } else{
        sub2$s <- 0.4*0.4*0.4*sub2$freq
      }
      
      # define vectors with the results
      names <- c(names, word(sub2$word, -1))
      score <- c(score, sub2$s)
    }
  }
  
  
  
  predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
  predictWord <- predictWord[order(predictWord$score,decreasing = T),]
  
  # in case replicated
  final <- unique(predictWord$next_word)
  
  return(final[1:maxResults])
  
}

setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone")
profanityWords <-read.csv("swearWords.txt", header = F)
profanityWords <- profanityWords$V1


# make a corpus from the set1 of text data
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/set1")
cname <- getwd()
docs <- VCorpus(DirSource(cname))


# perform most of the cleaning of the dataset
replSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

processDocs <- function(docs, profanityWords){
  docs <- tm_map(docs, replSpace, "/")
  docs <- tm_map(docs, replSpace, "@")
  docs <- tm_map(docs, replSpace, "\\|")
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeWords, stopwords("en"))
  docs <- tm_map(docs, removeWords, profanityWords)
  docs <- tm_map(docs, stripWhitespace)
  docs
}

docs <- processDocs(docs, profanityWords)

# 1. Make frequency tables for the first set of samples.
oneGram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
biGram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
triGram<- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quadGram <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

library(data.table)

getFreq <- function(tdm) {
  freq <- sort(rowSums(as.matrix(tdm), na.rm = T), decreasing = TRUE)
  return(data.table(word = names(freq), freq = freq))
}

# generate matrices of terms
uniGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = oneGram)), 0.97)
freq1 <- getFreq(uniGramTDM)
rm(uniGramTDM)

biGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = biGram)), 0.97)
freq2 <- getFreq(biGramTDM)
rm(biGramTDM)

triGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = triGram)), 0.97)
freq3 <- getFreq(triGramTDM)
rm(triGramTDM)

quadGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = quadGram)), 0.97)
freq4 <- getFreq(quadGramTDM)
rm(quadGramTDM)

# read the 2nd set of data
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/set2")
cname <- getwd()
docs <- VCorpus(DirSource(cname))

docs <- processDocs(docs, profanityWords)

uniGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = oneGram)), 0.97)
freq1_2 <- getFreq(uniGramTDM)
rm(uniGramTDM)

biGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = biGram)), 0.97)
freq2_2 <- getFreq(biGramTDM)
rm(biGramTDM)

triGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = triGram)), 0.97)
freq3_2 <- getFreq(triGramTDM)
rm(triGramTDM)

quadGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = quadGram)), 0.97)
freq4_2 <- getFreq(quadGramTDM)
rm(quadGramTDM)


freq1_merge <- merge(freq1, freq1_2, by = c("word"), all.x = TRUE, all.y = TRUE)
freq1_merge[is.na(freq1_merge$freq.x),]$freq.x <- 0
freq1_merge[is.na(freq1_merge$freq.y),]$freq.y <- 0
freq1_merge$freq <- freq1_merge$freq.x + freq1_merge$freq.y
freq1_merge<- freq1_merge[,c(1,4)]
freq1_merge <- freq1_merge[order(-freq1_merge$freq)]

rm(freq1)
rm(freq1_2)

freq2_merge <- merge(freq2, freq2_2, by = c("word"), all.x = TRUE, all.y = TRUE)
freq2_merge[is.na(freq2_merge$freq.x),]$freq.x <- 0
freq2_merge[is.na(freq2_merge$freq.y),]$freq.y <- 0
freq2_merge$freq <- freq2_merge$freq.x + freq2_merge$freq.y
freq2_merge<- freq2_merge[,c(1,4)]
freq2_merge <- freq2_merge[order(-freq2_merge$freq)]

rm(freq2)
rm(freq2_2)


freq3_merge <- merge(freq3, freq3_2, by = c("word"), all.x = TRUE, all.y = TRUE)
freq3_merge[is.na(freq3_merge$freq.x),]$freq.x <- 0
freq3_merge[is.na(freq3_merge$freq.y),]$freq.y <- 0
freq3_merge$freq <- freq3_merge$freq.x + freq3_merge$freq.y
freq3_merge<- freq3_merge[,c(1,4)]
freq3_merge <- freq3_merge[order(-freq3_merge$freq)]

rm(freq3)
rm(freq3_2)

freq4_merge <- merge(freq4, freq4_2, by = c("word"), all.x = TRUE, all.y = TRUE)
freq4_merge[is.na(freq4_merge$freq.x),]$freq.x <- 0
freq4_merge[is.na(freq4_merge$freq.y),]$freq.y <- 0
freq4_merge$freq <- freq4_merge$freq.x + freq4_merge$freq.y
freq4_merge<- freq4_merge[,c(1,4)]
freq4_merge <- freq4_merge[order(-freq4_merge$freq)]

rm(freq4)
rm(freq4_2)

# read set 3 data
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/set3")
cname <- getwd()
docs <- VCorpus(DirSource(cname))

docs <- processDocs(docs, profanityWords)

# generate matrices of terms
uniGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = oneGram)), 0.97)
freq1_3 <- getFreq(uniGramTDM)
rm(uniGramTDM)


freq1_merge <- merge(freq1_merge, freq1_3, by = c("word"), all.x = TRUE, all.y = TRUE)
freq1_merge[is.na(freq1_merge$freq.x),]$freq.x <- 0
freq1_merge[is.na(freq1_merge$freq.y),]$freq.y <- 0
freq1_merge$freq <- freq1_merge$freq.x + freq1_merge$freq.y
freq1_merge<- freq1_merge[,c(1,4)]
freq1_merge <- freq1_merge[order(-freq1_merge$freq)]

freq1_merge <- freq1_merge[freq1_merge$freq >= 2,]
rm(freq1_3)


biGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = biGram)), 0.97)
freq2_3 <- getFreq(biGramTDM)
rm(biGramTDM)

freq2_merge <- merge(freq2_merge, freq2_3, by = c("word"), all.x = TRUE, all.y = TRUE)
freq2_merge[is.na(freq2_merge$freq.x),]$freq.x <- 0
freq2_merge[is.na(freq2_merge$freq.y),]$freq.y <- 0
freq2_merge$freq <- freq2_merge$freq.x + freq2_merge$freq.y
freq2_merge<- freq2_merge[,c(1,4)]
freq2_merge <- freq2_merge[order(-freq2_merge$freq)]

freq2_merge <- freq2_merge[freq2_merge$freq >= 2,]

rm(freq2_3)


triGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = triGram)), 0.97)
freq3_3 <- getFreq(triGramTDM)
rm(triGramTDM)

freq3_merge <- merge(freq3_merge, freq3_3, by = c("word"), all.x = TRUE, all.y = TRUE)
freq3_merge[is.na(freq3_merge$freq.x),]$freq.x <- 0
freq3_merge[is.na(freq3_merge$freq.y),]$freq.y <- 0
freq3_merge$freq <- freq3_merge$freq.x + freq3_merge$freq.y
freq3_merge<- freq3_merge[,c(1,4)]
freq3_merge <- freq3_merge[order(-freq3_merge$freq)]

freq3_merge <- freq3_merge[freq3_merge$freq >= 2,]

rm(freq3_3)


quadGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = quadGram)), 0.97)
freq4_3 <- getFreq(quadGramTDM)
rm(quadGramTDM)

freq4_merge <- merge(freq4_merge, freq4_3, by = c("word"), all.x = TRUE, all.y = TRUE)
freq4_merge[is.na(freq4_merge$freq.x),]$freq.x <- 0
freq4_merge[is.na(freq4_merge$freq.y),]$freq.y <- 0
freq4_merge$freq <- freq4_merge$freq.x + freq4_merge$freq.y
freq4_merge<- freq4_merge[,c(1,4)]
freq4_merge <- freq4_merge[order(-freq4_merge$freq)]

freq4_merge <- freq4_merge[freq4_merge$freq >= 2,]

rm(freq4_3)


freq2_merge$unigram <- unlist(lapply(strsplit(freq2_merge$word, " "), function(x){x[1]}))
freq3_merge$bigram <- unlist(lapply(strsplit(freq3_merge$word, " "), function(x){paste(x[1], x[2], sep = " ")}))
freq4_merge$trigram <- unlist(lapply(strsplit(freq4_merge$word, " "), function(x){paste(x[1], x[2], x[3], sep = " ")}))

setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/noStops")
save(freq1_merge, file="freq1_merge.RData")
save(freq2_merge, file="freq2_merge.RData")
save(freq3_merge, file="freq3_merge.RData")
save(freq4_merge, file="freq4_merge.RData")


library(RSQLite)
db_file <- "NGrams3.sqlite"

# Open db connection.
db <- dbConnect(SQLite(), dbname=db_file)

# write the data from data frames into databases
dbWriteTable(conn = db, name = "Unigrams", value = as.data.frame(freq1_merge), row.names = FALSE)
dbWriteTable(conn = db, name = "Bigrams", value = as.data.frame(freq2_merge), row.names = FALSE)
dbWriteTable(conn = db, name = "Trigrams", value = as.data.frame(freq3_merge), row.names = FALSE)
dbWriteTable(conn = db, name = "Quadgrams", value = as.data.frame(freq4_merge), row.names = FALSE)

dbDisconnect(db)

db <- dbConnect(SQLite(), dbname=db_file)

# testing the accuracy of the model

# read data for creating a corpus from the test data
# the test data was separated from the training set

setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/test")
cname <- getwd()
docs <- VCorpus(DirSource(cname))

# perform most of the cleaning of the dataset
docs <- processDocs(docs, profanityWords)

quadGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = quadGram)), 0.97)
freq4_test <- getFreq(quadGramTDM)
rm(quadGramTDM)

# make trigrams for the four-Gram set
freq4_test$trigram <- unlist(lapply(strsplit(freq4_test$word, " "), function(x){paste(x[1], x[2], x[3], sep = " ")}))
# get the last word in each of the 4-grams
lastWords <- word(freq4_test$word, -1)


# test accuracy of the smaller initial database
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/noStops")

correctPredicted <- 0

for(j in 1:length(freq4_test$word)){
  
  # generate predictions
  pred <- predictNextWord3(freq4_test[j,]$trigram, 5)
  
  if(lastWords[j] %in% pred) {
    correctPredicted = correctPredicted + 1
    cat(correctPredicted, " ", lastWords[j],"\n")
  }
  
}

percentCorrect <- correctPredicted/j

# > percentCorrect <- correctPredicted/j
# > percentCorrect
# [1] 0.09522693



