# read in the profanity words
profanityWords <-read.csv("swearWords.txt", header = F)
profanityWords <- profanityWords$V1


# define a function to clean up the input
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


# define the function to do the prediction
predictNextWord <-function(input, maxResults = 5) {
  
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
                dbname ="NGrams.sqlite")
  
  # find a subset of trigram dataset which matches a bigram derived from our input 
  input2 <- paste(wordList[numWords-1], wordList[numWords], sep = " ")
  input2 <- str_trim(input2,  side= "both")
  
  sub3 <- sqldf(paste("SELECT * FROM Trigrams WHERE bigram LIKE '", input2, "'", sep = "", collapse = NULL), 
                dbname ="NGrams.sqlite")
  
  
  # find a subset of bigram dataset which matches a unigram derived from our input
  input3 <- wordList[numWords]
  
  sub2 <- sqldf(paste("SELECT * FROM Bigrams WHERE unigram LIKE '", input3, "'", sep = "", collapse = NULL), 
                dbname ="NGrams.sqlite")
  
  # define scores for the unigram data
  unigrams <- sqldf("SELECT * FROM Unigrams", dbname ="NGrams.sqlite")
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
                            dbname ="NGrams.sqlite")
        
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
                          dbname ="NGrams.sqlite")
      if(nrow(input2gram) > 0){
        sub3$s <- 0.4*sub3$freq/input2gram$freq
      } else {
        sub3$s <- 0.4*0.4*sub3$freq
      }
      
      # obtain data for the sub2 dataset if there is not enough data already 
      if(nrow(sub3) < maxResults){
        
        input1gram <- sqldf(paste("SELECT * FROM Unigrams WHERE word LIKE '", input3, "'", sep = "", collapse = NULL), 
                            dbname ="NGrams.sqlite")
        
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
                        dbname ="NGrams.sqlite")
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
                          dbname ="NGrams.sqlite")
      
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
                          dbname ="NGrams.sqlite")
      
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


# Another version of the prediction function for the larger database
predictNextWord2 <-function(input, maxResults = 5) {
  
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
                dbname ="NGrams2.sqlite")
  
  # find a subset of trigram dataset which matches a bigram derived from our input 
  input2 <- paste(wordList[numWords-1], wordList[numWords], sep = " ")
  input2 <- str_trim(input2,  side= "both")
  
  sub3 <- sqldf(paste("SELECT * FROM Trigrams WHERE bigram LIKE '", input2, "'", sep = "", collapse = NULL), 
                dbname ="NGrams2.sqlite")
  
  
  # find a subset of bigram dataset which matches a unigram derived from our input
  input3 <- wordList[numWords]
  
  sub2 <- sqldf(paste("SELECT * FROM Bigrams WHERE unigram LIKE '", input3, "'", sep = "", collapse = NULL), 
                dbname ="NGrams2.sqlite")
  
  # define scores for the unigram data
  unigrams <- sqldf("SELECT * FROM Unigrams", dbname ="NGrams2.sqlite")
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
                            dbname ="NGrams2.sqlite")
        
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
                          dbname ="NGrams2.sqlite")
      if(nrow(input2gram) > 0){
        sub3$s <- 0.4*sub3$freq/input2gram$freq
      } else {
        sub3$s <- 0.4*0.4*sub3$freq
      }
      
      # obtain data for the sub2 dataset if there is not enough data already 
      if(nrow(sub3) < maxResults){
        
        input1gram <- sqldf(paste("SELECT * FROM Unigrams WHERE word LIKE '", input3, "'", sep = "", collapse = NULL), 
                            dbname ="NGrams2.sqlite")
        
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
                        dbname ="NGrams2.sqlite")
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
                          dbname ="NGrams2.sqlite")
      
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
                          dbname ="NGrams2.sqlite")
      
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

