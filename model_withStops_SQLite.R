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


# set up the directory for work on this project
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone")

# read the whole datasets
blogs <- readLines("final/en_US/en_US.blogs.txt")
news <- scan("final/en_US/en_US.news.txt", character(0), sep = "\n")
tweets <- readLines("final/en_US/en_US.twitter.txt")


blogs_sample <- sample(blogs, 75000)
rm(blogs)
news_sample <- sample(news, 75000)
rm(news)
tweets_sample <- sample(tweets, 225000)
rm(tweets)

# Let us combine all of the sample data into a single dataset
allSamples <- c(blogs_sample, news_sample, tweets_sample)
rm(blogs_sample)
rm(news_sample)
rm(tweets_sample)
gc()

nonAscIDX<- grep("allSamples", iconv(allSamples, "latin1", "ASCII", sub="allSamples"))

# subset original vector of words to exclude words with non-ACCII characters
allSamples<- allSamples[ - nonAscIDX]

writeLines(allSamples, "sample/sampleData.txt")

allSamples <- readLines("sample/sampleData.txt")

allSamples <- sample(allSamples, 200000)
writeLines(allSamples, "sample/sampleData.txt")


# now it is possible to create a Corpus for further work
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/sample")
cname <- getwd()
docs <- VCorpus(DirSource(cname))

# perform most of the cleaning of the dataset
replSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, replSpace, "/")
docs <- tm_map(docs, replSpace, "@")
docs <- tm_map(docs, replSpace, "\\|")
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)

# remove profane words
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone")
profanityWords <-read.csv("swearWords.txt", header = F)
profanityWords <- profanityWords$V1

docs <- tm_map(docs, removeWords, profanityWords)
docs <- tm_map(docs, stripWhitespace)

# Tokenization
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
uniGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = oneGram)), 0.98)
freq1 <- getFreq(uniGramTDM)
rm(uniGramTDM)

# freq1 <- freq1[freq1$freq >= 2,]

biGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = biGram)), 0.97)
freq2 <- getFreq(biGramTDM)
rm(biGramTDM)

# freq2 <- freq2[freq2$freq >= 2,]

triGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = triGram)), 0.97)
freq3 <- getFreq(triGramTDM)
rm(triGramTDM)

# freq3 <- freq3[freq3$freq >= 2,]

quadGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = quadGram)), 0.97)
freq4 <- getFreq(quadGramTDM)
rm(quadGramTDM)

# freq4 <- freq4[freq4$freq >1,]

# write the tables to files
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone")
save(freq1, file="withStops/freq1.RData")
save(freq2, file="withStops/freq2.RData")
save(freq3, file="withStops/freq3.RData")
save(freq4, file="withStops/freq4.RData")


load("withStops/freq1.RData")
load("withStops/freq2.RData")
load("withStops/freq3.RData")
load("withStops/freq4.RData")

# change the data for higher N-grams to make them suitable for the stupid back-off
freq2$unigram <- unlist(lapply(strsplit(freq2$word, " "), function(x){x[1]}))
freq3$bigram <- unlist(lapply(strsplit(freq3$word, " "), function(x){paste(x[1], x[2], sep = " ")}))
freq4$trigram <- unlist(lapply(strsplit(freq4$word, " "), function(x){paste(x[1], x[2], x[3], sep = " ")}))

save(freq2, file="withStops/freq2.RData")
save(freq3, file="withStops/freq3.RData")
save(freq4, file="withStops/freq4.RData")

library(RSQLite)
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/merged")
db_file <- "NGrams2.sqlite"

# Open db connection.
db <- dbConnect(SQLite(), dbname=db_file)
dbListTables(db)

# write the data from data frames into databases
dbWriteTable(conn = db, name = "Unigrams", value = as.data.frame(freq1), row.names = FALSE)
dbWriteTable(conn = db, name = "Bigrams", value = as.data.frame(freq2), row.names = FALSE)
dbWriteTable(conn = db, name = "Trigrams", value = as.data.frame(freq3), row.names = FALSE)
dbWriteTable(conn = db, name = "Quadgrams", value = as.data.frame(freq4), row.names = FALSE)

dbDisconnect(db)

db <- dbConnect(SQLite(), dbname=db_file)



# define a cleaning function 

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


predictNextWord <-function(input, db, maxResults = 5) {

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



# perform predictions
checks <- rbind(
  c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", "beer", "cheese", "pretzels", "soda"), 
  c( "You're the reason why I smile everyday. Can you follow me please? It would mean the", "world", "most", "universe", "best"), 
  c( "Hey sunshine, can you follow me and make me the", "smelliest", "saddest", "bluest", "happiest"), 
  c( "Very early observations on the Bills game: Offense still struggling but the", "referees", "players", "defense", "crowd"), 
  c( "Go on a romantic date at the", "movies", "mall", "grocery", "beach"), 
  c( "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", "motorcycle", "way", "phone", "horse"), 
  c( "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", "time", "weeks", "thing", "years"), 
  c( "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", "eyes", "fingers", "toes", "ears"), 
  c( "Be grateful for the good times and keep the faith during the", "sad", "bad", "hard", "worse"), 
  c( "If this isn't the cutest thing you've ever seen, then you must be", "callous", "insane", "insensitive", "asleep"),
  c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd","die","give","sleep","eat"),
  c("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his","horticultural","spiritual","financial","marital"),
  c("I'd give anything to see arctic monkeys this","month","morning","weekend","decade"),
  c("Talking to your mom has the same effect as a hug and helps reduce your","sleepiness","happiness","stress","hunger"),
  c("When you were in Holland you were like 1 inch away from me but you hadn't time to take a","look","minute","picture","walk"),
  c("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the","incident","case","account","matter"),
  c("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each","finger","hand","arm","toe"),
  c("Every inch of you is perfect from the bottom to the","center","middle","top","side"),
  c("I'm thankful my childhood was filled with imagination and bruises from playing","outside","daily","inside","weekly"),
  c("I like how the same people are in almost all of Adam Sandler's","pictures","novels","movies","stories")
)


predictNextWord(checks[2,1], db)
predictNextWord(checks[3,1], db)
predictNextWord(checks[4,1], db)
predictNextWord(checks[5,1], db)
predictNextWord(checks[6,1], db)
predictNextWord(checks[7,1], db)
predictNextWord(checks[8,1], db)

# quiz 3
text1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
text2 <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
text3 <- "I'd give anything to see arctic monkeys this"
text4 <- "Talking to your mom has the same effect as a hug and helps reduce your"
text5 <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
text6 <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
text7 <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
text8 <- "Every inch of you is perfect from the bottom to the"
text9 <- "I'm thankful my childhood was filled with imagination and bruises from playing"
text10 <- "I like how the same people are in almost all of Adam Sandler's"

predictNextWord(text1, db)
predictNextWord(text2, db)
predictNextWord(text3, db)
predictNextWord(text4, db)
predictNextWord(text5, db)
predictNextWord(text6, db)
predictNextWord(text7, db)
predictNextWord(text8, db)

predictNextWord("this time next", db)


setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone")
load("withStops/freq1.RData")
load("withStops/freq2.RData")
load("withStops/freq3.RData")
load("withStops/freq4.RData")
