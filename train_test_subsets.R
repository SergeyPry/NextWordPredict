# the purpose of this script is to generate training and test subsets of the text datasets 
# for predicting the next word

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



# set up the directory for work on this project
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone")

# read the whole datasets
blogs <- readLines("final/en_US/en_US.blogs.txt")
news <- scan("final/en_US/en_US.news.txt", character(0), sep = "\n")
tweets <- readLines("final/en_US/en_US.twitter.txt")

# sample 200 blog lines, 200 news lines and 600 tweets

blog_ind <- sample(1:length(blogs), 200)
test_blogs <- blogs[blog_ind]
blogs <- blogs[-blog_ind]
length(blogs)

news_ind <- sample(1:length(news), 200)
test_news <- news[news_ind]
news <- news[-news_ind]
length(news)

tweet_ind <- sample(1:length(tweets), 200)
test_tweets <- tweets[tweet_ind]
tweets <- tweets[-tweet_ind]
length(tweets)


tests <- c(test_blogs, test_news, test_tweets)
writeLines(tests, "test/testSample.txt")

# generate three sets of samples from blogs, news and tweets
blogs_samp1_ind <- sample(1:length(blogs), 40000)
blogs_samp1 <- blogs[blogs_samp1_ind]
blogs <- blogs[-blogs_samp1_ind]

blogs_samp2_ind <- sample(1:length(blogs), 40000)
blogs_samp2 <- blogs[blogs_samp2_ind]
blogs <- blogs[-blogs_samp2_ind]

blogs_samp3_ind <- sample(1:length(blogs), 40000)
blogs_samp3 <- blogs[blogs_samp3_ind]
blogs <- blogs[-blogs_samp3_ind]

#news
news_samp1_ind <- sample(1:length(news), 40000)
news_samp1 <- news[news_samp1_ind]
news <- news[-news_samp1_ind]

news_samp2_ind <- sample(1:length(news), 40000)
news_samp2 <- news[news_samp2_ind]
news <- news[-news_samp2_ind]

news_samp3_ind <- sample(1:length(news), 40000)
news_samp3 <- news[news_samp3_ind]
news <- news[-news_samp3_ind]

# tweets
tweets_samp1_ind <- sample(1:length(tweets), 120000)
tweets_samp1 <- tweets[tweets_samp1_ind]
tweets <- tweets[-tweets_samp1_ind]

tweets_samp2_ind <- sample(1:length(tweets), 120000)
tweets_samp2 <- tweets[tweets_samp2_ind]
tweets <- tweets[-tweets_samp2_ind]

tweets_samp3_ind <- sample(1:length(tweets), 120000)
tweets_samp3 <- tweets[tweets_samp3_ind]
tweets <- tweets[-tweets_samp3_ind]

# remove unneeded data
rm(blogs)
rm(news)
rm(tweets)
rm(blogs_samp1_ind)
rm(blogs_samp2_ind)
rm(blogs_samp3_ind)

rm(news_samp1_ind)
rm(news_samp2_ind)
rm(news_samp3_ind)

rm(tweets_samp1_ind)
rm(tweets_samp2_ind)
rm(tweets_samp3_ind)

rm(test_blogs)
rm(test_news)
rm(test_tweets)

rm(blog_ind)
rm(news_ind)
rm(tweet_ind)

# Algorithm

# 0. Create a corpus for the first set of samples:
# Let us combine all of the sample data into a single dataset
sampleSet1 <- c(blogs_samp1, news_samp1, tweets_samp1)
rm(blogs_samp1)
rm(news_samp1)
rm(tweets_samp1)

sampleSet2 <- c(blogs_samp2, news_samp2, tweets_samp2)
rm(blogs_samp2)
rm(news_samp2)
rm(tweets_samp2)

sampleSet3 <- c(blogs_samp3, news_samp3, tweets_samp3)
rm(blogs_samp3)
rm(news_samp3)
rm(tweets_samp3)

gc()

# create corpus from the first set of samples
nonAscIDX<- grep("sampleSet1", iconv(sampleSet1, "latin1", "ASCII", sub="sampleSet1"))

# subset original vector of words to exclude words with non-ACCII characters
sampleSet1 <- sampleSet1[ - nonAscIDX]
writeLines(sampleSet1, "set1/sampleSet1.txt")

# now it is possible to create a Corpus for further work
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/set1")
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


rm(nonAscIDX)
rm(sampleSet1)

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

# save the data for the first set
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/set1data")
save(freq1, file="freq1.RData")
save(freq2, file="freq2.RData")
save(freq3, file="freq3.RData")
save(freq4, file="freq4.RData")


# 2. Make frequency tables for the second set of samples

# create corpus from the first set of samples
nonAscIDX<- grep("sampleSet2", iconv(sampleSet2, "latin1", "ASCII", sub="sampleSet2"))

# subset original vector of words to exclude words with non-ACCII characters
sampleSet2 <- sampleSet2[ - nonAscIDX]
writeLines(sampleSet2, "set2/sampleSet2.txt")

# now it is possible to create a Corpus for further work
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/set2")
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

rm(nonAscIDX)
rm(sampleSet2)

# generate matrices of terms
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

setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/set2data")
save(freq1_2, file="freq1_2.RData")
save(freq2_2, file="freq2_2.RData")
save(freq3_2, file="freq3_2.RData")
save(freq4_2, file="freq4_2.RData")


# 3. Merge the data from the second set to the first one in such a way 
# that the new words or n-grams are inserted if they do not exist or their count is updated

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


nonAscIDX<- grep("sampleSet3", iconv(sampleSet3, "latin1", "ASCII", sub="sampleSet3"))

# subset original vector of words to exclude words with non-ACCII characters
sampleSet3 <- sampleSet3[ - nonAscIDX]
writeLines(sampleSet3, "set3/sampleSet3.txt")

# now it is possible to create a Corpus for further work
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/set3")
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


docs <- tm_map(docs, removeWords, profanityWords)
docs <- tm_map(docs, stripWhitespace)

rm(nonAscIDX)
rm(sampleSet3)


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

setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/set3data")
save(freq1_3, file="freq1_3.RData")
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

save(freq2_3, file="freq2_3.RData")
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

save(freq3_3, file="freq3_3.RData")
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

save(freq4_3, file="freq4_3.RData")
rm(freq4_3)


freq2_merge$unigram <- unlist(lapply(strsplit(freq2_merge$word, " "), function(x){x[1]}))
freq3_merge$bigram <- unlist(lapply(strsplit(freq3_merge$word, " "), function(x){paste(x[1], x[2], sep = " ")}))
freq4_merge$trigram <- unlist(lapply(strsplit(freq4_merge$word, " "), function(x){paste(x[1], x[2], x[3], sep = " ")}))

setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/merged")
save(freq1_merge, file="freq1_merge.RData")
save(freq2_merge, file="freq2_merge.RData")
save(freq3_merge, file="freq3_merge.RData")
save(freq4_merge, file="freq4_merge.RData")

load("freq1_merge.RData")
load("freq2_merge.RData")
load("freq3_merge.RData")
load("freq4_merge.RData")

library(RSQLite)
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/merged")
db_file <- "NGrams.sqlite"

# Open db connection.
db <- dbConnect(SQLite(), dbname=db_file)

# write the data from data frames into databases
dbWriteTable(conn = db, name = "Unigrams", value = as.data.frame(freq1_merge), row.names = FALSE)
dbWriteTable(conn = db, name = "Bigrams", value = as.data.frame(freq2_merge), row.names = FALSE)
dbWriteTable(conn = db, name = "Trigrams", value = as.data.frame(freq3_merge), row.names = FALSE)
dbWriteTable(conn = db, name = "Quadgrams", value = as.data.frame(freq4_merge), row.names = FALSE)

dbDisconnect(db)

db <- dbConnect(SQLite(), dbname=db_file)

# remove profane words
setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone")
profanityWords <-read.csv("swearWords.txt", header = F)
profanityWords <- profanityWords$V1


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

library(sqldf)
predictNextWord(text1, db)
predictNextWord(text2, db)
predictNextWord(text3, db)
predictNextWord(text4, db)
predictNextWord(text5, db)
predictNextWord(text6, db)
predictNextWord(text7, db)
predictNextWord(text8, db)
predictNextWord(text9, db)
predictNextWord(text10, db)
predictNextWord("this time next", db)

# testing the accuracy of the model

# read data for creating a corpus from the test data
# the test data was separated from the training set

setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/test")
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
docs <- tm_map(docs, removeWords, profanityWords)
docs <- tm_map(docs, stripWhitespace)

quadGramTDM <- removeSparseTerms(TermDocumentMatrix(docs, control = list(tokenize = quadGram)), 0.97)
freq4_test <- getFreq(quadGramTDM)
rm(quadGramTDM)

# make trigrams for the four-Gram set
freq4_test$trigram <- unlist(lapply(strsplit(freq4_test$word, " "), function(x){paste(x[1], x[2], x[3], sep = " ")}))
# get the last word in each of the 4-grams
lastWords <- word(freq4_test$word, -1)


# test accuracy of the smaller initial database
correctPredicted <- 0

for(j in 1:length(freq4_test$word)){
  
  # generate predictions
  pred <- predictNextWord(freq4_test[j,]$trigram, db)
  
  if(lastWords[j] %in% pred) {
    correctPredicted = correctPredicted + 1
    cat(correctPredicted, " ", lastWords[j],"\n")
    }
  
}

percentCorrect <- correctPredicted/j

# The result below is for the smaller database NGrams.sqlite
# > percentCorrect <- correctPredicted/j
# percentCorrect
#[1] 0.2844252


setwd("c:/Coursera&MOOCS/00_Data_Science_Specialization/Capstone/merged")


# test accuracy of the larger initial database
correctPredicted <- 0

for(i in 1:length(freq4_test$word)){
  
  # generate predictions
  pred <- predictNextWord(freq4_test[i,]$trigram, 5)
  
  if(lastWords[i] %in% pred) {
    correctPredicted = correctPredicted + 1
    cat(correctPredicted, " ", lastWords[i],"\n")
  }
  
}

percentCorrect <- correctPredicted/i
percentCorrect

# > percentCorrect <- correctPredicted/i
# > percentCorrect
# [1] 0.3003068
