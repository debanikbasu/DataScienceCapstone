install.packages("tm")
install.packages("RWeka")
install.packages("ngram")
library(tm)
library(RWeka)
library(ngram)
library(ggplot2)

con <- file("en_US.twitter.txt", "r")
twitter_lines<-readLines(con)
close(con)
con1 <- file("en_US.blogs.txt", "r")
blogs_lines<-readLines(con1)
close(con1)
con2 <- file("en_US.news.txt", "r")
news_lines<-readLines(con2)
close(con2)

sum_data <- as.data.frame(matrix(nrow = 3,ncol=4))
names(sum_data) <- c("source","size(in MB)","lines","words")
sum_data[1,]<-c("twitter",round(file.size("en_US.twitter.txt")/1024^2,2),length(twitter_lines),wordcount(twitter_lines))
sum_data[2,]<-c("blogs",round(file.size("en_US.blogs.txt")/1024^2,2),length(blogs_lines),wordcount(blogs_lines))
sum_data[3,]<-c("news",round(file.size("en_US.news.txt")/1024^2,2),length(news_lines),wordcount(news_lines))
sum_data

set.seed(5000)
twit <- sample(twitter_lines,length(twitter_lines)*0.01,replace=FALSE)
blogs <- sample(blogs_lines,length(blogs_lines)*0.01,replace=FALSE)
news <- sample(news_lines,length(news_lines)*0.01,replace=FALSE)

all_mix <- c(twit,blogs,news)
rm(twit);rm(twitter_lines)
rm(blogs);rm(blogs_lines)
rm(news);rm(news_lines)

corpus<-VCorpus(VectorSource(all_mix))
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
corpus<-tm_map(corpus, removeNumbers)
corpus<-tm_map(corpus, removePunctuation, preserve_intra_word_dashes=TRUE)
corpus<-tm_map(corpus, stripWhitespace)
corpus<-tm_map(corpus, content_transformer(tolower))
badwords <- read.csv('badwords.txt',header=F)
badwordlist <- rep(as.character(badwords[[1]]))
corpus <- tm_map(corpus, removeWords, badwordlist)
#corpus_s <- tm_map(corpus, removeWords, stopwords("en"))

options(mc.cores=1)

uniGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters = " \\r\\n\\t.,;:\"()?!"))
biGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " \\r\\n\\t.,;:\"()?!"))
triGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " \\r\\n\\t.,;:\"()?!"))
quadGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters = " \\r\\n\\t.,;:\"()?!"))

uniGramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = uniGramTokenizer))
biGramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = biGramTokenizer))
triGramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = triGramTokenizer))
quadGramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = quadGramTokenizer))

freqTerms <- findFreqTerms(uniGramMatrix, lowfreq = 20)
termFrequency <- rowSums(as.matrix(uniGramMatrix[freqTerms,]))
unigrams <- data.frame(Text=names(termFrequency), Freq=termFrequency)
unigrams <- unigrams[order(unigrams$Freq,decreasing = TRUE),]

freqTerms <- findFreqTerms(biGramMatrix, lowfreq = 8)
termFrequency <- rowSums(as.matrix(biGramMatrix[freqTerms,]))
bigrams <- data.frame(Text=names(termFrequency), Freq=termFrequency)
bigrams <- bigrams[order(bigrams$Freq,decreasing = TRUE),]

freqTerms <- findFreqTerms(triGramMatrix, lowfreq = 4)
termFrequency <- rowSums(as.matrix(triGramMatrix[freqTerms,]))
trigrams <- data.frame(Text=names(termFrequency), Freq=termFrequency)
trigrams <- trigrams[order(trigrams$Freq,decreasing = TRUE),]

freqTerms <- findFreqTerms(quadGramMatrix, lowfreq = 2)
termFrequency <- rowSums(as.matrix(quadGramMatrix[freqTerms,]))
quadgrams <- data.frame(Text=names(termFrequency), Freq=termFrequency)
quadgrams <- quadgrams[order(quadgrams$Freq,decreasing = TRUE),]

saveRDS(unigrams, file = "myUnigram.RData")
saveRDS(bigrams, file = "myBigram.RData")
saveRDS(trigrams, file = "myTrigram.RData")
saveRDS(quadgrams, file = "myQuadgram.RData")

barplot(unigrams$Freq[1:10],names.arg=unigrams$Text[1:10],las=2)
barplot(bigrams$Freq[1:10],names.arg=bigrams$Text[1:10],las=2)
barplot(trigrams$Freq[1:10],names.arg=trigrams$Text[1:10],las=2)
barplot(quadgrams$Freq[1:10],names.arg=quadgrams$Text[1:10],las=2)

tot_words<-sum(unigrams$Freq)
t50<-0.5*tot_words
t90<-0.9*tot_words

j<-0
i<-1
while(j<t50){
      j<-j+unigrams[i,]$Freq
      i<-i+1
}
words50<-i

while(j<t90){
      j<-j+unigrams[i,]$Freq
      i<-i+1
}
words90<-i