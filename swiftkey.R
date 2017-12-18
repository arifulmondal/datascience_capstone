#swiftkey.R
#-------------------------------------------

## Load Packages
library("tm")
library("stringi")
library("wordcloud")
library("clue")
library("ggplot2")
library("RColorBrewer")
library("SnowballC")
library("RWeka")

#------------------------------------------------


setwd("D:\\dscapstone")

# Check if the file has been extracted 
if (!file.exists("./final")) {
  
  if(!file.exists("Swiftkey.zip")){ 
    url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"  
    download.file(url, "Swiftkey.zip", method = "curl")
    dateDownload <- date()
  }
  SwiftKey.zip<- "Coursera-SwiftKey.zip"
  outDir<-"."
  unzip(SwiftKey.zip,exdir=outDir)
}

#------------------------------------------------


# Read files in the R using
us_blogs<-readLines("final\\en_US\\en_US.blogs.txt",  encoding = "UTF-8", skipNul = TRUE)
us_news<-readLines("final\\en_US\\en_US.news.txt",  encoding = "UTF-8", skipNul = TRUE)
us_twitter<-readLines("final\\en_US\\en_US.twitter.txt",  encoding = "UTF-8", skipNul = TRUE)
#------------------------------------------------

# Summary US blogs
summary(us_blogs)
# Structure of the data
str(us_blogs)
us_blogs[3:5]

# Summary US News
summary(us_news)
str(us_news)
us_news[3:5]


# Summary US Twitter
summary(us_twitter)
str(us_twitter)
us_twitter[3:5]


### Longest lines in the files

longest_lines<- (c(max(nchar(us_blogs)), max(nchar(us_news)), max(nchar(us_twitter))))
type <- c("US-Blogs", "US-News", "US-Twitter")
longest_lines <- as.data.frame(cbind(type,longest_lines))
colnames(longest_lines) <- c("file.name", "line.length")
longest_lines$line.length <- as.numeric(longest_lines$line.length)
print(longest_lines)


### Shortest lines in the files

shortest_lines<-c(min(nchar(us_blogs)), min(nchar(us_news)), min(nchar(us_twitter)))
type <- c("US-Blogs", "US-News", "US-Twitter")
shortest_lines<-as.data.frame(cbind(type, shortest_lines))
colnames(shortest_lines) <- c("file.name", "line.length")
print(shortest_lines)

#------------------------------------------------


# Set random seed so that samples do not change
set.seed(12345)

# Sample from US-Blogs
# Help ?sample
us_blogs_sample <- sample(us_blogs, length(us_blogs)*0.01)

# Sample from US-News
us_news_sample <- sample(us_news, length(us_news)*0.10)

# Sample from US Twitter
us_twitter_sample <- sample(us_twitter, length(us_twitter)*0.01)

# Combine all the sample
us_data_sample <- c(us_blogs_sample,us_news_sample,us_twitter_sample)

# remove data to make some space
rm(list=c("type", "us_blogs", "us_news", "us_twitter", "us_blogs_sample", "us_news_sample", "us_twitter_sample"))

# Quick check on data structure
us_data_sample[1:3]
summary(us_data_sample)

#------------------------------------------------

# Remove non-English characters, letters etc.
# Help ?inconv
us_data_sample<-iconv(us_data_sample, "latin1", "ASCII", sub="")

# Remove special characters with spaces
# Help ?gsub
us_data_sample_1 <- gsub("[^0-9a-z]", " ", us_data_sample, ignore.case = TRUE)
rm(us_data_sample)

# Remove duplicate characters
us_data_sample_1 <- gsub('([[:alpha:]])\\1+', '\\1\\1', us_data_sample_1)

# Remove special numbers with spaces
us_data_sample_1 <- gsub("[^a-z]", " ", us_data_sample_1, ignore.case = TRUE)

# Remove multiple spaces to one
us_data_sample_1 <- gsub("\\s+", " ", us_data_sample_1)
us_data_sample_1 <- gsub("^\\s", "", us_data_sample_1)
us_data_sample_1 <- gsub("\\s$", "", us_data_sample_1)
#------------------------------------------------


# Summary
summary(us_data_sample_1)
str(us_data_sample_1)
#------------------------------------------------

# create Corpus
# Help ??VCorpus
myCorpus <- VCorpus(VectorSource(us_data_sample_1))
rm(us_data_sample_1)

#------------------------------------------------

# Transformation of text data

# Help ??tm_map'

# Normalize to small cases
myCorpus <- tm_map(myCorpus, content_transformer(tolower))  

# Remove Stop Words
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))

# Remove Punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

# Remove Numbers 
myCorpus <- tm_map(myCorpus, removeNumbers)

# Create plain text documents
myCorpus <- tm_map(myCorpus, PlainTextDocument)

# Stem words in a text document using Porter's stemming algorithm.
myCorpus <- tm_map(myCorpus, stemDocument, "english")

# Strip White Spaces
myCorpus <- tm_map(myCorpus, stripWhitespace)

#------------------------------------------------


#Most frequently occurred words (uni-gram, bi-gram and tri-gram) are shown in the plot.

## Most frequent terms:
#findMostFreqTerms(dtm)

#Unigram:

uni_token <- function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))}
uni_tdm <- TermDocumentMatrix(myCorpus, control = list(tokenize = uni_token))
uni_tdm <- removeSparseTerms(uni_tdm, 0.95)
uni_corpus <- findFreqTerms(uni_tdm,lowfreq = 20)
uni_corpus_freq <- rowSums(as.matrix(uni_tdm[uni_corpus,]))
uni_corpus_freq <- data.frame(word=names(uni_corpus_freq), frequency=uni_corpus_freq)
df1<- uni_corpus_freq[order(-uni_corpus_freq$frequency),][1:20,]


### Unigram: Most Frequently Occured Words 

uni_corpus[1:100]   #Top 100 frequenty occured words are..

#We have built a word cloud using top 50 frequent words from sample data.
wordcloud(words = uni_corpus_freq$word, freq = uni_corpus_freq$frequency, min.freq = 1,
          max.words=50, random.order=TRUE, rot.per=0.75,
          colors=brewer.pal(8, "Dark2"), c(5,.5), vfont=c("script","plain"))

barplot(df1[1:20,]$freq, las = 2, names.arg = df1[1:20,]$word,
        col =df1[1:20,]$freq, main ="",
        ylab = "Word frequencies", cex.axis=.5, cex = .5, cex.lab=0.75, cex.main=.75)



#------------------------------------------------
#Bigram:

bi_token <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
bi_tdm <- TermDocumentMatrix(myCorpus, control = list(tokenize = bi_token))
bi_corpus <- findFreqTerms(bi_tdm,lowfreq = 10)
bi_corpus_freq <- rowSums(as.matrix(bi_tdm[bi_corpus,]))
bi_corpus_freq <- data.frame(word=names(bi_corpus_freq), frequency=bi_corpus_freq)
df2 <- bi_corpus_freq[order(-bi_corpus_freq$frequency),][1:20,] 


### Bigram: Most Frequently occured "sequence of two adjacent elements"

wordcloud(words = bi_corpus_freq$word, freq = bi_corpus_freq$frequency, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.75,
          colors=brewer.pal(8, "Dark2"), c(5,.5), vfont=c("script","plain"))

ggplot(df2, aes(x = df2$word, y = frequency)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = " ") +
  xlab("Words") +
  ylab("Count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#------------------------------------------------
#Trigram:

tri_token <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}
tri_tdm <- TermDocumentMatrix(myCorpus, control = list(tokenize = tri_token))
tri_corpus <- findFreqTerms(tri_tdm,lowfreq = 5)
tri_corpus_freq <- rowSums(as.matrix(tri_tdm[tri_corpus,]))
tri_corpus_freq <- data.frame(word=names(tri_corpus_freq), frequency=tri_corpus_freq)
df3<-tri_corpus_freq[order(-tri_corpus_freq$frequency),][1:20,] 


## Trigram: Most Frequently occured "a group of three consecutive written units such as letters, syllables, or words"

wordcloud(words = tri_corpus_freq$word, freq = tri_corpus_freq$frequency, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.60,
          colors=brewer.pal(8, "Dark2"), c(5,.5), vfont=c("script","plain"))


ggplot(df3, aes(x = df3$word, y = frequency)) +
  geom_bar(stat = "identity", fill = "#FF6666") +
  labs(title = " ") +
  xlab("Words") +
  ylab("Count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#------------------------------------------------





