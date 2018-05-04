#1 Exercise I - API
# --- extract data from Twitter -----------------
rm(list = ls())
install.packages('twitteR')
library(twitteR)
consumer_key <- "riIVarPv7IXVU9VEqPvXYSmGD"
consumer_secret <- "AgqfAEgpOjwuR88zErIFxtM7KoRHL8NBmdC4F07NUjuHK1U910"
access_token <- "495493901-zcbN0OMxipo02uDfWSXofyXYReub3qrKf6a5xKLE"
access_secret <- "oSPJcqFDyAOVsnF6krL7m1OyieoRovrBB3drpRuAeyWmO"

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)
tweets <- userTimeline("NASA", n = 3200)#  retrieve various timelines within the Twitter universe # n<= 3200
tweets.df <- twListToDF(tweets)# convert tweets to a data frame
dim(tweets.df)
for (i in c(1:2, 350)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(tweets.df$text[i], 60))# Write text lines to a connection
}

# --- clean extracted data and build a document-term matrix -----------------


if(!require("tm")){ # check for package existence 
  install.packages("tm")
}
library(tm)
 

# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets.df$text))
# tm_map(corpus, function) is included in tm package, it's an interface to apply transformation functions to corpora.
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))# 
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)# search for matches to argument pattern within each element of a character vector

#---------------------------------- gsub() ------------------------------------
# gsub(pattern, replacement, x)# replace all matches
# Elements of character vectors x which are not substituted will be returned unchanged

txt <- c("The", "licenses", "for", "most", "software", "are",
         "designed", "to", "take", "away", "your", "freedom",
         "to", "share", "and", "change")
gsub("g","#", txt)

# --------------extended regular expressions in R ------------------------------------------------------
gsub("[[:space:]]", "", "a      
     b       c")
gsub(" ", "", "a
     b     c")


# ------------------------------------------------------------------------------
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) # gsub replacement of all matches respectively.
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct)) # tm_map(corpus, function, arguments to function): Interface to apply transformation functions to corpora

# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)# removePunctuation()
# Punctuation characters: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

# Stopwords are commonly used words in the English language such as I, me, my, etc. 
# You can see the full list of stopwords using stopwords('english').
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords("SMART"), "available", "via")# available stopword lists are catalan, romanian, SMART
# list of english stopwords: http://www.ai.mit.edu/projects/jmlr/papers/volume5/lewis04a/a11-smart-stop-list/english.stop
# remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))

# -----------------------------------------
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# all the words are converted to their stem (Ex: learning -> learn, walked -> walk, etc.)
if(!require("SnowballC")){ # check for package existence 
  install.packages("SnowballC")
}
library(SnowballC)
myCorpus <- tm_map(myCorpus, stemDocument)


# Package "hunspell" for English words
# The package provides functions to check whether a word is English word or no
# And also gives suggested words
if(!require("hunspell")){ # check for package existence 
  install.packages("hunspell")
}
library(hunspell)
a <- c("this iss the first sentnece", "this", "the", "vocabulary")

# Check if a single word is English word
hunspell_check(a[2])

# Give suggestion on this wrong word
hunspell_suggest(a[2])

# Give a vector of sentences and find bad words
bad <- hunspell_find(a)

# Each element includes the bad word from each element in the vector. 
print(bad)



#2 Exercise II - Text Mining
# -------------------- find frequent words and associations-------------------------------

# count frequency of "mining"
miningCases <- lapply(myCorpusCopy,
                      function(x) {grep(as.character(x), pattern = "\\<mining")} )
# ----------------- grep()------------
# grep(pattern, x)returns a vector of the indices of the elements of x that yielded a match
grep("[a-z]", letters)
# --------------------- lapply() ---------------------
# lapply(x, function) returns a list of the same length as X, each element of which is the result of applying FUN to the corresponding element of X.
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
lapply(x, mean)
----------------------------------------
  
sum(unlist(miningCases))

# count frequency of "miner"
minerCases <- lapply(myCorpusCopy,
                     function(x) {grep(as.character(x), pattern = "\\<miner")} ) # The symbols \< and \> respectively match the empty string at the beginning and end of a word
sum(unlist(minerCases))

# replace "miner" with "mining"
myCorpus <- tm_map(myCorpus, content_transformer(gsub),
                   pattern = "miner", replacement = "mining")


# Constructs a term-document matrix or a document-term matrix
# TDM is a matrix that lists all occurrences of words in the corpus
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(1, Inf)))
tdm

# inspect frequent words
freq.terms <- findFreqTerms(tdm, lowfreq = 15)# find words that occur at least 15 times
freq.terms
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 15)
term.freq

df <- data.frame(term = names(term.freq), freq = term.freq)
# Plot word frequencies
if(!require("ggplot2")){ 
  install.packages("ggplot2")
}
library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()
# find words which are associated with 'university'
findAssocs(tdm, "data", 0.3)#  lower correlation limits of each term is 0.3


# --- create a word cloud to visualize important words-----------------
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)

# plot word cloud
if(!require("wordcloud")){ # check for package existence 
  install.packages("wordcloud")
}
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)



# --- text clustering -----------------
# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)


# 1. hierarchical cluster analysis
m2 <- scale(m2) # Scale the data
distMatrix <- dist(m2)# Compute dissimilarity matrix
fit <- hclust(distMatrix, method = "ward.D") # Compute hierachical clustering
plot(fit)
rect.hclust(fit, k = 3) # cut tree into 6 clusters

# 2. K-means
m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
m3
#set.seed(122) # set a fixed random seed
k <- 3 # number of clusters

m3<-na.omit(m3)

kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers
for (i in 1:k){
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
  # print the tweets of every cluster
  # print(tweets[which(kmeansResult$cluster==i)])
}

# choose K 

cost_df <- data.frame()#accumulator for cost results
#run kmeans for all clusters up to 50
for(i in 1:50){
  #Run kmeans for each level of i, allowing up to 50 iterations for convergence
  kmeans<- kmeans(x=tdm, centers=i, iter.max=50)
  #Combine cluster number and cost together, write to df
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
  
}

names(cost_df) <- c("cluster", "cost")

#Calculate lm's for emphasis
lm(cost_df$cost[1:10] ~ cost_df$cluster[1:10])
lm(cost_df$cost[10:19] ~ cost_df$cluster[10:19])
lm(cost_df$cost[20:50] ~ cost_df$cluster[20:50])

cost_df$fitted <- ifelse(cost_df$cluster <10, (19019.9 - 550.9*cost_df$cluster), 
                         ifelse(cost_df$cluster <20, (15251.5 - 116.5*cost_df$cluster),
                                (13246.1 - 35.9*cost_df$cluster)))

#Cost plot

ggplot(data=cost_df, aes(x=cluster, y=cost, group=1)) + 
  theme_bw(base_family="Garamond") + 
  geom_line(colour = "darkgreen") +
  theme(text = element_text(size=20)) +
  ggtitle("Reduction In Cost For Values of 'k'\n") +
  xlab("\nClusters") + 
  ylab("Within-Cluster Sum of Squares\n") +
  scale_x_continuous(breaks=seq(from=0, to=50, by= 10)) +
  geom_line(aes(y= fitted), linetype=2)

# The plot above is known as the 'elbow method', 
# where we get breakpoints in our cost plot to understand where we should op adding clusters. 
# The slope of the cost function gets flatter at 10 clusters, then flatter again around 20 clusters. 
# This means that as we add clusters above 10 (or 20), 
# each additional cluster becomes less effective at reducing the distance from the each data center 


# --- topic modeling -----------------
dtm <- as.DocumentTermMatrix(tdm)
if(!require("topicmodels")){ # check for package existence 
  install.packages("topicmodels")
}
library(topicmodels)
lda <- LDA(dtm, k = 8) # create 8 topic-topic LDA model.It returns an object containing the full details of the model fit, such as how words are associated with topics and how topics are associated with documents.
term <- terms(lda, 6) # first 6 terms of every topic
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
# first topic identified for every document (tweet)
topic <- topics(lda, 1)
topics <- data.frame(date=as.Date(tweets.df$created), topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")

#3 Exercise III - Time Series Analysis
#. Download GOOG stock price through R API over past 10 years

rm(list = ls())
if(!require(quantmod)){
  install.packages("quantmod")
}
library(quantmod)


getSymbols(Symbols = "GOOG", src = "yahoo")
GOOG <- data.frame(GOOG)
head(GOOG)

getSymbols(Symbols = "GOOG", from = "2007-01-01", to = "2016-12-31")
goog <- data.frame(GOOG)
head(goog) 
write.csv(goog,'C:/Users/38933/Desktop/R/goog.csv',row.names = TRUE)  
goog = read.csv(file = "C:/Users/38933/Desktop/R/goog.csv", head = TRUE, sep = ",") 
head(goog)

# ------------------ convert to time series object ------------
# The ts() function will convert a numeric vector into an R time series object. 
myts <- ts(goog$GOOG.Close, start=c(2007, 1), end=c(2016, 12), frequency=12) 

# plot series
plot(myts)


# ----------------------forecasting-----------------------------
# 2. simple exponential smoothing

goog_data <- read.csv(file = "C:/Users/38933/Desktop/R/goog.csv", 
                    head = TRUE, sep = ",")
goog <- ts(goog_data$GOOG.Close, start = c(2017, 12), end=c(2018,12),frequency = 12)

# Using ses Function to get forecast based on simple exponential smoothing
if(!require(forecast)){
  install.packages("forecast")
}
library("forecast")

forecast_ses <- ses(goog, h = 12)
plot(forecast_ses, main = "Forecast of Close Price from Simple Exponential Smoothing", 
     xlab = "Date", ylab = "Stock Price")


# 3. Holt's Exponential Smoothing
goog_data <- read.csv(file = "C:/Users/38933/Desktop/R/goog.csv", 
                      head = TRUE, sep = ",")
goog <- ts(goog_data$GOOG.Close, start = c(2017, 12), end=c(2018,12),frequency = 12)
 

googseriesforecasts <- HoltWinters(goog, gamma=FALSE)
# gamma parameter used for the seasonal component. If set to FALSE, an non-seasonal model is fitted
googseriesforecasts
plot(googseriesforecasts,main = "Forecast of next 12 months Close Price from exponential Smoothing")

#Print a summary of the model estimated in the previous exercise


# model summary
analytics = create_analytics(goog, googseriesforecasts)
summary(analytics)

#bats------------ 
googforecastsb<- bats(goog_data$GOOG.Close,use.damped.trend = TRUE)
plot(googforecastsb,main = "Forecast of Close Price from BATS model")

