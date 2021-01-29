library("twitteR")
library("ROAuth")
library(base64enc)
library(httpuv)
library(tm)
library(wordcloud)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

install.packages('devtools')
library(devtools)
 devtools::install_github("lchiffon/wordcloud2")


cred <- OAuthFactory$new(consumerKey='BagGgBbanzbdpPNNp8Uy6TQBP', # Consumer Key (API Key)
                         consumerSecret='pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',                accessURL='https://api.twitter.com/oauth/access_token',                 authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

#Access Token Secret

setup_twitter_oauth("BagGgBbanzbdpPNNp8Uy6TQBP", # Consumer Key (API Key)
                    "pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa", #Consumer Secret (API Secret)
                    "1076425245521731584-Ev31ZLB7Cf0idVMqDI8BxiVG2SgRnu",  # Access Token
                    "ZVUw0Z0mFrX7d6sjQxuB08l48JHhmnjmlAm86G2OPG7BS")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('facebook', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)

View(TweetsDF)

write.csv(TweetsDF, "Tweets.csv",row.names = F)

getwd()

# handleTweets <- searchTwitter('DataScience', n = 10000)
# Read file
facebook <- read.csv(file.choose())
str(facebook)
facebook[sapply(facebook,is.character)] <- lapply(facebook[sapply(facebook,is.character)],as.factor)
str(facebook)

# Build Corpus and DTM/TDM
corpus <- facebook$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
#clean
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

corpus_clean <- tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])

cleanset <- tm_map(corpus,removeWords,stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))


inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,removeWords, c('facebook','can'))

cleanset <- tm_map(cleanset, gsub,pattern = 'pages', replacement = 'page')
inspect(cleanset[1:5])

tdm<- TermDocumentMatrix(cleanset)
tdm

tdm<- as.matrix(tdm)
tdm[1:10,1:20]

w <- rowSums(tdm)
w <- subset(w,w>=25)
barplot(w, las = 2, col = rainbow(50))


w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(123)
wordcloud(words = names(w), freq = w,
          max.words = 250,random.order = F,
          min.freq = 3,
          colors = brewer.pal(8,'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

################sentimental analysis######
install.packages("syuzhet")
library(syuzhet)
fbdata <- read.csv(file.choose(),header = TRUE)
tweets <- as.character(fbdata$text)
class(tweets)

install.packages("rlang")
library(rlang)
#obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[2]
get_nrc_sentiment('pretending')
get_nrc_sentiment('can learn')

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'count',main = "sentiment scores for facebook tweets")
