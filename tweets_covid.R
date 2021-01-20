#LIBRARIES REQUIRED FOR THIS PROJECT.

# Extracting tweets
library(twitteR)

#Data Maipulation
library(tidyverse)

#Text processing
library(stringr)

#Text mining 
library(tm)

#install.packages("wordcloud2")
library(wordcloud2)

install.packages('sentimentr')
library(sentimentr) 



#Extracted tweets from twitter using twitterR package and copied it to a csv file.
# Only 1500 rows of data was used out of 20,000 extracted to bring down the size of csv file


#Import tweets
tweetsdf <- read.csv(file.choose())

#Backup tweets
tweetsdf.backup <- tweetsdf


#Removing retweets
tweetsdf <- tweetsdf %>% filter(isRetweet=="FALSE")


#Converts tweets to ASCII 
tweetsdf$text <- iconv(tweetsdf$text,from="UTF-8",to="ASCII",sub="")

# We are only intrested in tweets(data) in text file, below process is used to clean text column of unwanted details.

#Remove usernames
tweetsdf$text <- gsub("@\\w+", "", tweetsdf$text)

#Remove blankspaces at begining
tweetsdf$text <- gsub("^ ", "", tweetsdf$text)

#Remove blankspaces at the end
tweetsdf$text <- gsub(" $", "", tweetsdf$text)

tweetsdf$text <- gsub("[[:punct:]]", "", tweetsdf$text)

#Remove links
tweetsdf$text <- gsub("http\\w+", "", tweetsdf$text)


#Create corpus, this is required for creating wordcloud and further cleansing of text field
covid.words <- Corpus(VectorSource(tweetsdf$text))

# Removing stop words
covid.words <- tm_map(covid.words,removeWords,stopwords("english"))

# Removing numbers 
covid.words <- tm_map(covid.words, removeNumbers)

# Transforming everything to lowercase
covid.words <- tm_map(covid.words, content_transformer(tolower))

covid.words <- tm_map(covid.words, stemDocument, "english")


#Create matrix from corpus for wordcloud
dtm <- TermDocumentMatrix(covid.words)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


#Wordcloud

wordcloud(words=d$word, freq=d$freq, min.freq = 8, colors= c("grey80","darkgoldenrod1","tomato"),random.order=FALSE,rot.per=0.35)

#Since tweets analysed were about covid, covid was the most frequently used word. Removed word covid from the wordcloud.
wordcloud2(data=d[-1,], size=1, color='random-dark')

#SENTIMENT ANALYSIS
sentiment <- tweetsdf$text %>% extract_sentiment_terms()


