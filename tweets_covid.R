library(twitteR)
library(tidyverse)

#install.packages("RColorBrewer")
library(RColorBrewer)

#Text processing
library(stringr)
#Text mining
library(tm)
#Wordcloud creation
library(wordcloud)

#install.packages("wordcloud2")
library(wordcloud2)

consumerKey = "KxnSMi1SiBgNMbPDE5Q2mL0Qo"
consumerSecret = "Q98N1pe3nELOEuSC1UUc3zjp6lq6ErCXk1u57DIMkUhSXyqE9k"

accessToken = "1148301020516225024-Gfgtvd9qOHLTsLS6yxLWFLsfF3JSw9"
accessTokenSecret = "24ZjJTLqLPElAeYfxgk1wH1moVWysbec7ue5nbZej34Sh"

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

#Search for tweets
twitter.covid.search <- searchTwitter("covid", n=15000, lang="en")
#Convert tweets to dataframe
twitter.excel <- twListToDF(twitter.covid.search)
#Converts tweets to csv
write.csv(twitter.excel,"twitter_covid02.csv")

#Import tweets
tweetsdf <- read.csv(file.choose())

#Backup tweets
tweetsdf.backup <- tweetsdf
tweetsdf <- tweetsdf.backup



#Removing retweets
tweetsdf <- tweetsdf %>% filter(isRetweet=="FALSE")



#Converts tweets to ASCII 
tweetsdf$text <- iconv(tweetsdf$text,from="UTF-8",to="ASCII",sub="")

#Remove usernames
tweetsdf$text <- gsub("@\\w+", "", tweetsdf$text)

#Remove blankspaces at begining
tweetsdf$text <- gsub("^ ", "", tweetsdf$text)

#Remove blankspaces at the end
tweetsdf$text <- gsub(" $", "", tweetsdf$text)

tweetsdf$text <- gsub("[[:punct:]]", "", tweetsdf$text)

#Remove links
tweetsdf$text <- gsub("http\\w+", "", tweetsdf$text)


#Create corpus
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

wordcloud2(data=d[-1,], size=1, color='random-dark')





