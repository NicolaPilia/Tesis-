
#creating the corpus
library(tm)
corp<-Corpus(VectorSource(summ))


#tokenization
corp<-tm_map(corp,stripWhitespace)
corp<-tm_map(corp, removePunctuation)
corp<-tm_map(corp, removeNumbers)

#stopwords
corp<-tm_map(corp, removeWords,stopwords("english"))

dataframe<-data.frame(text=unlist(sapply(corp,'[',"content")), stringsAsFactors = F)
View(dataframe)
#stemming
corp<-tm_map(corp,stemDocument)


#TF-IDF and latent semantic analysis
tdm<-TermDocumentMatrix(corp)
tfidf<-weightTfIdf(tdm)

library(lsa)
lsa.tfidf<-lsa(tfidf, dim=20)

words.df<-as.data.frame(as.matrix(lsa.tfidf$dk))

