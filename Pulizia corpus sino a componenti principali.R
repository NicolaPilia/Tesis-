
#creating the corpus
library(tm)
corp<-Corpus(VectorSource(pros))


#tokenization
corp<-tm_map(corp,stripWhitespace)
corp<-tm_map(corp, removePunctuation)
corp<-tm_map(corp, removeNumbers)
#stopwords
stopwords_list<-c(stopwords("english"),"Netflix","worked")
corp<-tm_map(corp, removeWords,stopwords("SMART"))
#stemming
corp<-tm_map(corp,stemDocument)

inspect(tdm)

#TF-IDF and latent semantic analysis
tdm<-TermDocumentMatrix(corp)
tfidf<-weightTfIdf(tdm)

library(lsa)
lsa.tfidf<-lsa(tfidf,dim=20)

words.df<-as.data.frame(as.matrix(lsa.tfidf$dk))
View(words.df)


#write.table(words.df, file="mydata.csv", quote=F, sep=",", dec=".", na="NA", row.names=T, col.names=T)
