#My functions

#for preparing data for Confusion Matrix 
conf_clean<-function(x){
  x<-ifelse(x>0.5,1,0)%>%factor()
  x<-as.vector(x)
  x[is.na(x)] <-0
  x<-as.factor(x)
}

complete_analysis<-function(y,z){
  #creating the corpus
  library(tm)
  corp<-Corpus(VectorSource(y))
  #tokenization
  corp<-tm_map(corp,stripWhitespace)
  corp<-tm_map(corp, removePunctuation)
  corp<-tm_map(corp, removeNumbers)
  #stopwords
  corp<-tm_map(corp, removeWords,stopwords("SMART"))
  #stemming
  corp<-tm_map(corp,stemDocument)
  #TF-IDF and latent semantic analysis
  tdm<-TermDocumentMatrix(corp)
  tfidf<-weightTfIdf(tdm)
  library(lsa)
  lsa.tfidf<-lsa(tfidf,dim=z)
  words.df<-as.data.frame(as.matrix(lsa.tfidf$dk))
}


