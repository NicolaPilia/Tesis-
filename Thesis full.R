## Loading and cleaning the data
#x <- readLines("netflix.csv")
data<- read.csv("netflix.csv", header =TRUE, sep=";")
#Viewing complete dataset
View(data)
#str(data)

#Separating text variables to other variables
cons<-data$cons
pros<-data$pros

View(pros)

#Converting foreign characters into english
library(gsubfn)
cons<-gsub("?","c",cons)
cons<-gsub("?",",",cons)
cons<-gsub("f","f",cons)
cons<-gsub("?","y",cons)
cons<-gsub("??","",cons)
cons<-gsub("*","",cons)

pros<-gsub("?","c",pros)
pros<-gsub("?",",",pros)
pros<-gsub("f","f",pros)
pros<-gsub("?","y",pros)
pros<-gsub("??","",pros)
pros<-gsub("*","",pros)


corp_p<-Corpus(VectorSource(pros))
corp_c<-Corpus(VectorSource(cons))

corp_p<-tm_map(corp_p,stripWhitespace)
corp_p<-tm_map(corp_p, removePunctuation)
corp_p<-tm_map(corp_p, removeNumbers)
#stopwords

corp_p<-tm_map(corp_p, removeWords,stopwords("english"))
#stemming
corp_p<-tm_map(corp,stemDocument)
inspect(tdm_p)
inspect(corp_p)
?tdm_p<-TermDocumentMatrix(corp_p)
#modifing words to differentiate between positive and negative terms.
tdm_p$dimnames$Terms <- paste0("P_",tdm$dimnames$Terms)

tfidf_p<-weightTfIdf(tdm)
inspect(tfidf_p)

#TF-IDF and latent semantic analysis
tdm<-TermDocumentMatrix(corp)
tfidf<-weightTfIdf(tdm)

library(lsa)
lsa.tfidf<-lsa(tfidf,dim=20)

words.df<-as.data.frame(as.matrix(lsa.tfidf$dk))
View(words.df) 
