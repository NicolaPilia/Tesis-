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

#Converting foreign characters into english and dropping french rows
library(gsubfn)
cons<-gsub("Ç","c",cons)
cons<-gsub("¸",",",cons)
cons<-gsub("ƒ","f",cons)
cons<-gsub("ÿ","y",cons)
cons<-gsub("¶","",cons)
cons<-gsub("½","",cons)

pros<-gsub("Ç","c",pros)
pros<-gsub("¸",",",pros)
pros<-gsub("ƒ","f",pros)
pros<-gsub("ÿ","y",pros)
pros<-gsub("¶","",pros)
pros<-gsub("½","",pros)

pros<-pros[-c(809,810)]
cons<-cons[-c(809,810)]

#creating Corpus
library(tm)
corp_p<-Corpus(VectorSource(pros))
corp_c<-Corpus(VectorSource(cons))

#Removing whitespace, punctuation and numbers

corp_p<-tm_map(corp_p, stripWhitespace)
corp_p<-tm_map(corp_p, removePunctuation)
corp_p<-tm_map(corp_p, removeNumbers)

corp_c<-tm_map(corp_c, stripWhitespace)
corp_c<-tm_map(corp_c, removePunctuation)
corp_c<-tm_map(corp_c, removeNumbers)

#removing stopwords
corp_p<-tm_map(corp_p, removeWords,stopwords("english"))
corp_c<-tm_map(corp_c, removeWords,stopwords("english"))

#stemming the corpuses
corp_p<-tm_map(corp_p,stemDocument)
corp_c<-tm_map(corp_c,stemDocument)

#creating TermDocumentMatrix
tdm_p<-TermDocumentMatrix(corp_p)
tdm_c<-TermDocumentMatrix(corp_c)


#modifing words to differentiate between positive and negative terms.
tdm_p$dimnames$Terms <- paste0("P_",tdm_p$dimnames$Terms)
tdm_c$dimnames$Terms <- paste0("C_",tdm_c$dimnames$Terms)

#TF-IDF
tfidf_c<-weightTfIdf(tdm_c)
tfidf_p<-weightTfIdf(tdm_p)
inspect(tfidf_c)


#TF-IDF and latent semantic analysis

library(lsa)
lsa.tfidf_c<-lsa(tfidf_c,dim=20)
lsa.tfidf_p<-lsa(tfidf_p,dim=20)

words.df_c<-as.data.frame(as.matrix(lsa.tfidf_c$dk))
words.df_p<-as.data.frame(as.matrix(lsa.tfidf_p$dk))

