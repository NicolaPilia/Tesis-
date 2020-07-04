summ<-data[c(7)]

summ2<-summ[c(1:720),]
#View(summ2)

#creating the corpus
library(tm)
corp<-Corpus(VectorSource(summ2))
tdm<-TermDocumentMatrix(corp)
inspect(tdm)
