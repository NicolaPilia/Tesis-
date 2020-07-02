library(tm)

text<-c("this is the first    sentence!!",
        "this is a second sentence :)",
        "the third sentence, is here",
        "forth of all sentences")
corp<-Corpus(VectorSource(text))
#tokenization
corp<-tm_map(corp, stripWhitespace)
corp<-tm_map(corp, removePunctuation)

#stopwords
corp<-tm_map(corp,removeWords,stopwords("english"))

#stemming
corp<-tm_map(corp,stemDocument)

tdm<-TermDocumentMatrix(corp)
inspect(tdm)

