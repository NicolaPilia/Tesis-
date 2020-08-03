cons<-data$cons
pros<-data$pros

cons<-gsub("Ç","c",cons)
cons<-gsub("¸",",",cons)
cons<-gsub("ƒ"," ",cons)
cons<-gsub("ÿ","",cons)
cons<-gsub("¶","",cons)
cons<-gsub("½","",cons)
cons<-gsub("","",cons)
cons<-gsub("ù","",cons)
cons<-gsub("/"," ",cons)
cons<-gsub("-"," ",cons)
cons<-gsub("[.]"," ",cons)
cons<-gsub(","," ",cons)
cons<-gsub("[+]"," ",cons)
cons<-gsub("ý","",cons)
cons<-gsub("<","",cons)
cons<-gsub("[(]"," ",cons)
cons<-gsub("sloooooow","slow",cons)
cons<-gsub("guis","guys",cons)
cons<-gsub("ppl","people",cons)
cons<-gsub("mgr","manager",cons)
cons<-gsub("manger","manager",cons)
cons<-gsub("guage","gauge",cons)
cons<-gsub("imposssible","impossible",cons)
cons<-gsub("disscussion","discussion",cons)
cons<-gsub("employess","employees",cons)
cons<-gsub("insufficiect","insufficient",cons)
cons<-gsub("mgt","management",cons)
cons<-gsub("WIshy washy","wishiwashy",cons)

pros<-gsub("Ç","c",pros)
pros<-gsub("¸",",",pros)
pros<-gsub("ƒ"," ",pros)
pros<-gsub("ÿ","",pros)
pros<-gsub("¶","",pros)
pros<-gsub("","",pros)
pros<-gsub("½","",pros)
pros<-gsub("ù","",pros)
pros<-gsub("/"," ",pros)
pros<-gsub("-"," ",pros)
pros<-gsub("[.]"," ",pros)
pros<-gsub(","," ",pros)
pros<-gsub("[+]"," ",pros)
pros<-gsub("ý","",pros)
pros<-gsub("<","",pros)
pros<-gsub("[(]"," ",pros)
pros<-gsub("ppl","people",pros)
pros<-gsub("saavy","savvy",pros)
pros<-gsub("mgr","manager",pros)
pros<-gsub("Cluture","culture",pros)
pros<-gsub("prettt","pretty",pros)


corp_p<-Corpus(VectorSource(pros))
corp_c<-Corpus(VectorSource(cons))

corp_p<-tm_map(corp_p, stripWhitespace)
corp_p<-tm_map(corp_p, removePunctuation)
corp_p<-tm_map(corp_p, removeNumbers)
corp_p<-tm_map(corp_p, tolower)

corp_c<-tm_map(corp_c, stripWhitespace)
corp_c<-tm_map(corp_c, removePunctuation)
corp_c<-tm_map(corp_c, removeNumbers)
corp_c<-tm_map(corp_c, tolower)

#removing stopwords
corp_p<-tm_map(corp_p, removeWords,stopwords("english"))
corp_c<-tm_map(corp_c, removeWords,stopwords("english"))
corp_p<-tm_map(corp_p, removeWords,stopwords("SMART"))
corp_c<-tm_map(corp_c, removeWords,stopwords("SMART"))

#stemming the corpuses
corp_p<-tm_map(corp_p,stemDocument)
corp_c<-tm_map(corp_c,stemDocument)

#re-removing stopwords using SMART
corp_p<-tm_map(corp_p, removeWords,stopwords("SMART"))
corp_c<-tm_map(corp_c, removeWords,stopwords("SMART"))

#modifing words to differentiate between positive and negative terms and unify the two variables, pros and cons, in the same variable.
library(stringr)
corp_c$content<- str_replace_all(corp_c$content, "(\\b\\w)", 'c_\\1')
corp_p$content<- str_replace_all(corp_p$content, "(\\b\\w)", 'p_\\1')
corp<-corp_c
corp$content<-paste(corp_c$content,corp_p$content)

#pulizia3<-c("c_veri","c_isnt","c_con")
corp <- tm_map(corp, removeWords, pulizia2)
corp<-tm_map(corp, stripWhitespace)
tdm<-TermDocumentMatrix(corp)

#TF-IDF
tfidf<- weightTfIdf(tdm)

#dropping empty documents
#colTotals <- apply(tfidf , 2, sum)
#tfidf <- tfidf[ , colTotals> 0]

#converting to dataframe
tfidf_df<-as.data.frame(as.matrix(tfidf))



#TF-IDF and latent semantic analysis with 6 components (the evaluation of the optimal number of
# LSA component is in the notebook called "CV for LSA")
library(lsa)
lsa.tfidf<-lsa(tfidf,dim=20)

words.df<-as.data.frame(as.matrix(lsa.tfidf$dk))

#run logistic model on training
trainData = cbind(label=data$over.b[training],words.df[training,])
reg<-glm(label~V1+V3+V6+V7+V8+V12+V14+V18,data=trainData, family='binomial')
summary(reg)
