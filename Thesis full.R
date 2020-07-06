## Loading and cleaning the data
#x <- readLines("netflix.csv")
data<- read.csv("netflix.csv", header =TRUE, sep=";")
#Viewing complete dataset
View(data)
#str(data)
data<-data[-c(809,810),]
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

View(corp_c$content)
library(stringr)
corp_c$content<- str_replace_all(corp_c$content, "(\\b\\w)", 'C_\\1')
corp_p$content<- str_replace_all(corp_p$content, "(\\b\\w)", 'P_\\1')
corp<-corp_c

corp$content<-paste(corp_c$content,corp_p$content)
View(corp$content)


#creating TermDocumentMatrix
tdm_p<-TermDocumentMatrix(corp_p)
tdm_c<-TermDocumentMatrix(corp_c)

tdm<-TermDocumentMatrix(corp)


#modifing words to differentiate between positive and negative terms.
tdm_p$dimnames$Terms <- paste0("P_",tdm_p$dimnames$Terms)
tdm_c$dimnames$Terms <- paste0("C_",tdm_c$dimnames$Terms)

#TF-IDF
tfidf_c<-weightTfIdf(tdm_c)
tfidf_p<-weightTfIdf(tdm_p)
inspect(tfidf_c)

tfidf<- weightTfIdf(tdm)

View(tfidf$dimnames$Terms)
#TF-IDF and latent semantic analysis with 20 components

library(lsa)
lsa.tfidf_c<-lsa(tfidf_c,dim=20)
lsa.tfidf_p<-lsa(tfidf_p,dim=20)

lsa.tfidf<-lsa(tfidf,dim=20)

words.df_c<-as.data.frame(as.matrix(lsa.tfidf_c$dk))
words.df_p<-as.data.frame(as.matrix(lsa.tfidf_p$dk))

words.df<-as.data.frame(as.matrix(lsa.tfidf$dk))
View(words.df)

#Moving LSA to excel
write.table(words.df, file="LSA.csv", quote=F, sep=",", dec=".", na="NA", row.names=T, col.names=T)



#Implementing the logistic regression model
#aggiungo una colonna e rendo binaria la variabile overall rating
data$over.b<-0

data$over.b<-ifelse(data$overall.ratings>3.5,1,0)
summary(data$over.b)  

#sample 60% training data
training<-sample(c(1:810), 0.6*810)
training

#run logistic model on training
trainData = cbind(label=data$over.b[training],words.df[training,])
reg<-glm(label ~.,data=trainData, family='binomial')
summary(reg)

#compute accuracy on validation set
ValidData<-cbind(label=data$over.b[-training],words.df[-training,])
pred<-predict(reg,newdata=ValidData,type='response')

#confusion matrix
library(caret)

pred_factor <- factor(ifelse(pred>0.5,1,0), levels = 1:323)
pred_vector<-as.vector(pred_factor)
pred_vector[is.na(pred_vector)] <- 0
pred_factor<-as.factor(pred_vector)

over.b_factor<-factor(data$over.b[-training], levels = 1:324)
over.b_vector<-as.vector(over.b_factor)
over.b_vector[is.na(over.b_vector)] <- 0
over.b_factor<-as.factor(over.b_vector)

confusionMatrix(pred_factor, over.b_factor)

#creating lift chart
#install.packages("gains")
library(gains)

pred_num<-as.numeric(pred)
over.b_num<- as.numeric(as.character(over.b_factor))

gain<-gains(over.b_num,pred_num, groups=10)
barplot(gain$mean.resp/mean(over.b_num), names.arg = gain$depth, xlab="Percentile", ylab= "Mean Response", main="Decile-wise lift chart")

Lift_chart_df<-data.frame(over.b_num,pred)

