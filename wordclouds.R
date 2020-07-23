##word cloud creation
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
#importing data from tables
wc_values<-read.table(file="clipboard",header=TRUE, sep="\t")
wc_words<-read.table(file="clipboard",header = TRUE, sep="\t" )
#creating wordcloud for each dimension
par(mar = rep(0, 4))
wordcloud(words = wc_words$V1, freq = wc_values$V1, min.freq = 0,
          max.words=20, random.order=FALSE, rot.per=0.35, scale=c(2.5,1.5), 
          col=brewer.pal(9,"Blues"))
par(mar = rep(0, 4))
wordcloud(words = wc_words$V2, freq = wc_values$V2, min.freq = 0,
          max.words=20, random.order=FALSE, rot.per=0.35, scale=c(2,1), 
          col=brewer.pal(9,"Reds"))
par(mar = rep(0, 4))
wordcloud(words = wc_words$V3, freq = wc_values$V3, min.freq = 0,
          max.words=20, random.order=FALSE, rot.per=0.35, scale=c(2,1), 
          col=brewer.pal(9,"Greens"))
par(mar = rep(0, 4))
wordcloud(words = wc_words$V4, freq = wc_values$V4, min.freq = 0,
          max.words=20, random.order=FALSE, rot.per=0.35, scale=c(2.5,1), 
          col=brewer.pal(9,"Oranges"))
par(mar = rep(0, 4))
wordcloud(words = wc_words$V5, freq = wc_values$V5, min.freq = 0,
          max.words=20, random.order=FALSE, rot.per=0.35, scale=c(2.5,1), 
          col=brewer.pal(9,"Purples"))
par(mar = rep(0, 4))
wordcloud(words = wc_words$V6, freq = wc_values$V6, min.freq = 0,
          max.words=20, random.order=FALSE, rot.per=0.35, scale=c(2.5,1), 
          col=brewer.pal(9,"YlOrRd"))

