# Loading and cleaning the data
x <- readLines("netflix.csv")
data<- read.csv("netflix.csv", header =TRUE, sep=";")
#Viewing complete dataset
View(data)
#str(data)

#Separating text variables to other variables
summ<-data$summary
pros<-data$pros


#Converting foreign characters into english
library(gsubfn)
data<-gsub("Ç","c",data)
data<-gsub("¸",",",data)
data<-gsub("f","f",data)
View(summ)

install.packages("SnowballC")
