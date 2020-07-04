cons<-data$cons
pros<-data$pros

cons_new<- str_replace_all(cons, "(\\b\\w)", 'C_\\1')



library(stringr)
View(cons_new)
