pres <- read.csv("http://www.stanford.edu/~messing/primaryres.csv", as.is=T)
 
# sort data in order of percent of vote:
pres <- pres[order(pres$Percentage, decreasing=T), ]
 
# only show top 15 candidates:
pres <- pres[1:15,]
 
# create a precentage variable
pres$Percentage <- pres$Percentage*100
 
# reorder the Candidate factor by percentage for plotting purposes:
pres$Candidate <- reorder(pres$Candidate, pres$Percentage)
 
library(ggplot2)
ggplot(pres, aes(x = Percentage, y = factor(Candidate))) +
geom_point(size=3.5) + 
theme_bw() + 
  #opts(axis.title.x = theme(size = 12, vjust = .25)) +
  xlab("Percent of Vote") + ylab("Candidate") + 
  ggtitle("New Hampshire Primary 2012")






