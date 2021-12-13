setwd("D:/625 final project")
library(ggplot2)

load("lr_model_test.Rdata")
p[which(p>=0.05)] <- 1
psignificant <- p[which(p<0.05)]
xmax <- max(psignificant)
xmin <- min(psignificant)


for (i in 1:716) {
  if(p[i] < 0.05){
   p[i] <- 0.5*(p[i]-xmin)/(xmax-xmin)
  }
}

heatmap(p, col=rev(heat.colors(12)))


