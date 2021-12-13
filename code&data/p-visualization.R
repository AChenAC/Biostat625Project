library(ggplot2)
library(hrbrthemes)

load("code&data/lr_model_test.Rdata")

# rescale p-values: in significant p-values are assigned 1, significant ones are scaled to 0-0.5
p[which(p >= 0.05)] <- 1
psignificant <- p[which(p < 0.05)]
xmax <- max(psignificant)
xmin <- min(psignificant)

for (i in 1:716) {
  if(p[i] < 0.05){
   p[i] <- 0.8 * (p[i] - xmin) / (xmax - xmin)
  }
}

# change sign of p-values according to sign of coefficient
p = 1 - p
p = sign(beta) * p

# create gird for plotting
x <- c('dangerous', 'excited')
y <- colnames(p)
data <- expand.grid(X = x, Y = y)
data$significance <- as.numeric(p)

# exclude intercept
data = data[-c(1:2), ]

# obtain labels
load("code&data/question_map.Rdata")
question_label = indexofquestion[3, ]

# change y-label
first_occurance = match(unique(question_label), question_label)
idx = first_occurance + floor(diff(c(first_occurance, 357)) / 2)
y_label = rep('', length(question_label))
y_label[idx] = question_label[idx]
#y_label[first_occurance] = question_label[first_occurance]

# plot
cut = c(0, 48, 97, 150, 200, 259, 319, 357)
plots = list()
for (i in 1:7) {
  q_start = cut[i] + 1
  q_end = cut[i + 1]
  i_start = (q_start - 1) * 2 + 1
  i_end = q_end * 2
  
  colormap = 'none'
  if (i == 1) {
    colormap = guide_colourbar(direction = "horizontal", position = 'top', title.position = 'top')
  }
  plots[[i]] = ggplot(data[i_start:i_end, ], aes(X, Y, fill = significance)) + 
               geom_tile() +
               scale_fill_gradient2(midpoint = 0, low = 'skyblue4', mid = 'beige', high = 'orangered3', guide = colormap) +
               theme_ipsum() + 
               scale_y_discrete(labels = y_label[q_start:q_end]) + 
               geom_hline(yintercept = first_occurance - (q_start - 1) - 0.5, 
                          color = 'purple4', linetype = "dashed", size = 0.5) + 
               theme(axis.text.y = element_text(size = 10), 
                     axis.text.x = element_text(size = 10),
                     legend.position = "top",
                     plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) + 
               labs(y = "", x = "") +
               scale_x_discrete(guide = guide_axis(angle = 90))
}


# plot side by side
library(gridExtra)
png('p-value.png', width = 3500, height = 3000, res = 200)
grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], plots[[7]], ncol = 7)
dev.off()
