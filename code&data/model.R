library(nnet)
load('cleaned_data.Rdata')
set.seed(2021)

cleaned_data[, 357] = scale(cleaned_data[, 357])
cleaned_data = data.frame(cleaned_data)
n = dim(cleaned_data)[1]
n_train = floor(0.8 * n)

# split train/test
train_idx = sample(n, size = n_train, replace = F)
cleaned_data_train = cleaned_data[train_idx, ]
cleaned_data_test = cleaned_data[-train_idx, ]

# create weight
weight = rep(1, n_train)
weight[cleaned_data_train$AI_Future == 1] = n * 0.5 / sum(cleaned_data_train$AI_Future == 1)
weight[cleaned_data_train$AI_Future == 2] = n / sum(cleaned_data_train$AI_Future == 2)
weight[cleaned_data_train$AI_Future == 3] = n * 0.7 / sum(cleaned_data_train$AI_Future == 3)

# fit multinomial logistic regression model
mlr = multinom(AI_Future ~. - AI_Dangerous - AI_Interesting - AI_Responsible, data = cleaned_data_train, MaxNWts = 1436, weights = weight, maxit = 500)
save(mlr, file = 'lr_model.Rdata')

# label distribution
table(cleaned_data_train$AI_Future) # training label
table(predict(mlr)) # predicted training label
mean(predict(mlr) == cleaned_data_train$AI_Future) # training accuracy

table(cleaned_data_test$AI_Future) # testing label
table(predict(mlr, newdata = cleaned_data_test)) # testing label
mean(predict(mlr, newdata = cleaned_data_test) == cleaned_data_test$AI_Future) # testing accuracy

# Wald test
mlr.sum = summary(mlr)
z <- mlr.sum$coefficients / mlr.sum$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
mean(p < 0.05)
save(z, p, file = 'lr_model_test.Rdata')