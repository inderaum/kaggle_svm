install.packages("kernlab")
library(kernlab)
#read
train <- read.csv("data/train_imputed.csv")
test <- read.csv("data/test_imputed.csv")
train <- train[-1]
test <- test[-1]

#split target and features
target <- train[1]
features <- train[2:11]

mod_poly <-ksvm(formula = as.factor(target)~.,
                data = train,
                kernel = 'polydot',
                kpar = list('degree' = 2),
                prob.model = T)

dat <- train[sample(1:nrow(train),100, replace = F),]