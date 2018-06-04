#required packages
install.packages('e1071')
library(e1071)
install.packages('ElemStatLearn')
library(ElemStatLearn)
install.packages("mice")
library(mice)

#read data
train <- read.csv("data/cs-training.csv", header = T, sep = ",", dec = ".")
#exclude ids
train <- train[-c(1)]
test <- read.csv("data/cs-test.csv", header = T, sep = ",", dec = ".")
#exclude ids
test <- test[-c(1)]

#view data
summary(train)
summary(test)

#view na pattern
md.pattern(train)
md.pattern(test)

#imputation using MICE package
imp <- mice(train, m=5, maxit=2, method='pmm', seed = 123)
train_imputed <- complete(x = imp,action =  1)
#summary(train_imputed)
imp <- mice(test, m=5, maxit=2, method='pmm', seed = 123)
test_imputed <- complete(x = imp,action =  1)

#export imputed data as .csv for future usage
write.csv(test_imputed,"data/test_imputed.csv")
write.csv(train_imputed,"data/train_imputed.csv")

#----------------------------

#read imputed data
train <- read.csv("data/train_imputed.csv")
test <- read.csv("data/test_imputed.csv")
train <- train[-1]
test <- test[-1]
#imputed data
summary(train)
summary(test)
#seed for reproducibility
set.seed(123)

#visualize data


#svm classifier
library(e1071)
classifier <- svm(formula = SeriousDlqin2yrs ~ ., 
                  data = train, 
                  type = "C-classification", 
                  kernel = "linear")

classifier_rbf <- svm(formula = SeriousDlqin2yrs ~ ., 
                  data = train, 
                  type = "C-classification", 
                  kernel = "radial")

pred_lin <- predict(classifier, newdata = test[-1])
pred_rbf <- predict(classifier_rbf, newdata = train[-1])

cm_lin = table(train[1], pred_lin[2])
