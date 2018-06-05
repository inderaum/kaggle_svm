#required packages
install.packages('e1071')
library(e1071)
install.packages("mice")
library(mice)
install.packages("caret")
library(caret)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

#read train data
train <- read.csv("data/cs-training.csv", header = T, sep = ",", dec = ".")
#exclude ids
train <- train[-c(1)]
#read test data
test <- read.csv("data/cs-test.csv", header = T, sep = ",", dec = ".")
#exclude ids
test <- test[-c(1)]

#view data
summary(train)
summary(test)

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

#imputed data
summary(train)
summary(test)

#Preprocessing

#-RevolvingUtilizationOfUnsecuredLines
#-- (total balance) / (total credit limit)

# the closer this value is to 100% the more the consumer is using the credit limit 
summary(train$RevolvingUtilizationOfUnsecuredLines)
mis <-train %>%
  filter(train$RevolvingUtilizationOfUnsecuredLines > 1)

summary(mis)

#percentage of regressor > 1 in train data
nrow(mis)/nrow(train)*100
train$RevolvingUtilizationOfUnsecuredLines[train$RevolvingUtilizationOfUnsecuredLines > 1] <- -1

summary(train$RevolvingUtilizationOfUnsecuredLines)


#-age

summary(train$age)
mis <- train %>%
  filter(train$age == 0)
nrow(mis)
#omit line with age = 0
train <- subset(train, age > 0)

#-NumberOfTimeXX_XXDaysPastDueNotWorse

summary(train$NumberOfTime30_59DaysPastDueNotWorse)
summary(train$NumberOfTime60_89DaysPastDueNotWorse)
summary(train$NumberOfTimes90DaysLate)

nrow(subset(train, train$NumberOfTime30_59DaysPastDueNotWorse >=15))
n_96 <- nrow(subset(train, train$NumberOfTime30_59DaysPastDueNotWorse ==96))
n_98 <- nrow(subset(train, train$NumberOfTime30_59DaysPastDueNotWorse ==98))

(n_96+n_98)/nrow(train)*100

train_tmp <- train[train$NumberOfTime30_59DaysPastDueNotWorse >= 15]

#- DebtRatio
#-- (total debts) / (monthly income)

summary(train$DebtRatio)

#https://youtu.be/SsZZx6e_TNI?t=306


#remove ids
train <- train[-1]
test <- test[-1]

#Modelling
#seed for reproducibility
set.seed(123)


#svm classifier using e1071
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


#svm classifier using caret

library(caret)

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     sampling = "down")

grid <- expand.grid(C = c(0.25, 0.5, 0.75, 1, 1.25,1.5))

svm_linMod <- train(SeriousDlqin2yrs~.,
                    data = train,
                    method = "svmLinear",
                    preProcess = c("center", "scale"),
                    metric = "ROC",
                    tuneGrid = grid,
                    trControl = ctrl)
