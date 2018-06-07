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
install.packages("Hmisc")
library(Hmisc)

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

#apply coded value -1 to outliers
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

#it can be assumed that 96 and 98 are coded values of some kind, 
#because both values have their own meaning they cant be ommited
# and have to be encoded in some kind
nrow(subset(train, train$NumberOfTime30_59DaysPastDueNotWorse >=15))
n_96 <- nrow(subset(train, train$NumberOfTime30_59DaysPastDueNotWorse ==96))
n_98 <- nrow(subset(train, train$NumberOfTime30_59DaysPastDueNotWorse ==98))

(n_96+n_98)/nrow(train)*100

train$NumberOfTime30_59DaysPastDueNotWorse[train$NumberOfTime30_59DaysPastDueNotWorse >= 15]<- -1

summary(train$NumberOfTime30_59DaysPastDueNotWorse)

#the same approach applies to NumberOfTime60_89DaysPastDueNotWorse and
#NumberOfTimes90DaysLate
nrow(subset(train, train$NumberOfTime60_89DaysPastDueNotWorse >=15))
n_96 <- nrow(subset(train, train$NumberOfTime60_89DaysPastDueNotWorse ==96))
n_98 <- nrow(subset(train, train$NumberOfTime60_89DaysPastDueNotWorse ==98))

(n_96+n_98)/nrow(train)*100

train$NumberOfTime60_89DaysPastDueNotWorse[train$NumberOfTime60_89DaysPastDueNotWorse >= 15] <- -1

summary(train$NumberOfTime60_89DaysPastDueNotWorse)

summary(train$NumberOfTimes90DaysLate)
nrow(subset(train, train$NumberOfTimes90DaysLate >=19))
n_96 <- nrow(subset(train, train$NumberOfTimes90DaysLate ==96))
n_98 <- nrow(subset(train, train$NumberOfTimes90DaysLate ==98))

(n_96+n_98)/nrow(train)*100

train$NumberOfTimes90DaysLate[train$NumberOfTimes90DaysLate >= 19] <- -1

summary(train$NumberOfTime60_89DaysPastDueNotWorse)

#- DebtRatio
#-- (total debts) / (monthly income)
#-- thus, values > 1 indicate more debts than income

summary(train$DebtRatio)
summary(train$MonthlyIncome)

#monthly income is the denominator of debt ratio thus it cannot be 0
#percentage of regressor > 1 in train data
n_inc0 <- nrow(subset(train, train$MonthlyIncome ==0))
n_inc0/nrow(train)*100

#if the monthly salary is equal to zero it is replaced by -1 

index <- train$MonthlyIncome == 0
train$DebtRatio[index] <- -1
summary(train$MonthlyIncome)

#if the monthly income is missing, it is replaced by 1

train$MonthlyIncome[is.na(train$MonthlyIncome)] <- 1

summary(train$DebtRatio)

#-Monthly income

summary(train$MonthlyIncome)

n_inc50k <- nrow(subset(train, train$MonthlyIncome >50000))
n_inc50k/nrow(train)*100

#omit outliers
train <- subset(train, MonthlyIncome < 50000)

#-NumberOfOpenCreditLinesAndLoans

summary(train$NumberOfOpenCreditLinesAndLoans)

#omit outliers in the 99th percentile

nrow(train[train$NumberOfOpenCreditLinesAndLoans < quantile(train$NumberOfOpenCreditLinesAndLoans, 0.99),])
train <- train[train$NumberOfOpenCreditLinesAndLoans < quantile(train$NumberOfOpenCreditLinesAndLoans, 0.99),]
summary(train$NumberOfOpenCreditLinesAndLoans)

#-NumberOfDependents

summary(train$NumberOfDependents)

#omit outliers in the 99th percentile

nrow(train[train$NumberOfDependents < quantile(train$NumberOfDependents, 0.99),])
train <- train[train$NumberOfDependents < quantile(train$NumberOfDependents, 0.99),]
summary(train$NumberOfDependents)

summary(train)


#remove ids
train <- train[-1]
test <- test[-1]




#Modelling
#seed for reproducibility
set.seed(123)

#svm classifier using e1071
library(e1071)
# classifier <- svm(formula = SeriousDlqin2yrs ~ ., 
#                   data = train, 
#                   type = "C-classification", 
#                   kernel = "linear")

classifier_rbf <- svm(formula = SeriousDlqin2yrs ~ ., 
                  data = train, 
                  type = "C-classification", 
                  kernel = "radial")

# pred_lin <- predict(classifier, newdata = test[-1])
pred_rbf <- predict(classifier_rbf, newdata = train[-1])

cm_lin = table(train[1], pred_rbf[2])


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
