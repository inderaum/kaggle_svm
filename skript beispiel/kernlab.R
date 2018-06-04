rm(list = ls())
letters <- read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/letterdata.csv')
str(letters)

#Split into test and train:
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000,]

library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train,kernel = "vanilladot")
letter_classifier

#predictions:
letter_predictions <- predict(letter_classifier, letters_test)
#Check the accuracy:
caret::confusionMatrix(letter_predictions,letters_test$letter)
agreement <- letter_predictions == letters_test$letter
prop.table(table(agreement))
#We get an accuracy of 84% with a simple linear kernel,let's try with RBF kernel:
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train,kernel = "rbfdot")

#predictions:
letter_predictions <- predict(letter_classifier_rbf,letters_test)
#Check the accuracy:
caret::confusionMatrix(letter_predictions,letters_test$letter)
agreement <- letter_predictions == letters_test$letter
prop.table(table(agreement))
