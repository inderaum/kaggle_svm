library(e1071)
dat <- read.csv(file.choose())
x <- dat[-c(1:2)]
y <- SeriousDlqin2yrs
svm_linear <- svm(x,y)
summary(svm_linear)
p <- predict(svm_linear,x)
system.time(p <- predict(svm_linear,x))
con_mat <- table(p, y)
svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
svm_tuned <- svm(formula = Species ~ ., data = dat, kernel = 'radial',cost = 1, gamma = 0.5)
summary(svm_tuned)
system.time(predict(svm_tuned,x))
(con_mat_tuned <- table(p, y))