install.packages("kernlab")
library(kernlab)

vorlesung <- read.csv(file.choose())
y <- vorlesung[,1]

v_mod <- ksvm(as.factor(y)~., data = vorlesung, kernel = 'polydot', kpar = list("degree"=2), prob.model=T)

v_mod@nSV

v_mod@alphaindex

v_mod@alpha

v_mod@b

v_mod@error

u <- predict(v_mod, vorlesung[-1])

table(u, t(vorlesung[1]))

v_mod@xmatrix

v_mod@scaling

v_mod@coef[[1]][1]
v_mod@coef[[1]][2]

#Prognosewerte
f <- function(x1,x2){
  sum = 0
  for (i in 1:5) {
    sum = sum + v_mod@coef[[1]][i]*
    (v_mod@xmatrix[[1]][i,1]*x1+
    v_mod@xmatrix[[1]][i,2]*x2+1)^2
  }
  print(sum - v_mod@b)
}
f(-0.9979654098966,-0.9960238411121)

#Entscheidungsfunktion
f <- function(x1,x2) {
  summe=0
  for (i in 1:5) {
    summe=summe + v_mod@coef[[1]][i]*
      ( v_mod@xmatrix[[1]][i,1]*x1 +
          v_mod@xmatrix[[1]][i,2]*x2 + 1)^2
  }
  print(
    sign(summe-v_mod@b))
}
f(1.5682313584086,0.9960238411120)
