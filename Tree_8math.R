library(tidyverse)
library(corrplot)

Math = read.table("C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/STAT515/Final/student-mat.csv",sep=",",header=TRUE)

# Check data
str(Math)

# numerical summary
math = Math[,c("Medu", "Fedu", "studytime", "absences","goout", "Dalc", "Walc", "health","G3")]
summary(math)

set.seed(27)
#dividing sample in train and test sets.
Math.train=sample(1:nrow(math),nrow(math)*.8)
Math.test=math[-Math.train,]
#dim(Wine.test)

library(tree)
library(MASS)

#fit tree on training data by specifying subset=train 
tree.math=tree(G3~.,math ,subset=Math.train) #fit on training data
tree.math
plot(tree.math)
text(tree.math,pretty=0)
summary(tree.math)
#predict
math_yhat = predict(tree.math,math[-Math.train,])
math_test = math[-Math.train,"G3"]

#compute MSE
mean((math_yhat-math_test)^2)

SSE <- sum((math_yhat - math_test)^2)
SST <- sum((math_test - mean(math_test))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/nrow(math))
R_square
RMSE
## Using Cross Validation ##
#cross validation
set.seed(28)
cv.math=cv.tree(tree.math)
plot(cv.math$size,cv.math$dev,type='b')

#prune tree
prune.math=prune.tree(tree.math, best=4) 
plot(prune.math)
text(prune.math,pretty=0)

#compute MSE
cvmath_yhat = predict(prune.math,math[-Math.train,])
cvmath_test = math[-Math.train,"G3"]
mean((cvmath_yhat-cvmath_test)^2)
plot(cvmath_yhat, cvmath_test)
abline(0,1)


## Random Forest ##
library(randomForest)
set.seed(29)
#fit random forest with quality data set
rf.math=randomForest(G3~.,math,subset=Math.train, importance=TRUE)
summary(rf.math)
plot(rf.math)
which.min(rf.math$mse)

#predict
mathyhat.rf = predict(rf.math,newdata=math[-Math.train,])
plot(mathyhat.rf)

#MSE
MSE= mean((mathyhat.rf-cvmath_test)^2)
mean((mathyhat.rf2-cvmath_test)^2)
#important variables
importance(rf.math)
varImpPlot(rf.math)


