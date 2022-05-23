library(tidyverse)
library(corrplot)

Por = read.table("C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/STAT515/Final/student-por.csv",sep=",",header=TRUE)

# Check data
str(Por)

# numerical summary
por = Por[,c("Medu", "Fedu", "studytime", "absences","goout", "Dalc", "Walc", "health","G3")]
summary(por)

set.seed(1)
#dividing sample in train and test sets.
Por.train=sample(1:nrow(por),nrow(por)*.8)
Por.test=por[-Por.train,]

library(tree)
library(MASS)

#fit tree on training data by specifying subset=train
tree.por=tree(G3~.,por ,subset=Por.train) #fit on training data
tree.por
plot(tree.por)
text(tree.por,pretty=0)
summary(tree.por)
#predict
por_yhat = predict(tree.por,por[-Por.train,])
por_test = por[-Por.train,"G3"]

#compute MSE
mean((por_yhat-por_test)^2)

SSE <- sum((por_yhat - por_test)^2)
SST <- sum((por_test - mean(por_test))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/nrow(math))
R_square
RMSE
## Using Cross Validation ##
#cross validation
set.seed(2)
cv.por=cv.tree(tree.por)
plot(cv.por$size,cv.por$dev,type='b')

#prune tree
prune.por=prune.tree(tree.por, best=3) 
plot(prune.por)
text(prune.por,pretty=0)

#compute MSE
cvpor_yhat = predict(prune.por,por[-Por.train,])
cvpor_test = por[-Por.train,"G3"]
mean((cvpor_yhat-cvpor_test)^2)
plot(cvpor_yhat, cvpor_test)
abline(0,1)
## Random Forest ##
library(randomForest)
set.seed(3)
#fit random forest with quality data set
rf.por=randomForest(G3~.,por,subset=Por.train, importance=TRUE)
rf.por

#predict
poryhat.rf = predict(rf.por,newdata=por[-Por.train,])
plot(poryhat.rf)
#MSE
mean((poryhat.rf-cvpor_test)^2)

importance(rf.por)
varImpPlot(rf.por)
