library(MASS) 
library(DAAG)
library(leaps)

stu_mat<-read.csv("C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/STAT515/Final/student-mat.csv") 
stu_mat = stu_mat[-c(31,32)]
View(stu_mat) 
stu_mat = stu_mat[,c("Medu", "Fedu", "studytime", "absences","goout", "Dalc", "Walc", "health","G3")]
## Forward Stepwise ##
matfit.fwd=regsubsets(G3~.,data=stu_mat, nvmax = 9, method ="forward")
fwd.sum = summary(matfit.fwd)
names(fwd.sum)
which.min(fwd.sum$rss)
which.max(fwd.sum$adjr2)
which.min(fwd.sum$cp) 
which.min(fwd.sum$bic)
coef(matfit.fwd,9)
## Backward ##
matfit.bwd=regsubsets(G3~.,data=stu_mat, nvmax = 9, method ="backward")
bwd.sum = summary(matfit.bwd)
names(bwd.sum)
which.min(bwd.sum$rss)
which.max(bwd.sum$adjr2)
which.min(bwd.sum$cp) 
which.min(bwd.sum$bic)
coef(matfit.bwd,9)

model_stu_mat3<-lm(G3~sex+ 
                     Mjob+ 
                     Fjob+
                     studytime+ 
                     failures+ 
                     romantic+ 
                     goout+
                     absences, data=stu_mat)
summary(model_stu_mat3)
plot(model_stu_mat3) 
mean(model_stu_mat3$residuals^2)

model_stu_mat2<-lm(G3~sex+ 
                    Medu+ 
                    Mjob+ 
                    studytime+ 
                    failures+ 
                    famsup+
                    romantic+ 
                    goout, data=stu_mat) 
summary(model_stu_mat2)
plot(model_stu_mat2) 
mean(model_stu_mat2$residuals^2)

model_stu_mat<-lm(G3~Medu+ 
                    Fedu+ 
                    absences+ 
                    studytime+ 
                    Dalc+ 
                    Walc+
                    health+ 
                    goout, data=stu_mat) 
summary(model_stu_mat) 
export_summs(model_stu_mat) 
plot(model_stu_mat) 

set.seed(24)
#dividing sample in train and test sets.
math_train=sample(1:nrow(stu_mat),nrow(stu_mat)*.8)
math_test=math[-math_train,]
predict.math <- predict(model_stu_mat, newdata = math_test)

#Getting MSE
mean(model_stu_mat$residuals^2)
model_stu_mat$residuals

# Check Varience inflation factors(VIF)
vif(model_stu_mat) 

## Cross validation ##
cv_stu_mat<-cv.lm(data=stu_mat, model_stu_mat, m=10) 
mean(csv_stu_mat$residuals^2)


