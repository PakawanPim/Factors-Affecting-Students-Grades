library(MASS) 
library(DAAG)

stu_por<-read.csv("C:/Users/pumpim/OneDrive - George Mason University - O365 Production/Desktop/DEAN/Major/STAT515/Final/student-por.csv") 

View(stu_por) 

model_stu_por<-lm(G3~Medu+ 
                    Fedu+ 
                    absences+ 
                    studytime+ 
                    Dalc+ 
                    freetime+ 
                    Walc+ 
                    health+ 
                    goout, data=stu_por) 

summary(model_stu_por) 

#export_summs(model_stu_mat) 

plot(model_stu_por) 


#Getting MSE
mean(model_stu_por$residuals^2)
# Check Varience inflation factors(VIF)
vif(model_stu_por) 

## Cross validation ##
cv_stu_mat<-cv.lm(data=stu_mat, model_stu_mat, m=10) 
mean(csv_stu_mat$residuals^2)


