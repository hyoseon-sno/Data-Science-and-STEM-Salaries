#<3장>
getwd()

#변수 출력
salary <- read.csv("TL_preprocessed_전처리완료.csv", header=T, na.strings=c("."))
names(salary)

#train, test data 분리
salary_test <- read.csv("TL_preprocessed_test.csv", header=T, na.strings=c("."))
salary_train <- read.csv("TL_preprocessed_train.csv", header=T, na.strings=c("."))

lm.fit=lm(totalyearlycompensation~.,data=salary_train)
dim(salary_train)
pred=predict(lm.fit,salary_test)

#여기서도 오류 발생해버림//./...??
lm.fit=lm(totalyearlycompensation~.-tag-location, data=salary_train)
predict(lm.fit,salary_test)

lm.fit=lm(totalyearlycompensation~., data=salary_train)
summary(lm.fit)

# maxpoint 사용해서 다 보기

#상호작용항
lm.fit2=lm(totalyearlycompensation~timestamp+gender+(timestamp*gender), data=salary_train)
summary(lm.fit2)

#비선형 변환
anova(lm.fit,lm.)



#<6장>
dim(salary)
salary=na.omit(salary)
dim(salary)
sum(is.na(salary))

library(leaps)
library(mlbench)
regfit.full=regsubsets(totalyearlycompensation~.,salary)
