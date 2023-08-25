library(ISLR)
library(boot)
library(MASS)
library(class)
total <- read.csv("TL_preprocessed_total.csv", header=TRUE, na.strings=c("."))
train <- read.csv("TL_preprocessed_train.csv", header=TRUE, na.strings=c("."))
test <- read.csv("TL_preprocessed_test.csv", header=TRUE, na.strings=c("."))


cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(totalyearlycompensation~poly(yearsofexperience+yearsatcompany+basesalary+stockgrantvalue+bonus+timestamp,i),data=total)
  cv.error.10[i]=cv.glm(total,glm.fit,K=10)$delta[1]
}
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(totalyearlycompensation~poly(basesalary+yearsofexperience+timestamp+yearsatcompany+stockgrantvalue+bonus,i),data=total)
  cv.error.10[i]=cv.glm(total,glm.fit,K=10)$delta[1]
}
cv.error.10

for(i in 1:10){
  glm.fit=glm(totalyearlycompensation~poly(basesalary+yearsofexperience+timestamp+yearsatcompany+stockgrantvalue+bonus+gender,i),data=total)
  cv.error.10[i]=cv.glm(total,glm.fit,K=10)$delta[1]
}



alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y) -2*cov(X,Y)))
}

alpha.fn(total,1:100)
alpha.fn(lm.fit,1:100)

lm.fit=lm(totalyearlycompensation~.,data=total)

coef(lm(totalyearlycompensation~., data=total))
alpha.fn <- function(data,index){
  return ( coef(lm(totalyearlycompensation~., data=data, subset=index)) ) 
}
alpha.fn(total, 1:100)

lm(totalyearlycompensation~., data = total, subset = train)

alpha.fn <- function(data,index){
  return ( coef(lm(totalyearlycompensation~timestamp, data=data, subset=index)) ) # estimate alpha
}
alpha.fn(total, 1:100)

alpha.fn <- function(data,index){
  return ( coef(lm(totalyearlycompensation~.-title, data=data, subset=index)) ) # estimate alpha
}
alpha.fn(total, 1:100)

alpha.fn <- function(data,index){
  return ( coef(lm(totalyearlycompensation~title, data=data, subset=index)) ) # estimate alpha
}
alpha.fn(total, 1:100)

alpha.fn <- function(data,index){
  return ( coef(lm(totalyearlycompensation~timestamp+title, data=data, subset=index)) ) # estimate alpha
}
alpha.fn(total, 1:100)

alpha.fn <- function(data,index){
  return ( coef(lm(totalyearlycompensation~timestamp+title+Education, data=data, subset=index)) ) # estimate alpha
}
alpha.fn(total, 1:100)
alpha.fn <- function(data,index){
  return ( coef(lm(totalyearlycompensation~Education, data=data, subset=index)) ) # estimate alpha
}
alpha.fn(total, 1:100)
alpha.fn <- function(data,index){
  return ( coef(lm(totalyearlycompensation~.-Education, data=data, subset=index)) ) # estimate alpha
}
alpha.fn <- function(data,index){
  return ( coef(lm(totalyearlycompensation~gender, data=data, subset=index)) ) # estimate alpha
}

alpha.fn <- function(data,index){
  return ( coef(lm(totalyearlycompensation~.-Race-Education, data=data, subset=index)) ) # estimate alpha
}
alpha.fn <- function(data,index){
  return ( coef(lm(totalyearlycompensation~Race, data=data, subset=index)) ) # estimate alpha
}
alpha.fn <- function(data,index){
  return ( coef(lm(totalyearlycompensation~timestamp+title+yearsofexperience+yearsatcompany+basesalary+stockgrantvalue+bonus, data=data, subset=index)) ) # estimate alpha
}
alpha.fn(total, 1:100)

alpha.fn(total,sample(100,100,replace=T))


set.seed(1)
alpha.fn(total,sample(100,100,replace=T))
alpha.fn(total,sample(100,100,replace=T))

boot(total,alpha.fn,R=1000)
summary (lm(totalyearlycompensation~timestamp+title+yearsofexperience+yearsatcompany+basesalary+stockgrantvalue+bonus,data=total))$coef

boot.fn=function(data ,index) return(coef(lm(totalyearlycompensation~timestamp+yearsofexperience+yearsatcompany+basesalary+stockgrantvalue+bonus,data=data,subset=train)))
boot.fn=function(data) 
  return(coef(lm(totalyearlycompensation~timestamp+yearsofexperience+yearsatcompany+basesalary+stockgrantvalue+bonus,data=total)))

boot.fn(total,1:22350)

set.seed(1)
boot.fn(total,sample(22350,22350, replace=T))
  

#의사트리
library(tree)
library(ISLR)
library(MASS)
set.seed(1)

train = sample(1:nrow(total), nrow(total)/2)
tree.total=tree(totalyearlycompensation~.,total,subset=train)
summary(tree.total)

plot(tree.total)
text(tree.total,pretty=0)

cv.total=cv.tree(tree.total)
plot(cv.total$size, cv.total$dev, type="b")

prune.total=prune.tree(tree.total,best=5)
plot(prune.total)
text(prune.total,pretty=0)

yhat=predict(tree.total, newdata=total[-train,])
total.test=total[-train,"totalyearlycompensation"]
plot(yhat,total.test)
abline(0,1)
mean((yhat-total.test)^2)

library(randomForest)
set.seed(1)
bag.total= randomForest(totalyearlycompensation~.,data=total , subset=train ,mtry=10,importance =TRUE)
bag.total

yhat.bag = predict (bag.total , newdata=total[-train ,])
plot(yhat.bag , total.test)
abline(0,1)
mean((yhat.bag-total.test)^2)

set.seed(1)
rf.total= randomForest(totalyearlycompensation~.,data=total,subset=train,mtry=6,importance =TRUE)
yhat.rf = predict(rf.total ,newdata=total[-train ,])
mean((yhat.rf-total.test)^2)

bag.total=randomForest(totalyearlycompensation~.,data=total,subset=train,mtry=10,ntree=25)
bag.total=randomForest(totalyearlycompensation~.,data=total,subset=train,mtry=10,ntree=100)
yhat.bag=predict(bag.total,newdata=total[-train,])
mean((yhat.bag-total.test)^2)

importance(rf.total)

varImpPlot(rf.total)

#부스팅
library(gbm)
set.seed(1)
boost.total=gbm(totalyearlycompensation~.,data=total[train,], distribution="gaussian",n.trees=5000,interaction.depth=4)
