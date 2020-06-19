profit <- read.csv(file.choose())
View(profit)
str(profit)
summary(profit)
#linear model
attach(profit)
set.seed(1234)
ind <- sample(2,nrow(profit),replace = T,prob = c(0.7,0.3))
train <- profit[ind==1,]
test <- profit[ind==2,]
model.linear <- lm(Profit~.,data=train)
summary(model.linear)
predict.linear <- predict(model.linear,test)
mean((predict.linear-test$Profit)^2)
mean((model.linear$residuals)^2)
la <- lm(Profit~.,data=profit)
predict1.linear <- predict(la,data=profit)
mean((la$residuals)^2)
#using ridge regression
x_train <- model.matrix(Profit~.-1,data=train)
y_train <- train$Profit
x_test <- model.matrix(Profit~.-1,data = test)
y_test <- test$Profit
library(glmnet)
ridge.model <-cv.glmnet(x_train,y_train,alpha=0,type.measure="mse",family = "gaussian")
predict.ridge <- predict(ridge.model,s=ridge.model$lambda.min,newx=x_test)
mean((predict.ridge-y_test)^2)
plot(ridge.model)
lasso.model <- cv.glmnet(x_train,y_train,alpha=1,type.measure="mse",family = "gaussian")
predict.lasso <- predict(lasso.model,s=lasso.model$lambda.min,newx=x_test)
mean((predict.lasso-y_test)^2)
plot(lasso.model)
summary(lasso.model)
SST <- sum(y_test)^2
SSE <- sum((predict.lasso-y_test)^2)
(R_SQUARE <- 1-SSE/SST)
#elastic regression
list.of.fit <- list()
for (i in 1:10) {
  fit.name <- paste0("alpha",i/10)
  list.of.fit[[fit.name]] <- cv.glmnet(x_train,y_train,alpha = i/10,type.measure="mse",family="gaussian")
}
result <- data.frame()
for (i in 1:10) {
  fit.name <- paste0("alpha",i/10)
  predicted <- predict(list.of.fit[[fit.name]],s=list.of.fit[[fit.name]]$lambda.min,newx=x_test)
  mse <- mean((predicted-y_test)^2)
  print(i/10)
  print(mse) 
  
}
SSE <- sum((predict.lasso - y_test)^2)
SST <- sum((y_test - mean(y_test))^2)
r_sq <- 1-lasso.model$cvm/var(y_train)
install.packages("ncvreg")
library(ncvreg)
model1.lasso <- cv.ncvreg(x_train,y_train,penalty="lasso")
plot(model1.lasso,type="rsq")
model2.lasso <- ncvreg(x_train,y_train,penalty="lasso")
plot(AIC(model2.lasso))
