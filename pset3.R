#install.packages("mlbench", repos = "http://cran.us.r-project.org");
library(mlbench)
library(ISLR);
data(BostonHousing)

names(BostonHousing)

# PART A

# Matrix summary of all variables plotted against each other.
pairs(~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat+medv, data=BostonHousing,
    main="Boston Matrix", pch=10, cex=0.1, gap=0.1);

# Plot median housing price against all relevant predictors.
plot(x=BostonHousing$crim, y=BostonHousing$medv);
plot(x=BostonHousing$zn, y=BostonHousing$medv);
plot(x=BostonHousing$indus, y=BostonHousing$medv);
plot(x=BostonHousing$chas, y=BostonHousing$medv);
plot(x=BostonHousing$nox, y=BostonHousing$medv);
plot(x=BostonHousing$nox, y=BostonHousing$medv);
plot(x=BostonHousing$rm, y=BostonHousing$medv);
plot(x=BostonHousing$age, y=BostonHousing$medv);
plot(x=BostonHousing$dis, y=BostonHousing$medv);
plot(x=BostonHousing$rad, y=BostonHousing$medv);
plot(x=BostonHousing$tax, y=BostonHousing$medv);
plot(x=BostonHousing$ptratio, y=BostonHousing$medv);
plot(x=BostonHousing$b, y=BostonHousing$medv);
plot(x=BostonHousing$lstat, y=BostonHousing$medv);

# PART B

# k-folds cross validation: generate 10 folds
folds <- cut(seq(1,nrow(BostonHousing)),breaks=10,labels=FALSE)

library(glmnet)

#x <- model.matrix(medv ~ ., data=BostonHousing)[,-1]
#y <- BostonHousing$medv

# Perform Ordinary Least Squares (lambda = 0)
#ols <- cv.glmnet(x, y, lambda=0, alpha=1, folds=10)
#coef(ols)
#summary(ols)




# Perform 10 fold cross validation
#ols1_mse = c()  # OLS using just nox variable
#ols2_mse = c()  # OLS using all relevant variables
#for(i in 1:10) {
#    #Segement data by fold using the which() function 
#    testIndexes <- which(folds==i,arr.ind=TRUE)
#    testData <- BostonHousing[testIndexes, ]
#    trainData <- BostonHousing[-testIndexes, ]
#
#    # Ordinary Least Squares to predict median housing price using nox.
#    ols1 <- lm(medv~nox, data=trainData)
#    ols1_pred <- predict(ols1, testData)
#    ols1_mse[i] <- mean((ols1_pred - testData$medv)^2)
#
#    # Ordinary Least Squares to predict median housing oprice using all variables.
#    ols2 <- lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat, data=BostonHousing)
#    ols2_pred <- predict(ols2, testData)
#    ols2_mse[i] <- mean((ols2_pred - testData$medv)^2)
#}
#mean(ols1_mse)
#mean(ols2_mse)

#install.packages("DAAG", repos="http://cran.us.r-project.org")
#library(DAAG)
library(boot)

# Ordinary Least Squares to predict median housing price using nox.
ols1 <- glm(medv~nox, data=BostonHousing)
ols1_cv <- cv.glm(BostonHousing, ols1, K=10)$delta[1]
ols1_cv

# Ordinary Least Squares to predict median housing oprice using all variables.
ols2 <- glm(medv~., data=BostonHousing)
ols2_cv <- cv.glm(BostonHousing, ols2, K=10)$delta[1]
ols2_cv
summary(ols2)

# Try a bunch of OLS with interaction terms.
ols3 <- glm(medv~nox*crim, data=BostonHousing)
ols3_cv <- cv.glm(BostonHousing, ols3, K=10)$delta[1]
ols3_cv

ols4 <- glm(medv~nox*zn, data=BostonHousing)
ols4_cv <- cv.glm(BostonHousing, ols4, K=10)$delta[1]
ols4_cv

ols5 <- glm(medv~nox*indus, data=BostonHousing)
ols5_cv <- cv.glm(BostonHousing, ols5, K=10)$delta[1]
ols5_cv

ols6 <- glm(medv~nox*chas, data=BostonHousing)
ols6_cv <- cv.glm(BostonHousing, ols6, K=10)$delta[1]
ols6_cv

# The best bivariate model.
ols7 <- glm(medv~nox*rm, data=BostonHousing)
ols7_cv <- cv.glm(BostonHousing, ols7, K=10)$delta[1]
summary(ols7)
plot(ols7)
ols7_cv

ols8 <- glm(medv~nox*age, data=BostonHousing)
ols8_cv <- cv.glm(BostonHousing, ols8, K=10)$delta[1]
ols8_cv

ols9 <- glm(medv~nox*dis, data=BostonHousing)
ols9_cv <- cv.glm(BostonHousing, ols9, K=10)$delta[1]
ols9_cv

ols10 <- glm(medv~nox*rad, data=BostonHousing)
ols10_cv <- cv.glm(BostonHousing, ols10, K=10)$delta[1]
ols10_cv

ols11 <- glm(medv~nox*tax, data=BostonHousing)
ols11_cv <- cv.glm(BostonHousing, ols11, K=10)$delta[1]
ols11_cv

ols12 <- glm(medv~nox*ptratio, data=BostonHousing)
ols12_cv <- cv.glm(BostonHousing, ols12, K=10)$delta[1]
ols12_cv

ols13 <- glm(medv~nox*b, data=BostonHousing)
ols13_cv <- cv.glm(BostonHousing, ols13, K=10)$delta[1]
ols13_cv

# The second best bivariate model
ols14 <- glm(medv~nox*lstat, data=BostonHousing)
ols14_cv <- cv.glm(BostonHousing, ols14, K=10)$delta[1]
summary(ols14)
plot(ols14)
ols14_cv

# When testing interaction terms with nox, the predictors lstat and rm are very effective.
ols15 <- glm(medv~lstat, data=BostonHousing)
ols15_cv <- cv.glm(BostonHousing, ols15, K=10)$delta[1]
ols15_cv

ols16 <- glm(medv~rm, data=BostonHousing)
ols16_cv <- cv.glm(BostonHousing, ols16, K=10)$delta[1]
ols16_cv

## SPLINES BEGIN HERE ## 

# Nox & medv natural spline with 1 knot 
fit_1knot <- glm(medv ~ ns(nox,df=1),data=BostonHousing)
newdata <- data.frame(nox=seq(min(BostonHousing$nox), max(BostonHousing$nox),0.01))
plot(BostonHousing$nox,BostonHousing$medv)
newdata$predictions_knot1 <- predict(fit_1knot, newdata)
lines(newdata$nox, newdata$predictions_knot1, col = "blue")

# Nox & medv natural spline with 2 knots
fit_2knot <- glm(medv ~ ns(nox,df=2),data=BostonHousing)
plot(BostonHousing$nox,BostonHousing$medv)
newdata$predictions_knot2 <- predict(fit_2knot, newdata)
lines(newdata$nox, newdata$predictions_knot2, col = "blue")

# Nox & medv natural spline with 3 knots
fit_3knot <- glm(medv ~ ns(nox,df=3),data=BostonHousing)
plot(BostonHousing$nox,BostonHousing$medv)
newdata$predictions_knot3 <- predict(fit_3knot, newdata)
lines(newdata$nox, newdata$predictions_knot3, col = "blue")

# Nox & medv natural spline with 4 knots
fit_4knot <- glm(medv ~ ns(nox,df=4),data=BostonHousing)
plot(BostonHousing$nox,BostonHousing$medv)
newdata$predictions_knot4 <- predict(fit_4knot, newdata)
lines(newdata$nox, newdata$predictions_knot4, col = "blue")

# Nox & medv natural spline with 5 knots
fit_5knot <- glm(medv ~ ns(nox,df=5),data=BostonHousing)
plot(BostonHousing$nox,BostonHousing$medv)
newdata$predictions_knot5 <- predict(fit_5knot, newdata)
lines(newdata$nox, newdata$predictions_knot5, col = "blue")

# Get CV accuracy of all nox splines 
cv_error1 = cv.glm(BostonHousing, fit_1knot, K=10)$delta[1]
cv_error1

cv_error2 = cv.glm(BostonHousing, fit_2knot, K=10)$delta[1]
cv_error2

cv_error3 = cv.glm(BostonHousing, fit_3knot, K=10)$delta[1]
cv_error3

cv_error4 = cv.glm(BostonHousing, fit_4knot, K=10)$delta[1]
cv_error4

cv_error5 = cv.glm(BostonHousing, fit_5knot, K=10)$delta[1]
cv_error5

# lstat & medv natural spline with 1 knot 
fit_1knot <- glm(medv ~ ns(lstat,df=1),data=BostonHousing)
newdata <- data.frame(lstat=seq(min(BostonHousing$lstat), max(BostonHousing$lstat),0.1))
plot(BostonHousing$lstat,BostonHousing$medv)
newdata$predictions_knot1 <- predict(fit_1knot, newdata)
lines(newdata$lstat, newdata$predictions_knot1, col = "blue")

# lstat & medv natural spline with 2 knots
fit_2knot <- glm(medv ~ ns(lstat,df=2),data=BostonHousing)
plot(BostonHousing$lstat,BostonHousing$medv)
newdata$predictions_knot2 <- predict(fit_2knot, newdata)
lines(newdata$lstat, newdata$predictions_knot2, col = "blue")

# lstat & medv natural spline with 3 knots
fit_3knot <- glm(medv ~ ns(lstat,df=3),data=BostonHousing)
plot(BostonHousing$lstat,BostonHousing$medv)
newdata$predictions_knot3 <- predict(fit_3knot, newdata)
lines(newdata$lstat, newdata$predictions_knot3, col = "blue")

# lstat & medv natural spline with 4 knots
fit_4knot <- glm(medv ~ ns(lstat,df=4),data=BostonHousing)
plot(BostonHousing$lstat,BostonHousing$medv)
newdata$predictions_knot4 <- predict(fit_4knot, newdata)
lines(newdata$lstat, newdata$predictions_knot4, col = "blue")

# lstat & medv natural spline with 5 knots
fit_5knot <- glm(medv ~ ns(lstat,df=5),data=BostonHousing)
plot(BostonHousing$lstat,BostonHousing$medv)
newdata$predictions_knot5 <- predict(fit_5knot, newdata)
lines(newdata$lstat, newdata$predictions_knot5, col = "blue")

# Get CV accuracy of all lstat splines 
cv_error1 = cv.glm(BostonHousing, fit_1knot, K=10)$delta[1]
cv_error1

cv_error2 = cv.glm(BostonHousing, fit_2knot, K=10)$delta[1]
cv_error2

cv_error3 = cv.glm(BostonHousing, fit_3knot, K=10)$delta[1]
cv_error3

# Lstat & medv with 4 knots has best CV accuracy score
cv_error4 = cv.glm(BostonHousing, fit_4knot, K=10)$delta[1]
cv_error4

cv_error5 = cv.glm(BostonHousing, fit_5knot, K=10)$delta[1]
cv_error5

# rm & medv natural spline with 1 knot 
fit_1knot <- glm(medv ~ ns(rm,df=1),data=BostonHousing)
newdata <- data.frame(rm=seq(min(BostonHousing$rm), max(BostonHousing$rm),0.01))
plot(BostonHousing$rm,BostonHousing$medv)
newdata$predictions_knot1 <- predict(fit_1knot, newdata)
lines(newdata$rm, newdata$predictions_knot1, col = "blue")

# rm & medv natural spline with 2 knots
fit_2knot <- glm(medv ~ ns(rm,df=2),data=BostonHousing)
plot(BostonHousing$rm,BostonHousing$medv)
newdata$predictions_knot2 <- predict(fit_2knot, newdata)
lines(newdata$rm, newdata$predictions_knot2, col = "blue")

# rm & medv natural spline with 3 knots
fit_3knot <- glm(medv ~ ns(rm,df=3),data=BostonHousing)
plot(BostonHousing$rm,BostonHousing$medv)
newdata$predictions_knot3 <- predict(fit_3knot, newdata)
lines(newdata$rm, newdata$predictions_knot3, col = "blue")

# rm & medv natural spline with 4 knots
fit_4knot <- glm(medv ~ ns(rm,df=4),data=BostonHousing)
plot(BostonHousing$rm,BostonHousing$medv)
newdata$predictions_knot4 <- predict(fit_4knot, newdata)
lines(newdata$rm, newdata$predictions_knot4, col = "blue")

# rm & medv natural spline with 5 knots
fit_5knot <- glm(medv ~ ns(rm,df=5),data=BostonHousing)

# Get CV accuracy of all rm splines 
cv_error1 = cv.glm(BostonHousing, fit_1knot, K=10)$delta[1]
cv_error1

cv_error2 = cv.glm(BostonHousing, fit_2knot, K=10)$delta[1]
cv_error2

cv_error3 = cv.glm(BostonHousing, fit_3knot, K=10)$delta[1]
cv_error3

cv_error4 = cv.glm(BostonHousing, fit_4knot, K=10)$delta[1]
cv_error4

cv_error5 = cv.glm(BostonHousing, fit_5knot, K=10)$delta[1]
cv_error5

# b & medv natural spline with 1 knot 
fit_1knot <- glm(medv ~ ns(b,df=1),data=BostonHousing)
newdata <- data.frame(b=seq(min(BostonHousing$b), max(BostonHousing$b),0.1))
plot(BostonHousing$b,BostonHousing$medv)
newdata$predictions_knot1 <- predict(fit_1knot, newdata)
lines(newdata$b, newdata$predictions_knot1, col = "blue")

# b & medv natural spline with 2 knots
fit_2knot <- glm(medv ~ ns(b,df=2),data=BostonHousing)
plot(BostonHousing$b,BostonHousing$medv)
newdata$predictions_knot2 <- predict(fit_2knot, newdata)
lines(newdata$b, newdata$predictions_knot2, col = "blue")

# b & medv natural spline with 3 knots
fit_3knot <- glm(medv ~ ns(b,df=3),data=BostonHousing)
plot(BostonHousing$b,BostonHousing$medv)
newdata$predictions_knot3 <- predict(fit_3knot, newdata)
lines(newdata$b, newdata$predictions_knot3, col = "blue")

# b & medv natural spline with 4 knots
fit_4knot <- glm(medv ~ ns(b,df=4),data=BostonHousing)
plot(BostonHousing$b,BostonHousing$medv)
newdata$predictions_knot4 <- predict(fit_4knot, newdata)
lines(newdata$b, newdata$predictions_knot4, col = "blue")

# 5 knots causes error: Error in qr.default(t(const))
# b medv natural spline with 5 knots
# fit_5knot <- glm(medv ~ ns(b,df=5),data=BostonHousing)

# Get CV accuracy of all rm splines 
cv_error1 = cv.glm(BostonHousing, fit_1knot, K=10)$delta[1]
cv_error1

cv_error2 = cv.glm(BostonHousing, fit_2knot, K=10)$delta[1]
cv_error2

cv_error3 = cv.glm(BostonHousing, fit_3knot, K=10)$delta[1]
cv_error3

cv_error4 = cv.glm(BostonHousing, fit_4knot, K=10)$delta[1]
cv_error4

# indus & medv natural spline with 1 knot 
fit_1knot <- glm(medv ~ ns(indus,df=1),data=BostonHousing)
newdata <- data.frame(indus=seq(min(BostonHousing$indus), max(BostonHousing$indus),0.1))
plot(BostonHousing$indus,BostonHousing$medv)
newdata$predictions_knot1 <- predict(fit_1knot, newdata)
lines(newdata$indus, newdata$predictions_knot1, col = "blue")

# indus & medv natural spline with 2 knots
fit_2knot <- glm(medv ~ ns(indus,df=2),data=BostonHousing)
plot(BostonHousing$indus,BostonHousing$medv)
newdata$predictions_knot2 <- predict(fit_2knot, newdata)
lines(newdata$indus, newdata$predictions_knot2, col = "blue")

# indus & medv natural spline with 3 knots
fit_3knot <- glm(medv ~ ns(indus,df=3),data=BostonHousing)
plot(BostonHousing$indus,BostonHousing$medv)
newdata$predictions_knot3 <- predict(fit_3knot, newdata)
lines(newdata$indus, newdata$predictions_knot3, col = "blue")

# indus & medv natural spline with 4 knots
fit_4knot <- glm(medv ~ ns(indus,df=4),data=BostonHousing)
plot(BostonHousing$indus,BostonHousing$medv)
newdata$predictions_knot4 <- predict(fit_4knot, newdata)
lines(newdata$indus, newdata$predictions_knot4, col = "blue")

# indus & medv natural spline with 5 knots
fit_5knot <- glm(medv ~ ns(indus,df=5),data=BostonHousing)
plot(BostonHousing$indus,BostonHousing$medv)
newdata$predictions_knot5 <- predict(fit_5knot, newdata)
lines(newdata$indus, newdata$predictions_knot5, col = "blue")

# Get CV accuracy of all indus splines 
cv_error1 = cv.glm(BostonHousing, fit_1knot, K=10)$delta[1]
cv_error1

cv_error2 = cv.glm(BostonHousing, fit_2knot, K=10)$delta[1]
cv_error2

cv_error3 = cv.glm(BostonHousing, fit_3knot, K=10)$delta[1]
cv_error3

cv_error4 = cv.glm(BostonHousing, fit_4knot, K=10)$delta[1]
cv_error4

cv_error5 = cv.glm(BostonHousing, fit_5knot, K=10)$delta[1]
cv_error5

## SPLINES END HERE ## 

## TRANSFORMED PREDICTORS BEGIN HERE ##
# Squared predictors #
fit_crim_sq <- glm(medv ~ crim^2,data=BostonHousing)
newdata <- data.frame(crim=seq(min(BostonHousing$crim), max(BostonHousing$crim),0.1))
plot(BostonHousing$crim^2,BostonHousing$medv)
newdata$predictions_crim_sq <- predict(fit_crim_sq, newdata)
lines(newdata$crim^2, newdata$predictions_crim_sq, col = "blue")

fit_indus_sq <- glm(medv ~ indus^2,data=BostonHousing)
newdata <- data.frame(indus=seq(min(BostonHousing$indus), max(BostonHousing$indus),0.1))
plot(BostonHousing$indus^2,BostonHousing$medv)
newdata$predictions_indus_sq <- predict(fit_indus_sq, newdata)
lines(newdata$indus^2, newdata$predictions_indus_sq, col = "blue")

###
# Polynomial binary predictors are hard 
# fit_chas_sq <- glm(medv ~ chas^2,data=BostonHousing)
# newdata <- data.frame(chas=seq(min(BostonHousing$chas), max(BostonHousing$chas),0.1))
# plot(BostonHousing$chas^2,BostonHousing$medv)
# newdata$predictions_chas_sq <- predict(fit_chas_sq, newdata)
# lines(newdata$chas^2, newdata$predictions_chas_sq, col = "blue")

fit_rm_sq <- glm(medv ~ rm^2,data=BostonHousing)
newdata <- data.frame(rm=seq(min(BostonHousing$rm), max(BostonHousing$rm),0.1))
plot(BostonHousing$rm^2,BostonHousing$medv)
newdata$predictions_rm_sq <- predict(fit_rm_sq, newdata)
lines(newdata$rm^2, newdata$predictions_rm_sq, col = "blue")

fit_age_sq <- glm(medv ~ age^2,data=BostonHousing)
newdata <- data.frame(age=seq(min(BostonHousing$age), max(BostonHousing$age),0.1))
plot(BostonHousing$age^2,BostonHousing$medv)
newdata$predictions_age_sq <- predict(fit_age_sq, newdata)
lines(newdata$age^2, newdata$predictions_age_sq, col = "blue")

fit_dis_sq <- glm(medv ~ dis^2,data=BostonHousing)
newdata <- data.frame(dis=seq(min(BostonHousing$dis), max(BostonHousing$dis),0.1))
plot(BostonHousing$dis^2,BostonHousing$medv)
newdata$predictions_dis_sq <- predict(fit_dis_sq, newdata)
lines(newdata$dis^2, newdata$predictions_dis_sq, col = "blue")

fit_rad_sq <- glm(medv ~ rad^2,data=BostonHousing)
newdata <- data.frame(rad=seq(min(BostonHousing$rad), max(BostonHousing$rad),0.1))
plot(BostonHousing$rad^2,BostonHousing$medv)
newdata$predictions_rad_sq <- predict(fit_rad_sq, newdata)
lines(newdata$rad^2, newdata$predictions_rad_sq, col = "blue")

fit_tax_sq <- glm(medv ~ tax^2,data=BostonHousing)
newdata <- data.frame(tax=seq(min(BostonHousing$tax), max(BostonHousing$tax),0.1))
plot(BostonHousing$tax^2,BostonHousing$medv)
newdata$predictions_tax_sq <- predict(fit_tax_sq, newdata)
lines(newdata$tax^2, newdata$predictions_tax_sq, col = "blue")

fit_ptratio_sq <- glm(medv ~ ptratio^2,data=BostonHousing)
newdata <- data.frame(ptratio=seq(min(BostonHousing$ptratio), max(BostonHousing$ptratio),0.1))
plot(BostonHousing$ptratio^2,BostonHousing$medv)
newdata$predictions_ptratio_sq <- predict(fit_ptratio_sq, newdata)
lines(newdata$ptratio^2, newdata$predictions_ptratio_sq, col = "blue")

fit_b_sq <- glm(medv ~ b^2,data=BostonHousing)
newdata <- data.frame(b=seq(min(BostonHousing$b), max(BostonHousing$b),0.1))
plot(BostonHousing$b^2,BostonHousing$medv)
newdata$predictions_b_sq <- predict(fit_b_sq, newdata)
lines(newdata$b^2, newdata$predictions_b_sq, col = "blue")

fit_lstat_sq <- glm(medv ~ lstat^2,data=BostonHousing)
newdata <- data.frame(lstat=seq(min(BostonHousing$lstat), max(BostonHousing$lstat),0.1))
plot(BostonHousing$lstat^2,BostonHousing$medv)
newdata$predictions_lstat_sq <- predict(fit_lstat_sq, newdata)
lines(newdata$lstat^2, newdata$predictions_lstat_sq, col = "blue")

# Get CV MSE of all squared predictors
cv_error = cv.glm(BostonHousing, fit_crim_sq, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_indus_sq, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_rm_sq, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_age_sq, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_dis_sq, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_rad_sq, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_tax_sq, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_ptratio_sq, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_b_sq, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_lstat_sq, K=10)$delta[1]
cv_error

# Log predictors # 
fit_crim_log <- glm(medv ~ log(crim),data=BostonHousing)
newdata <- data.frame(crim=seq(min(BostonHousing$crim), max(BostonHousing$crim),0.1))
plot(log(BostonHousing$crim),BostonHousing$medv)
newdata$predictions_crim_log <- predict(fit_crim_log, newdata)
lines(log(newdata$crim), newdata$predictions_crim_log, col = "blue")

fit_indus_log <- glm(medv ~ log(indus),data=BostonHousing)
newdata <- data.frame(indus=seq(min(BostonHousing$indus), max(BostonHousing$indus),0.1))
plot(log(BostonHousing$indus),BostonHousing$medv)
newdata$predictions_indus_log <- predict(fit_indus_log, newdata)
lines(log(newdata$indus), newdata$predictions_indus_log, col = "blue")

###
# Polynomial binary predictors are hard 
# fit_chas_sq <- glm(medv ~ chas^2,data=BostonHousing)
# newdata <- data.frame(chas=seq(min(BostonHousing$chas), max(BostonHousing$chas),0.1))
# plot(BostonHousing$chas^2,BostonHousing$medv)
# newdata$predictions_chas_sq <- predict(fit_chas_sq, newdata)
# lines(newdata$chas^2, newdata$predictions_chas_sq, col = "blue")

fit_rm_log <- glm(medv ~ log(rm),data=BostonHousing)
newdata <- data.frame(rm=seq(min(BostonHousing$rm), max(BostonHousing$rm),0.1))
plot(log(BostonHousing$rm),BostonHousing$medv)
newdata$predictions_rm_log <- predict(fit_rm_log, newdata)
lines(log(newdata$rm), newdata$predictions_rm_log, col = "blue")

fit_age_log <- glm(medv ~ log(age),data=BostonHousing)
newdata <- data.frame(age=seq(min(BostonHousing$age), max(BostonHousing$age),0.1))
plot(log(BostonHousing$age),BostonHousing$medv)
newdata$predictions_age_log <- predict(fit_age_log, newdata)
lines(log(newdata$age), newdata$predictions_age_log, col = "blue")

fit_dis_log <- glm(medv ~ log(dis),data=BostonHousing)
newdata <- data.frame(dis=seq(min(BostonHousing$dis), max(BostonHousing$dis),0.1))
plot(log(BostonHousing$dis),BostonHousing$medv)
newdata$predictions_dis_log <- predict(fit_dis_log, newdata)
lines(log(newdata$dis), newdata$predictions_dis_log, col = "blue")

fit_rad_log <- glm(medv ~ log(rad),data=BostonHousing)
newdata <- data.frame(rad=seq(min(BostonHousing$rad), max(BostonHousing$rad),0.1))
plot(log(BostonHousing$rad),BostonHousing$medv)
newdata$predictions_rad_log <- predict(fit_rad_log, newdata)
lines(log(newdata$rad), newdata$predictions_rad_log, col = "blue")

fit_tax_log <- glm(medv ~ log(tax),data=BostonHousing)
newdata <- data.frame(tax=seq(min(BostonHousing$tax), max(BostonHousing$tax),0.1))
plot(log(BostonHousing$tax),BostonHousing$medv)
newdata$predictions_tax_log <- predict(fit_tax_log, newdata)
lines(log(newdata$tax), newdata$predictions_tax_log, col = "blue")

fit_ptratio_log <- glm(medv ~ log(ptratio),data=BostonHousing)
newdata <- data.frame(ptratio=seq(min(BostonHousing$ptratio), max(BostonHousing$ptratio),0.1))
plot(log(BostonHousing$ptratio),BostonHousing$medv)
newdata$predictions_ptratio_log <- predict(fit_ptratio_log, newdata)
lines(log(newdata$ptratio), newdata$predictions_ptratio_log, col = "blue")

fit_b_log <- glm(medv ~ log(b),data=BostonHousing)
newdata <- data.frame(b=seq(min(BostonHousing$b), max(BostonHousing$b),0.1))
plot(log(BostonHousing$b),BostonHousing$medv)
newdata$predictions_b_log <- predict(fit_b_log, newdata)
lines(log(newdata$b), newdata$predictions_b_log, col = "blue")

fit_lstat_log <- glm(medv ~ log(lstat),data=BostonHousing)
newdata <- data.frame(lstat=seq(min(BostonHousing$lstat), max(BostonHousing$lstat),0.1))
plot(log(BostonHousing$lstat),BostonHousing$medv)
newdata$predictions_lstat_log <- predict(fit_lstat_log, newdata)
lines(log(newdata$lstat), newdata$predictions_lstat_log, col = "blue")

# Get CV MSE of all squared predictors
cv_error = cv.glm(BostonHousing, fit_crim_log, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_indus_log, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_rm_log, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_age_log, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_dis_log, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_rad_log, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_tax_log, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_ptratio_log, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_b_log, K=10)$delta[1]
cv_error

cv_error = cv.glm(BostonHousing, fit_lstat_log, K=10)$delta[1]
cv_error

# PART 3
# Combine nox with best models (5 knot rm spline & 4 knot lstat spline & relationship betwee nox*rm)
best_model <- glm(medv~nox+ns(rm,df=5)+ns(lstat,df=4)+(nox*rm), data=BostonHousing)
summary(best_model)
cv_error = cv.glm(BostonHousing, best_model, K=10)$delta[1]
cv_error

xx <- names(BostonHousing)[c(1:1, 3:4, 6:13)]
xx

# Remove sparse rows
BostonHousing <- na.omit(BostonHousing)

for (i in xx) {
    new_name <- i
    old_name <- paste("BostonHousing$",i, sep="")
    assign(new_name, eval(parse(text=paste("as.numeric(",old_name,")",sep=""))))
}

for (i in xx) {
    new_name <- paste(i, "_sq", sep="") 
    old_name <- paste("BostonHousing$", i,sep="")
    assign(new_name, eval(parse(text=paste("as.numeric(",old_name,")^2",sep=""))))
}

for (i in xx) {
    new_name <- paste(i, "_log", sep="") 
    old_name <- paste("BostonHousing$", i,sep="")
    assign(new_name, eval(parse(text=paste("log(as.numeric(",old_name,"))",sep=""))))
}

controls <- xx
for (j in xx) {
    controls <- c(controls,paste(j,"_sq",sep=""))
    controls <- c(controls,paste(j,"_log",sep=""))
}
controls

x <- as.numeric(BostonHousing$nox)
mod <- paste(paste("model.matrix(~x+",paste(controls,collapse="+")),",data=BostonHousing)",sep="")
assign("model_z", eval(parse(text=mod)))
model_z
mod

# PART 4

y <- BostonHousing$medv

ylasso <- cv.glmnet(model_z,y,alpha=1)
y.z <- coef(ylasso, ylasso$lambda.min)
y.z
plot(ylasso)
ylasso$lambda.min
#ylasso


# PART 5

summary(BostonHousing$nox)
library(devtools)
install_github("susanathey/causalTree")
library(causalTree)

tree <- causalTree(medv ~ ., data=BostonHousing, treatment=I(BostonHousing$nox>=0.5380), split.Rule="CT", cv.option="CT", split.Honest=T, cv.Honest=T, split.Bucket=F, xval=5, cp=0, minsize=5, propensity=0.5)
summary(BostonHousing$nox)

opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]
opfit <- prune(tree, opcp)
rpart.plot(opfit)
