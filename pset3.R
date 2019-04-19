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

# Correlations between main predictor and other predictors 
# Negative correxlations
plot(BostonHousing$nox~BostonHousing$zn)
plot(BostonHousing$nox~BostonHousing$dis)

# Positive correlations 
plot(BostonHousing$nox~BostonHousing$indus)
plot(BostonHousing$nox~BostonHousing$rad)
plot(BostonHousing$nox~BostonHousing$tax)
plot(BostonHousing$nox~BostonHousing$age)

# Correlations between other predictors
plot(BostonHousing$lstat~BostonHousing$age)
plot(BostonHousing$lstat~BostonHousing$rm)
plot(BostonHousing$lstat~BostonHousing$zn)

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

# PART 4

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
