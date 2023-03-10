# install.packages("MASS")
install.packages("ISLR2")
library(MASS)
library(ISLR2)

# simple linear regression
head(Boston)

# find more about the data set
?Boston

# fit a simple linear regression
# - medv: response
# - lsatat: predictor
# - syntax (y ~ x, data): y-response, x-predictor, data-data set
lm.fit <- lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat)

# get some basic information about the model
lm.fit

summary(lm.fit)

# get other pieces of information are stored in lm.fit
names(lm.fit)
coef(lm.fit)

# obtain a confidence interval for the coefficient estimates
confint(lm.fit)

# produce confidence intervals
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="confidence")

#produce prediction intervals
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="prediction")

plot(lstat, medv)
plot(lstat, medv, col="green", pch="+")
abline(lm.fit, lwd=3, col="red")

par(mfrow=c(2, 2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# -------------------------------
# MULTIPLE LINEAR REGRESSION
# all variables
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)

# all variables except age
lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)

# interaction terms
# lstat:black -> interaction term between lstat and black
# medv = lstat + age + lstat*age
lm.fit2 <- lm(medv ~ lstat * age, data = Boston)
summary(lm.fit2)
# -------------------------------

# NON-LINEAR TRANSFORMATIONS OF PREDICTORS
# I(X^2) = X^2
lmfit3 <- lm(medv ~ lstat + I(lstat^2))
summary(lmfit3)

# compare 2 models
anova(lm.fit, lmfit3)

par(mfrow = c(2, 2))
plot(lmfit3)
