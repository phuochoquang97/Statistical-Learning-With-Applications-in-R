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
confint(lm.fit)
