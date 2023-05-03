# ------------------------------------------------------------
#                     The Validation Set Approach
# ------------------------------------------------------------

library(ISLR2)
set.seed(1)

# using sample() function to split the set of observations into 2 halves
train <- sample(392, 196)

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

attach(Auto)

# estimate MSE
mean((mpg - predict(lm.fit, Auto))[-train]^2)
# => 23.27

# use poly() function
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
# => 18.72

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
# => 18.79

# => the model that predicts mpg using a quadratic function of horsepower performs
# better than the model that involves only a linear function of horsepower

# ------------------------------------------------------------
#             Leave-One-Out Cross-Validation (LOOCV)
# ------------------------------------------------------------

# we use glm() function to perform logistic regression by passing family = "binomial"
# but if we fit a model without passing the family argument -> perform linear regression
# ex
glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)

lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
# => the same

# in the lab, we use glm() function rather than lm() because the former can be used
# together with cv.glm()
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

# ------------------------------------------------------------
#                  k-Fold Cross Validation
# ------------------------------------------------------------
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10

x <- 1:10
y <- cv.error.10
plot(x, y)

# ------------------------------------------------------------
#                  The Bootstrap
# ------------------------------------------------------------
