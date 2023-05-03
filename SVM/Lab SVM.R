# ------------------------------------------------------------
#             Support Vector Classifier
# ------------------------------------------------------------

set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = (3 - y))

# create a data frame with the response coded as factor
dat <- data.frame(x = x, y = as.factor(y))
library(e1071)

# ------------------------------------------------------------
#                  cost = 10
# ------------------------------------------------------------

svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale= FALSE)
plot(svmfit, dat)

# determine the support vectors
svmfit$index

# obtain some basic info
summary(svmfit)

# ------------------------------------------------------------
#                  cost = 0.1
# ------------------------------------------------------------
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit, dat)
# => obtain a large number of support vectors because the margin is now wider

# ------------------------------------------------------------
#                  tune() for cross-validation
# ------------------------------------------------------------

set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)
plot(bestmod, dat)

# predict the class label

xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)

xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)


# test with cost = 0.01
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.01, scale = FALSE)
ypred <- predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)
# => 6 observations are misclassified

par(mfrow = c(2, 1))
x[y == 1, ] <- x[y == 1, ] + 0.5
plot(x, col = (y + 5) / 2, pch = 19)
# => observations are just barely linearly separable
dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat)

# ------------------------------------------------------------
# ------------------------------------------------------------
#             Support Vector Machine
# ------------------------------------------------------------
# ------------------------------------------------------------
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
plot(x, col = y)

# randomly split data into training and testing
train <- sample(200, 100)
train
svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train, ])
summary(svmfit)

# perform cross-validation
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat[train, ], kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)
# => best parameters: cost = 1, gamma = 2

table(true = dat[-train, "y"], pred = predict(tune.out$best.model, newdata = dat[-train, ]))

# ------------------------------------------------------------
# ------------------------------------------------------------
#             ROC Curves
# ------------------------------------------------------------
# ------------------------------------------------------------
# require(devtools)
# install_version(
#   "ROCR",
#   version = "4.2.2",
#   dependencies = TRUE)
# 
#   
# 
# library(ROCR)
# rocplot <- function(pred, truth, ...) {
#   predob <- prediction(pred, truth)
#   perf <- performance(predob, "tpr", "fpr")
#   plot(perf, ...)
# }
# 
# svmfit.opt <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 2, cost = 1, decision.values = T)
# fitted <- attributes(predict(svmfit.opt, dat[train, ], decision.values = TRUE))$decision.values
# par(mfrow = c(1, 2))
# rocplot(-fitted, dat[train, "y"], main = "Training Data")
