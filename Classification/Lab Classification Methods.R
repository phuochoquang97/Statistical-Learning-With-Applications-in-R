# understand the data

library(ISLR2)
names(Smarket)
dim(Smarket)
summary(Smarket)

# plot matrix, consisting of scatterplots for each variable-combination
pairs(Smarket)

# produce correlation matrix, except the last column as it is qualitative
cor(Smarket[, -9])

attach(Smarket)
plot(Volume)

# ---------------------------

# logistic regression
# predict Direction using Lag1->Lag5 + Volume
# glm(): fit many generalized models, including logistic regression
# glm() ~ lm(), except must pass family = binomial
glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fits)

# access the coefficients for this fitted model
# this is a part of summary(glm.fits :3)
coef(glm.fits)

# or we can access coef like this
# summary(glm.fits)$coef

# predict
glm.probs <- predict(glm.fits, type = "response") # no data set -> use the training data
glm.probs[1:10]

contrasts(Direction)

# create a vector of 1250 Down elements
glm.pred <- rep("Down", 1250)

# transform to Up all of elements with prob > 0.5
glm.pred[glm.probs > 0.5] = "Up"

# product a confusion matrix
table(glm.pred, Direction)

(145 + 507) / 1250
mean(glm.pred == Direction)

# create a vector corresponding to the observations from 2001 through 2004
# then use this vector to create a held out data set of observations from 2005
train <- (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
head(Smarket.2005)
Direction.2005 <- Direction[!train]

# fit a logistic regression model using only the subset
# of observations thay correspond to dates before 2005, using the subset argument

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
summary(glm.fits)

glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.2005)

# accuracy for prediction data Smarket.2005
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

# => Disappointing results :v

# now we build a model with only Lag1 and Lag2
glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
summary(glm.fits)

glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs> 0.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
# => the accuracy is enhanced

# ------------------------------
# LDA
# import lib for lda
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
# => predict function returns a list with 3 elements
# class: contains LDA's predictions about the movement of the market
# posterior: a matrix whose kth column contains the posterior probability
# that corresponding observation belongs to the kth class
# x: contains the linear discriminant

table(lda.pred$class, Direction.2005)
mean(lda.pred$class == Direction.2005)


# ---------------------------
# SKIP QDA @@
# ---------------------------

# Naive Bayes
# import lib to use naive bayes
# install.packages(e1071)
library(e1071)
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
nb.fit
# => Conditional probabilities: (comment a block Ctrl Shift C)
# Lag1
# Y             [,1]     [,2]
# Down  0.04279022 1.227446
# Up   -0.03954635 1.231668
#
# Lag2
# Y             [,1]     [,2]
# Down  0.03389409 1.239191
# Up   -0.03132544 1.220765

# => Interpretation:
# for direction = Down in Lag1, mean ~ 0.0428, std ~ 1.2274, similar to other cases

# we can verify this
mean(Lag1[train][Direction[train] == "Down"])
sd(Lag1[train][Direction[train] == "Down"])

nb.pred <- predict(nb.fit, Smarket.2005)
table(nb.pred, Direction.2005)
mean(nb.pred == Direction.2005)
# => LDA < accuracy ~ 59% < QDA

# generate estimates of the prob that each observation belongs to a particular class
nb.preds <- predict(nb.fit, Smarket.2005, type = "raw")
nb.preds[1:5, ]


# ---------------------
# K-nearest neighbors
library(class)
train.X <- cbind(Lag1, Lag2)[train, ]
dim(train.X)
test.X <- cbind(Lag1, Lag2)[!train, ]
dim(test.X)
train.Direction <- Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)
# => 50%, K = 1 results in an overly flexible fit to data

knn.pred3 <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred3, Direction.2005)
mean(knn.pred3 == Direction.2005)
# => 0.536, improved slightly

# ---------------------------------------------
# Now we use another data set, Caravan from ISLR2 with KNN
dim(Caravan)
attach(Caravan)
summary(Caravan)

# because KNN predicts class of a given test observation by identifying the observations
# that are nearest to it
# => the scale of variables matters
# => standardize the data
# => mean = 0 and sd = 1
standardize.X <- scale(Caravan[, -86])

# now let compare variance before and afer standardizing
var(Caravan[, 1])
var(Caravan[, 2])
var(standardize.X[, 1])
var(standardize.X[, 2])

# split the observations into a test set and a training set
test <- 1:1000
train.X <- standardize.X[-test, ]
test.X <- standardize.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
dim(train.X) # 4822,85
dim(test.X) # 1000, 85
length(train.Y) # 4822
length(test.Y) # 1000

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")

table(knn.pred, test.Y)
# => if sell randomly -> only ~6% customers purchase
# => if sell to customers who are likely to purchase => 9/(9+68) ~ 11.7%

# using K = 3
knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)
# => 5 / (5 + 21) ~ 0.192

# using K = 5
knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
# => 4 / (4 + 11) ~ 0.267

# use logistic regression
glm.fits <- glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fits, Caravan[test, ], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > 0.25] <- "Yes"
table(glm.pred, test.Y)
# => 11 / (11 + 22) ~ 0.333, better than random guessing


# -----------------------------
# Poisson regression
# using Bikeshare data set from ISLR2
attach(Bikeshare)
dim(Bikeshare)
summary(Bikeshare)
