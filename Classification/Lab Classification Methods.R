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