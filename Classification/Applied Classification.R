# ----------------------------------------------------
# ----------------------------------------------------
#                      QUESTION 13
# ----------------------------------------------------
# ----------------------------------------------------

library(ISLR2)
summary(Weekly)

# scatterplot matrix
pairs(Weekly[, 1:8])

# correlation matrix
cor(Weekly[, 1:8])

# => as can be seen on the scatterplot and correlation matrix,
# there appears to be a positive correlation between Year and Volume
# from the summary statistic, Lag variables are very similar to each other and Today
# there doesn't appear to be any patterns except for an increase in volume from 1989 to 2001


attach(Weekly)

# Logistic regression model
lg_model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(lg_model)
# => Lag2 is statistically significant

lg_probs = predict(lg_model, type = "response")
lg_preds = rep("Down", 1089)
lg_preds[lg_probs > 0.5] <- "Up"
table(lg_preds, Direction)
# => The fraction of days where predictions are correct is (54+557)/1089 ~ 56%
# therefore, training error rate = 44%
# of (430+557) Up predictions the model makes, it is correct 557/(430+557)% of the time
# Actually, there are (48+557)/1089 Up, the model's accuracy when predicting Up is only slightly better than random guessing

# model with only Lag2, training data period from 1990 to 2008
train = (Year < 2009)
test_data = Weekly[!train, ]
test_direction = Direction[!train]
lg_model2 = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(lg_model2)

# prediction on the test set
lg_probs2 = predict(lg_model2, test_data, type = "response")
lg_preds2 = rep("Down", 104)
lg_preds2[lg_probs2 > .5] <- "Up"
table(lg_preds2, test_direction)
# => accuracy = (64) / (104) = 62.5%

# ------------------------------------------------------------
#                             LDA
# ------------------------------------------------------------
library(MASS)
lda_model <- lda(Direction ~ Lag2, data = Weekly, subset = train)
plot(lda_model)
# prediction on the test set
lda_pred = predict(lda_model, test_data)
lda_class = lda_pred$class
table(lda_class, test_direction)
# => accuracy = (9 + 56) / 106 = 62.5%

# ------------------------------------------------------------
#                             QDA
# ------------------------------------------------------------
qda_model <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda_pred = predict(qda_model, test_data)
qda_class = qda_pred$class
table(qda_class, test_direction)
# => accuracy = 61 / 104 = 58%, no better than guessing each day is Up

# ------------------------------------------------------------
#                             KNN
# ------------------------------------------------------------
library(class)
set.seed(1)
train_x = Weekly[train, 3] # Lag2 column
test_x = Weekly[!train, 3]
train_direction = Direction[train]

# changing from vector to matrix by adding dimentions
dim(train_x) = c(985, 1)
dim(test_x) = c(104, 1)

# predictions for K = 1
knn_pred = knn(train_x, test_x, train_direction, k = 1)

table(knn_pred, test_direction)
# => accuracy = (21 + 31) / 104 = 50%

# using K = 3
knn_pred3 = knn(train_x, test_x, train_direction, k = 3)
table(knn_pred3, test_direction)
# => accuracy = (16 + 42) / 104 > knn_pred

# using K = 9
knn_pred9 = knn(train_x, test_x, train_direction, k = 9)
table(knn_pred9, test_direction)
# accuracy = (17 + 41) / 104 = knn_pred3

# ----------------------------------------------------
# ----------------------------------------------------
#                      QUESTION 14
# ----------------------------------------------------
# ----------------------------------------------------
df = Auto
head(df)
df$mpg01 = NA
median_mpg = median(df$mpg)
median_mpg

dim(df) # 392 * 10
df$mpg01[df$mpg > median_mpg] <- 1
df$mpg01[df$mpg <= median_mpg] <- 0
head(df)

# function to move a column to the end of dataframe
moveToEnd = function(data, move) {
  data[c(setdiff(names(data), move), move)]
}

df = moveToEnd(df, c("name"))
head(df)
pairs(df[, 1:9])

cor(df[, 1:9])
# => mpg and mpg01 has a strong positive correlation
# cylinders, displacement, horsepower and weight have strong negative correlations with mpg01

# split the data into a trainig set and test set
dim(df)
sample <- sample(c(TRUE, FALSE), nrow(df), replace = TRUE, prob = c(0.7, 0.3))
train_data <- df[sample, ]
test_data <- df[!sample, ]
dim(train_data)
dim(test_data)

# ------------------------------------------------------------
#                             LDA
# ------------------------------------------------------------
library(MASS)
lda_fit <- lda(mpg01 ~ cylinders + displacement + horsepower + weight, data = train_data)
lda_pred <- predict(lda_fit, test_data)
prediction_labels <- lda_pred$class
true_labels <- test_data$mpg01
table(prediction_labels, true_labels)
(53 + 49) / 118
# => accuracy = (53 + 49) / 118 ~ 86.4%
(2 + 14) / 118
# error rate = 13.6%

# ------------------------------------------------------------
#                             Logistic regression
# ------------------------------------------------------------
logistic_fit <- glm(mpg01 ~ cylinders + displacement + horsepower + weight, data = train_data, family = binomial)
logistic_probs = predict(logistic_fit, test_data, type = "response")
logistic_preds <- rep(0, length(test_data$mpg01))
logistic_preds[logistic_probs > 0.5] <- 1
table(logistic_preds, true_labels)
(56 + 48) / 118
# => accuracy = (56 + 48) / 118 ~ 88.1%

# ------------------------------------------------------------
#                             Naive Bayes
# ------------------------------------------------------------
library(e1071)
nb <- naiveBayes(mpg01 ~ cylinders + displacement + horsepower + weight, data = train_data)
nb
nb_preds <- predict(nb, test_data)
mean(nb_preds == test_data$mpg01)
# => accuracy ~ 88.1% = Logistic regresssion

# ------------------------------------------------------------
#                             KNN
# ------------------------------------------------------------
library(class)
set.seed(3)
train_data_knn <- cbind(train_data$cylinders, train_data$displacement, train_data$horsepower, train_data$weight)
test_data_knn <- cbind(test_data$cylinders, test_data$displacement, test_data$horsepower, test_data$weight)
# K = 2
knn_fit1 <- knn(train_data_knn, test_data_knn, train_data$mpg01, k = 2)
mean(knn_fit1 == test_data$mpg01)
# 88.4%

# K = 3
knn_fit2 <- knn(train_data_knn, test_data_knn, train_data$mpg01, k = 3)
mean(knn_fit2 == test_data$mpg01)
# 86.8%

# K = 9
knn_fit3 <- knn(train_data_knn, test_data_knn, train_data$mpg01, k = 9)
mean(knn_fit3 == test_data$mpg01)
# 86%

# ----------------------------------------------------
# ----------------------------------------------------
#                      QUESTION 15 - Writing function
# ----------------------------------------------------
# ----------------------------------------------------

f_power = function(x, a) {
  print(x ^ a)
}

f_power(2, 3)
f_power(4, 10)

f_power1 <- function(x, a) {
  return(x ^ a)
}

par(mfrow = c(1, 1))
x <- -30 : 30
y <- f_power1(x, 2)
plot(x, y)
