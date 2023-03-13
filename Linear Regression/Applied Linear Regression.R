#PROBLEM 1

# load the library for Auto dataset
library(ISLR)
# fix(Auto)
data(Auto)
mpg_pwr = lm(mpg ~ horsepower, data = Auto)
summary(mpg_pwr)
# => There is a strong relationship between mpg and horsepower as the p-value
# for horsepower's coefficient is close to 0


# R^2 statistic is 0.61 -> 60% of variance in mpg can be explained by horsepower
mean(Auto$mpg, na.rm = T)
# RSE = residual standard error
4.906/mean(Auto$mpg, na.rm = T)
# => the relationship betwwen mqp and horsepower is reasonably strong

predict(mpg_pwr, data.frame(horsepower = c(98)), interval = "prediction")
predict(mpg_pwr, data.frame(horsepower = c(98)), interval = "confidence")

# plot response and predictor with least squares regression line
plot(mpg ~ horsepower, main = "Scatter plot of mpg vs horsepower", data = Auto)
abline(mpg_pwr, lwd = 3, col = "red")

# produce diagnostic plots of least squares regression fit
par(mfrow = c(2, 2))
plot(mpg_pwr)

rstudent(mpg_pwr)[which(rstudent(mpg_pwr) > 3)]
# -----------------------------------

# PROBLEM 2
head(Auto)
# produce a scatter matrix which includes all of the variables
pairs(Auto[, 1:8])

# compute the matrix of correlations between the all variables
cor(Auto[, 1:8])

lmfit5 = lm(mpg ~ . -name, data = Auto)
summary(lmfit5)
# comment
# - p-value < 2.2e016 for a F-statistic of 252.4 => significant evidence of a relationship between the predictors and the mpg
# - displacement, weight, year and origin are statistically significant as their p-values are below 0.05 or ~0
# - increasing year by 1 will make an increase of 0.75 in mpg, assuming all other predictors are kept constant

# produce diagnostic plots of the linear regression fit
par(mfrow = c(2, 2))
plot(lmfit5)

mpg_interaction <- lm(mpg ~ . - name + year:cylinders + acceleration:horsepower, data = Auto)
summary(mpg_interaction)
# => Multiple R-squared increases from 0.82 to 0.85

mpg_poly <- lm(mpg ~ . -name + year:cylinders + I(horsepower^2) + I(acceleration^2), data = Auto)
summary(mpg_poly)
# => Multiple R-squared increases from 0.82 to 0.8622

mpg_poly2 <- lm(mpg ~ . -name + log(weight) + log(acceleration) + sqrt(displacement), data = Auto)
summary(mpg_poly2)
# => Multiple R-squared increase from 0.82 to 0.864
par(mfrow = c(2, 2))
plot(mpg_poly2)


# ----------------------------
# PROBLEM 3
library(ISLR)
head(Carseats)
carseats_lm <- lm(Sales ~ Price + Urban + US, data = Carseats) # model A
summary(carseats_lm)

# interpretation
# Price coefficient is negative so sales will fall by roughly -0.054 for every unit increase in price
# Urban=YES coefficient is not statistically significant
# US=YES coefficient means an increase unit in US=YES will make Sales increase 1.2

attach(Carseats)
contrasts(US)
contrasts(Urban)
# the model in equation form
# sales = 13.04 -0.05Price - 0.02Urban(YES:1, NO:0) + 1.2US(YES:1, NO: 0)


# for which of the predictors I can reject null hypothesis
car_seats_all <- lm(Sales ~ ., data = Carseats)
summary(car_seats_all)
# => Null hypothesis can be rejected for CompPrice, Income, Advertising, Price, ShelveLocGood, ShelveLocMedium, Age

car_seats_all_lm <- lm(Sales ~ . -Population -Education -Urban -US, data = Carseats)
summary(car_seats_all_lm) # model E

# how well model A and model E fit the data
summary(carseats_lm)
# RSE goes down from 2.472 to 1.019
# R2 statistic goes up from 0.2393 to 0.872
# F statistic goes up from 41.52 to 381.4
# => model E is much better fit than model A

# obtain 95% confidence intervals for the coefficient(s)
confint(car_seats_all_lm)

# check outliers or high leverage observations
par(mfrow = c(2, 2))
plot(car_seats_all_lm)

rstudent(car_seats_all_lm)[which(rstudent(car_seats_all_lm)>3)]
# => observation 358 appears to an outlier

hatvalues(car_seats_all_lm)[order(hatvalues(car_seats_all_lm), decreasing = T)][1]
# => one high leverage observation ??? BUT WHY -> have not understood yet