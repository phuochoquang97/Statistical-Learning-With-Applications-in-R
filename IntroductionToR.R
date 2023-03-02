x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
y <- matrix(data = c(5, 6, 7, 8), 2, 2, byrow = TRUE) # populate the matrix in order of the rows
sqrt(x) # return the square root of each element
y ^ 2

# generate a vector of random normal variables, n = sample size
x1 <- rnorm(50) # mean = 0, sd = 1

y1 <- x1 + rnorm(50, mean = 50, sd = 0.1)

cor(x1, y1)

set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sqrt(y)
sd(y)


# GRAPHICS
x <- rnorm(100)
y <- rnorm(100)
plot(x, y)
plot(x, y, xlab = "this is the x-axis", ylab = "this is the y-axis", main = "Plot of X vs Y")

# create a sequence of numbers
x <- seq(0, 1, length = 10)
y <- 1 : 20
y
z <- seq(-pi, pi, length = 50)
z


#  contour plot
x <- seq(-pi, pi, length = 50)
y <- x
f <- outer(x, y, function(x, y) cos(y) / (1 + x ^ 2))
contour(x, y ,f)
contour(x, y , f, nlevels = 45, add = T)

fa <- (f - t(f)) / 2
contour(x, y, fa, nlevels = 15)

image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 40)


# indexing data
A <- matrix(1:16, 4, 4)
A[2, 3]               # row 2, col 3
A[c(1, 3), c (2, 4)]  # row 1+3, col 2+4 => 5 13 7 15
A[1:3, 2:4]           # row 1, 2, 3; col 2, 3, 4 => 5 9 13- 6, 10, 14 - 7, 11, 15
A[1:2, ]              # row 1, 2; all cols => 1 5 9 13 - 2 6 10 14

A[-c(1, 3), ]         # except row 1 + 3; all cols => 2 6 10 14 - 4 8 12 16
dim(A)                # => 4 4

# loading data
# install.packages("ISLR")
library(ISLR)
# "Auto.data" %in% list.files()
#Auto = read.table("Auto.data")
Auto = ISLR::Auto
View(Auto)
head(Auto)
DIR <- "D:/LEARNING/R/StatisticalLearning/"
write.table(Auto, file = paste(DIR, "Auto.csv", sep = ""), sep = ",")
AutoCsv <- read.csv(paste(DIR, "Auto.csv"), header = T, na.strings = "?", stringsAsFactors = T)
View(AutoCsv)
dim(AutoCsv)
