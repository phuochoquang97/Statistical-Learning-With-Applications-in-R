DIR <- "D:/LEARNING/R/StatisticalLearning/Chapter1/"
college <- read.csv(paste(DIR, "College.csv", sep = ""))
View(college)

# remove the first column
college <- college[, -1]
View(college)
summary(college)

# produce scatterplot matrix of first 10 cols
pairs(college[, 2:10])

#plot side-by-side boxplots of Outstate vs Private
plot(college$Apps, college$Outstate)

Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
View(college)

summary(college)
plot(college$Elite, college$Outstate)

par(mfrow = c(2, 2))
hist(college$PhD)
hist(college$Enroll)
hist(college$Books)
hist(college$F.Undergrad)

# some interesting observations
college <- read.csv(paste(DIR, "College.csv", sep = ""))
college[which.max(college$Top10 perc), 1] # what is the university with the most students in the top 10%

# - which university has the smallest acceptance rate
acceptance_rate <- college$Accept / college$Apps
college[which.min(acceptance_rate), 1]

# - which university has the most liberal acceptance
college[which.max(acceptance_rate), 1]
