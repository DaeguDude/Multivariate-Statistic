# This program is to find out if there's actually explainable relationship
# Between X variables(Sleep, Coffee) and Y variable(Exam Score)

# Load the data first
dataset_sleep <- readRDS(file.choose())

# I will plot the data to make sure if there's a visible relationship
plot(dataset_sleep)

# First checking the collinearity between explanatory variables
cor.sleep.coffee = round(cor(dataset_sleep$sleep, dataset_sleep$coffee, method='pearson'), 2)

# Check the correlation between X to Y
cor.sleep.exam = round(cor(dataset_sleep$sleep, dataset_sleep$exam_score), 2)
cor.coffee.exam = round(cor(dataset_sleep$coffee, dataset_sleep$exam_score), 2)

# It seems that it shows no collinearity between explanatory variables, which is good,
# And coffee has negative linear relationship, sleep has positive linear relationship.
# However, we are not sure how relevant this is yet. So it requires the further process.

# I will conduct the regression now
multiple.regression <- lm(exam_score ~ coffee + sleep, data=dataset_sleep)

# Have a look at multiple regression summary
# What we are interested here is,
# R squared, F value, and P value
summary(multiple.regression)


## I want to make sure, by calculating R value manually,
# If the summmary above is legit.
# R value explains how much variation in exam score
# can be explained by both variables, sleep and Coffee

# Sum of squares
ss.mean <- round( sum((dataset_sleep$exam_score - mean(dataset_sleep$exam_score))^2),  2)
ss.multiple <- round( sum(multiple.regression$residuals^2),  2)

# It shows as same as what summary says
# About 63% variation in exam score can be explained
# by sleep duration and coffee consumption
Rsquared = round( (ss.mean - ss.multiple) / ss.mean, 2)

# Now we know our R squared value is calculated correctly
# We need to know if our R squared value is legitimate
# To know that, we need p-value and it comes from F distribution
# I will check if our F-value is correct.

# And it seems correct
F.value <- round(
  ((ss.mean- ss.multiple) / (3 - 1)) / 
  (ss.multiple / (nrow(dataset_sleep) - 3)),
  2)

# And let's check the summary again
summary(multiple.regression)

# Our summary shows that,
# R sqaured value of 0.6321, which means 63% of variation in
# Exam score can be explained by coffee consumption and sleep duration

# And our P value of 2.2e-16 confirms that, to have a F value of 83.33
# in 2 and 97 degrees of freedom F distribution is REALLY LOW
# So I can confirm that both Sleep duration and Coffee consumption has
# effect on the results of the exam.
