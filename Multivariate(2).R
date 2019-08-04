happiness_data <- readRDS(file.choose())


# I will first plot if relationship makes sense
plot(happiness_data)

# In this linear regression t-test
# We want to focus on if the slope of regression is equal to zero or not

# Null Hypothesis = The slope of regression is equal to zero
#                   There is no relationship between self_esteem and grades
# Alternative Hypothesis = The slope of regression is not equal to zero
#                   There is a relationship between self_esteem and grades
esteem.grades.regression <- lm(self_esteem ~ grades, data=happiness_data)
plot(happiness_data$grades, happiness_data$self_esteem)
abline(esteem.grades.regression, col="blue")

summary(esteem.grades.regression)

# Summary shows that with 98 degrees of freedom of t statstic having
# more extreme value than t stsatistic 5.93 is extremly low = 4.93e-0.8
# P(t > 5.93)  = (4.39e-08 / 2)
# P(t > -5.93) = (4.39e-08 / 2)
# So we reject the null hypothesis that there is no relationship between
# self esteem and grade, and we accept the alternative hypothesis which
# we conclude that there is a relationship between them.


# We will do the samething again for the happiness and self_esteem
# Null Hypothesis = The slope of regression is equal to zero
#                   There is no relationship between happiness and self_esteem
# Alternative Hypothesis = The slope of regression is not equal to zero
#                   There is a relationship between happiness and self_esteem
happiness.esteem.regression <- lm(happiness ~ self_esteem, data=happiness_data)
plot(happiness_data$self_esteem, happiness_data$happiness)
abline(happiness.esteem.regression, col="blue")

summary(happiness.esteem.regression)

# Summary shows that with 98 degrees of freedom of t statstic having
# more extreme value than t stsatistic 7.622 is extremly low = 1.61e-11
# P(t > 7.622)  = (1.61e-11 / 2)
# P(t > -7.622) = (1.61e-11 / 2)
# So we reject the null hypothesis that there is no relationship between
# self esteem and happiness, and we accept the alternative hypothesis which
# we conclude that there is a relationship between them.
