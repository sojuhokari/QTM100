################################
# Author: Soju Hokari
# Document Name: Lab 8 Manual -- Inference for a single mean and sampling
#                                distribution of the mean
# Date: 25 October 2022
################################

# T-tests

# chi-squared and proportion tests are useful for making inferences about
# categorical or dichotomous data.

# if we are interested in quantitative data, we need the t test

# CourseEvals.csv -- 463 courses taught by 94 profs at UT Austin

# --------------------------------
# Set up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
evals <- read.csv("datasets/CourseEvals.csv", header = T)

# --------------------------------
# Exploring the Data
# --------------------------------

# Create histogram of the cls_perc_eval variable
hist(evals$cls_perc_eval)

# Summary
summary(evals$cls_perc_eval)
sd(evals$cls_perc_eval)

# --------------------------------
# One-sample t-test
# --------------------------------

# Admin say the true average completion rate is 80%. Is that reasonable based on
# our sample? Use the one-sample t test to determine whether our sample mean of
# 74.43% is statistically significantly different from 80%

# Hypotheses: H0: mu = 80; Ha: mu != 80, where mu is the true average percent

# CONDITIONS: Is the data normally distributed, and are the observations
# independent?

# Our data are left-skewed, but the sample size (n=463) is large enough for the
# conditions for valid inference to be satisfied. The data is also NOT
# independent because the professors teach multiple courses, but we're simply
# ignoring that for the purposes of this lab.

# Perform one-sample t-test
t.test(evals$cls_perc_eval, mu=80)
# test statistic: t = -7.1555
# df = 462
# p-value: 3.294e-12
# 95% confidence interval: 72.89749 75.95808
# mean of x: 74.42779
#
# We therefore REJECT the null hypothesis because 80 doesn't fit in the
# confidence interval, and our p-value is smaller than 0.05

# Adding additional arguments: confidence level
t.test(evals$cls_perc_eval, mu=80, conf.level=0.90)

# Test evaluating the likelihood that evaluation % is less than 80
t.test(evals$cls_perc_eval, mu=80, alternative="less")
# results are the same as above, except that our 95% conf interval is now
# -Inf 75.71126
#
# We REJECT the null hypothesis because the p-value is EVEN SMALLER

# --------------------------------
# t distribution
# --------------------------------

# probabilities and quantiles for distributions:
# pnorm and qnorm for normal
# pchisq for chi squared
# pt and qt for t distribution

# t distribution approaches normal distribution as df approaches infinity

# Using the t distribution to get the two-tailed p-value for a one-sample two-
# sided t test
# t = -2, df = 50
2*pt(-2, df=50) # multiply by 2 to make it two-tailed
# we get 0.05094707

# Use the qt function to identify a t-score for  aspecific confidence interval
# a 95% confidence interval corresponds to lower tail area of 0.025
qt(0.025, df=50)
# we get -2.008559
# use the absolute value as the t test statistic: 2.008559

# --------------------------------
# PART 2 SAMPLING DISTRIBUTION OF THE MEAN
# --------------------------------

# population distribution
# data distribution
# sampling distribution
#             a probability distribution of a sample

# The sampling distribution illustrates the importance of sample size
# as n increases, the sampling distribution gets tighter and closer to normal


# --------------------------------
# Set up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
yrbss <- read.csv("datasets/yrbss2013.csv", header = T)

# We are interested in the number of days that students had at least one drink
# of alcohol in the last 30 days

days_drink <- yrbss$days_drink

# Summary
summary(days_drink)
hist(days_drink)
# mean: 1.454
# median: 0.000
# max: 30.000

# Draw a random sample of size 50 from the vector days_drink
# We're creating a sampling distribution from the "population"
samp_dd1 <- sample(x = days_drink, size = 50)
mean(samp_dd1)
# We get a random mean that's DIFFERENT from 1.454
# 1.96
# 1.06
# 1.8

# Use a for loop to create a sampling distribution
sample_means50 <- rep(NA, 5000)
for (i in 1:5000) {
  samp_d <- sample(days_drink, 50)
  sample_means50[i] <- mean(samp_d)
}

# View histogram!
hist(sample_means50)

# Do it again for size 10 and size 100!
sample_means10 <- rep(NA, 5000)
sample_means100 <- rep(NA, 5000)
for (i in 1:5000) {
  samp_d <- sample(days_drink, 10)
  sample_means10[i] <- mean(samp_d)
  samp_d <- sample(days_drink, 100)
  sample_means100[i] <- mean(samp_d)
}

# The histograms get wider and more right skewed for the 10, and closer to
# normal for the 100
hist(sample_means10)
hist(sample_means100)

# PLOT TOGETHER
# divide the plot into three rows and 1 column
par(mfrow = c(3,1))

# set the range of the x-axis of the histograms so they are equal
xlimits <- range(sample_means10)

# plot the histograms
hist(sample_means10, breaks=20, xlim=xlimits)
hist(sample_means50, breaks=20, xlim=xlimits)
hist(sample_means100, breaks=20, xlim=xlimits)








