################################
# Authors: Jaleel Sanders, Akonam Agbu, Soju Hokari
# Document Name: Lab 7 Homework
# Date: 9 November 2022
################################

# --------------------------------
# PART ONE: INFERENCE FOR A SINGLE MEAN
# --------------------------------

# --------------------------------
# 1. Import the dataset and explore the mean IQ
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
lead <- read.csv("datasets/lead.csv", header = T)

# See a summary of the dataset
summary(lead)

# Get the mean and standard deviation of the IQ of children in the sample
mean(lead$Iqf)
sd(lead$Iqf)

# a. The mean IQ is 91.08065
#    The standard deviation is 14.40393

# Plot the IQ to see whether it's normally distributed
hist(lead$Iqf)

# b. Looking at the plot, the data look to be roughly normally distributed

# --------------------------------
# 2. Average IQ of children in another study was 85. Perform the appropriate
#    test to determine if the average IQ in our sample is significantly
#    different than the IQ in the other research study
# --------------------------------

# a. We should run a one-sample T test

# b. H0: (mean IQ of our sample) = 85
#    Ha: (mean IQ of our sample) != 85

# Perform the t-test
t.test(lead$Iqf, mu=85)

# c. test statistic: t = 4.7009
#    df = 123
#    p-value = 6.825e-06

# d. 95% confidence interval:
#    88.52022, 93.64107

# e. We reject the null hypothesis, because the mu of 85% does not fit in the
#    95% confidence interval, the test statistic is 4.7009 which is much higher
#    than around 1.96 (which would be the cutoff for 95% confidence for a normal
#    distribution and this t-distribution has a df of 123 so it is close to
#    to normal), and the p-value is 6.825e-06, which is much smaller than 0.05
#    (the significance level).

# --------------------------------
# 3. Average blood lead levels of children in 1972 in another study was 36
#    (micrograms/100mL). Perform appropriate tests to determine if our sample
#    is significantly different.
# --------------------------------

# a. one-sample t test

# b. H0: (mean lead level of our sample) = 36
#    Ha: (mean lead level of our sample) != 36

# Perform the t-test
t.test(lead$Ld72, mu=36)

# c. t = -1.1502
#    df = 120
#    p-value = 0.2524

# d. 95% confidence interval
#    32.19903, 37.00758

# e. We fail to reject the null hypothesis, because the mu of 36% is within the
#    95% confidence interval, the test statistic is -1.1502 (less than 1.96),
#    and the p-value is 0.2524, which is larger than 0.05.

# --------------------------------
# PART TWO: SAMPLING DISTRIBUTION OF THE MEAN
# --------------------------------

# --------------------------------
# 1. height_m variable
# --------------------------------

# Import dataset
yrbss <- read.csv("datasets/yrbss2013.csv", header = T)

# a. create a new height_cm variable
yrbss$height_cm <- yrbss$height_m * 100

# summary
summary(yrbss$height_cm)

# mean and standard deviation
mean(yrbss$height_cm)
sd(yrbss$height_cm)

# plot
hist(yrbss$height_cm)

# b. mean: 168.729
#    standard deviation: 10.29791
#    The distribution is roughly normal

# --------------------------------
# 2. varying numbers of samples, samples of size 10
# --------------------------------

# Use a for loop to create a sampling distribution
sample_means_100 <- rep(NA, 100)
for (i in 1:100) {
  sample <- sample(yrbss$height_cm, 10)
  sample_means_100[i] <- mean(sample)
}

# Get info about the 100 samples
summary(sample_means_100)
sd(sample_means_100)
hist(sample_means_100)

# a. mean: 168.7
#    sd: 3.4564
#    Distribution is slightly skewed left, but roughly normal


# Use a for loop to create a sampling distribution
sample_means_500 <- rep(NA, 500)
for (i in 1:500) {
  sample <- sample(yrbss$height_cm, 10)
  sample_means_500[i] <- mean(sample)
}

# Get info about the 500 samples
summary(sample_means_500)
sd(sample_means_500)
hist(sample_means_500)

# b. mean: 168.7
#    sd: 3.354708
#    Distribution is normal


# Use a for loop to create a sampling distribution
sample_means_1000 <- rep(NA, 1000)
for (i in 1:1000) {
  sample <- sample(yrbss$height_cm, 10)
  sample_means_1000[i] <- mean(sample)
}

# Get info about the 1000 samples
summary(sample_means_1000)
sd(sample_means_1000)
hist(sample_means_1000)

# c. mean: 168.7
#    sd: 3.204742
#    Distribution is normal


# Use a for loop to create a sampling distribution
sample_means_5000 <- rep(NA, 5000)
for (i in 1:5000) {
  sample <- sample(yrbss$height_cm, 10)
  sample_means_5000[i] <- mean(sample)
}

# Get info about the 5000 samples
summary(sample_means_5000)
sd(sample_means_5000)
hist(sample_means_5000)

# d. mean: 168.7
#    sd: 3.321415
#    Distribution is normal

# e. Increasing the number of samples while keeping the size constant leads the
#    sampling distribution to be closer to normal.


# --------------------------------
# 3. 5000 samples, samples of varying size
# --------------------------------

# Use a for loop to create a sampling distribution
sample_means_size100 <- rep(NA, 5000)
for (i in 1:5000) {
  sample <- sample(yrbss$height_cm, 100)
  sample_means_size100[i] <- mean(sample)
}

# Get info about the samples with size 100
summary(sample_means_size100)
sd(sample_means_size100)

# a. mean: 168.7
#    sd: 1.03549


# Use a for loop to create a sampling distribution
sample_means_size200 <- rep(NA, 5000)
for (i in 1:5000) {
  sample <- sample(yrbss$height_cm, 200)
  sample_means_size200[i] <- mean(sample)
}

# Get info about the samples with size 200
summary(sample_means_size200)
sd(sample_means_size200)

# b. mean: 168.7
#    sd: 0.7288578


# Use a for loop to create a sampling distribution
sample_means_size500 <- rep(NA, 5000)
for (i in 1:5000) {
  sample <- sample(yrbss$height_cm, 500)
  sample_means_size500[i] <- mean(sample)
}

# Get info about the samples with size 500
summary(sample_means_size500)
sd(sample_means_size500)

# c. mean: 168.7
#    sd: 0.4427662

# d. Increasing the size of each draw while keeping the number of samples high
#    and constant decreased the standard deviation (and therefore, presumably,
#    the margin of error)



