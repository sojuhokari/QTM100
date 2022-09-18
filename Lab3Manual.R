################################
# Author: Soju Hokari
# Document Name: Lab 3 Manual -- NORMAL AND BINOMIAL DISTRIBUTIONS
# Date: 16 September 2022
################################

# Normal distribution
# - a bell curve
# - have certain properties that help us make inferences based on the distribution

# 68-96-99.7 rule -- standard bell curve
# 68.27% within 1 sd from mean
# 95.45% within 2
# 99.73% within 3

# If we know mean and sd and we know it's normally distributed, we can know a lot

# --------------------------------
# MARK: Set-up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
fruitfly <- read.csv("datasets/fruitfly.csv", header=T)

# Fruitfly dataset:
# No: serial number (1-25) within each group of 25
# type:
#   Type of experimental assignment
# 1 = no females
# 2 = 1 newly pregnant female 3 = 8 newly pregnant females 4 = 1 virgin female
# 5 = 8 virgin females
# lifespan: lifespan (days)
# thorax: length of thorax (mm)
# sleep: percentage of each day spent sleeping

# --------------------------------
# MARK: Set-up
# --------------------------------

# Histogram!
hist(
    fruitfly$lifespan,
    xlab = "lifespan (days)"
)

# Check the normality with a quantile/quantile plot
qqnorm(fruitfly$lifespan)

# add a line to see how close the data is to a normal distribution
qqline(fruitfly$lifespan)
# the dots fall close to the diagonal line, which means that it is pretty well distributed

# What is the probability that a fruitfly lives less than or equal to 50 days?
# Find mean and sd to make a normal dist
mean(fruitfly$lifespan)
sd(fruitfly$lifespan)

# store these values in objects
lifemean <- mean(fruitfly$lifespan)
lifesd <- sd(fruitfly$lifespan)

# calculate the probablility of living less than 50 days using pnorm
pnorm(q = 50, mean = 57.4, sd = 17.6)
# 33.7% of fruitflies are expected to survive less than or equal to 50 days

# Calculate the same probability using the actual data
sum(fruitfly$lifespan<=50)/length(fruitfly$lifespan)
# 39.2% in the actual data -> fairly close to the prediction based on the normal distribution

# How can we calculate the upper tail? Surviving more than 50 days
1-pnorm(q=50, mean=lifemean, sd=lifesd)
# 0.6640699 = 66.4%

# get a precise percentile
# p= the percentile of interest
qnorm(p=0.90, mean=lifemean, sd = lifesd)
# We get 79.94903, which means that 90% live less than 80 days

# Using the actual data to get the percentile
quantile(x=fruitfly$lifespan, probs=0.90)
# We get 79.6

# The binomial distribution!
# Predicting the survival rate of 8 new flies

# binomial dist required when fruitflies are independent,
# there is a fixed number of fruitflies in sample,
# each fruitfly can either survive more than or less than 50 days -- mutually exclusive
# each fruitfly has an equal odds of surviving

# Use the dbinom function to find the probability that a certain number of flies in our sample will survive more than 50 days

# calculate the probability of 5 out of 8 flies living longer than 50 days
dbinom(x = 5, size = 8, prob = 0.663)
# the prob argument is the probability that a fruitfly lives longer than 50 days
# We get the probability to be 0.2745651
