################################
# Author: Soju Hokari
# Document Name: Lab 4 Manual -- 
# Date: 23 September 2022
################################

# Stats from a random sample as point estimates for population parameters

# Population distribution <- parameters descrbie populations
# Data distribution <- statistics describe samples

# Sampling distribution
# A probability distribution of a statistic obtained from a large number of
# samples drawn from a specifc population

# Youth Risk Behavior Survey Something (yrbss)
# We're going to pretend the yrbss dataset includes the full population of every "youth" enrolled in school in the US
# We are interested in whether or not students were bullied on school property in the last 12 months

# --------------------------------
# Set up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
yrbss <- read.csv("datasets/yrbss2013.csv", header = T)

# --------------------------------
# Create random samples
# --------------------------------

# Create bullied object
bullied <- yrbss$bullied

table(bullied)
prop.table(table(bullied))

# Create bar plot with proportions
barplot(prop.table(table(bullied)), beside=T)

# Draw a random sample of 10 from the vector bullied
samp1 <- sample(x = bullied, size = 10)

# Look at proportion of students bullied in samp1
prop.table(table(samp1))

# What if we take a sample of size 200?
samp2 <- sample(x = bullied, size = 200)
prop.table(table(samp2))

# This time it's much closer to the true population because it's 200 samples

# --------------------------------
# For loops
# --------------------------------

# The `sampling distribution` is the distribution of all of the proportions from all of these samples
# Some samples have populations that are closer to the true population proportion -- `sampling error`
# How can we make repeated samples easier?

# For loops!
# Create an empty matrix to store proportions
sample_prop10 <- matrix(rep(NA, 500), nrow=500, ncol=2)
# Make a 500x2 matrix
# the rep(NA, 500) fills the matrix with NA in every row/col

# Populate the Matrix using a for loop
for(i in 1:500) {
  print(paste0('i =', i)) # print out i
  samp <- sample(bullied, 10) # make a sample
  print(prop.table(table(samp)))
  sample_prop10[i,] <- prop.table(table(samp)) # store a table in the matrix
}

# now, we can plot the average proportion values
barplot(colMeans(sample_prop10), names.arg = c("no", "yes"), ylim = c(0,1))


