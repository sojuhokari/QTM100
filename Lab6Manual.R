################################
# Author: Soju Hokari
# Document Name: Lab 6 Manual -- Inference for a Single Proportion
# Date: 7 October 2022
################################

# What info do we have?
# What are we trying to infer?

# Hypotheses
# null and alternative
# Null: No statistically significant difference between values
# -- any difference we see is due to chance
# Alternative hypothesis: There is a statistically significant difference

# --------------------------------
# Set up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
gardasil <- read.table("datasets/gardasil.txt", header = T)

# Explore data
str(gardasil)
summary(gardasil)

# --------------------------------
# Conducting a z-test
# --------------------------------

# view contingency table w/ frequencies
table(gardasil$Completed)

# Convert table into proportion table
prop.table(table(gardasil$Completed))

# Another study found the Gardasil completion rate to be 40% -- are the results
# of that study consistent with the data we have?
#
# H0: p = 0.40 versus Ha: p != 0.40

# Run one sample z test on a proportion
# number of subjects who experienced the event, total number of subjects studied,
# value tested in null hypothesis, correct=F
# 
# correct: a logical indicating whether Yates' continuity correction should be
# applied where possible. Basically, correct=F ensures the results are the same
# as if we calculated by hand.
prop.test(469, 469+944, p=0.4, correct=F)

# --------------------------------
# Calculate a p-value manually in R
# --------------------------------

# We get that X-squared is 27.79, which means that z = square root of x squared
# which is 5.223983
sqrt(27.29)

# We can use pnorm to find the probability that Z < 5.2
pnorm(5.2)

# To find the upper tail (Z > 5.2)
pnorm(5.2, lower.tail = F)
# Same as
1-pnorm(5.2)
# or
pnorm(-5.2)

# BUT this should be a two-tailed test
# so really it's
2*(pnorm(-5.2))
# Correct for rounding error
2*(1-pnorm(sqrt(27.29)))

# --------------------------------
# How we SHOULD do it
# --------------------------------

# Use our data set, through a table, instead of inputting data manually
prop.test(table(gardasil$Completed), p = 0.4, correct = F)

# This table doesn't work tho because the default level ordering for factors
# orders by alphabetical ordering -- so "no" comes before "yes", so we end
# up comparing not successes to successes

# So we restructure
gardasil$Completed2 <- factor(gardasil$Completed, levels=c("yes", "no"))
# Then test again
prop.test(table(gardasil$Completed2), p = 0.4, correct = F)
# And we get the right data

# WE can reject the null hypothesis because 0.4 is not within the confidence
# interval returned by prop.test, AND because p-value is less than 0.05

# So we reject the null hypothesis, meaning there is a statistically significant
# difference between the two studies

