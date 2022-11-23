################################
# Author: Soju Hokari
# Document Name: Lab 7 Manual -- Inference for Categorical Data
# Date: 25 October 2022
################################

# --------------------------------
# Set up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
gardasil <- read.table("datasets/gardasil.txt", header = T)

# Explore
str(gardasil)
summary(gardasil)

# --------------------------------
# Two sample z test
# --------------------------------

# Deciding whether the completion rate varies by age group
# Comparing 11-17 age group to 18-26 age group
#
# The hypotheses are therefore:
# H0: p1 = p2 and Ha: p1 != p2
# or
# H0: p1 - p2 = 0 and Ha: p1 - p2 != 0

# We use a two-proportion z-test

# Create frequency table
Age_Completion_Table <- table(gardasil$AgeGroup, gardasil$Completed)

# View table
Age_Completion_Table

# Add sumary margins
addmargins(Age_Completion_Table)

# Calculate row proportions
prop.table(Age_Completion_Table, margin=1)

# There IS a difference between age groups, but we have to now check whether
# that difference is statistically significant

# Two sample proportion test
prop.test(c(247, 222), c(701, 712), correct=F)

# Alternately using the contingency table as input
# The RESPONSE of interest must be the first column, so we have to order the factor

# Create new variable with correct ordering
gardasil$Completed2 <- factor(gardasil$Completed, levels=c("yes", "no"))

# Create new table
Age_Completion_Table2 <- table(gardasil$AgeGroup, gardasil$Completed2)
Age_Completion_Table2

# Two sample proportion test using the TABLE
prop.test(Age_Completion_Table2, correct=F)

# We can fail to regect the null hypothesis because the confidence interval
# includes zero, so the true difference could plausibly be zero, and the
# p-value is 0.1055, which is MORE than 0.05 level of significance.

# This test yields a chi-squared test statistic with one degree of freedom.
# To calculate by hand, z=sqrt(X^2) = sqrt(2.62) = 1.62

# --------------------------------
# Chi-Square Test
# --------------------------------

# For when we have more than two groups and cannot therefore use a proportion test

# Begin by examining descriptive statistics

# Table of frequencies
Insurance_Completion_Table <- table(gardasil$InsuranceType, gardasil$Completed)
Insurance_Completion_Table

# Add summary margins
addmargins(Insurance_Completion_Table)

# Calculate row proportions
prop.table(Insurance_Completion_Table, margin=1)

# Completion rates seem to differ quite a bit.

# Perform a Chi-Square test for completion by insurance type
# We use correct=F to suppress the continuity correction for data with small counts
chisq.test(gardasil$Completed, gardasil$InsuranceType, correct=F)

# Run and save chi square test for completion by insurance type
# in order to view expected cell counts, because all expected cell counts must
# be at least 5
Ins.Comp.test <- chisq.test(gardasil$Completed, gardasil$InsuranceType, correct=F)

# View expected cell count
Ins.Comp.test$expected

# All are above 5

# --------------------------------
# Fisher's Exact Test
# --------------------------------

# When one expected cell count is less than 5, use this test!

# Run fisher's exact with variables
fisher.test(gardasil$Completed, gardasil$InsuranceType)

# Run with existing table
fisher.test(Insurance_Completion_Table)

# --------------------------------
# Chi-squared distribution
# --------------------------------

# The chi-squared distribution is generally right-skewed.

# To calculate area under the curve for chi-square distribution, use `pchisq`
# P-values based on a chi-squared test stat are given by the upper area under
# the tail of the distribution; bu default pchisq returns a lower tail area.
# To obtain the upper tail, take the compliment

# Example: when we examined the relationship between AgeGroup and Completion,
# we got a chi-squared test statistic of 2.62 on 1 degree of freedom. The
# p-value for this test of association can be calculated as follows:
1 - pchisq(2.62, df=1)

# Result is 0.1055.
# Chi-square tests are always one-tailed due to the shape of the distribution.
# A one-tailed chi-square p-value is equivalent to that of a 2-tailed z test.






