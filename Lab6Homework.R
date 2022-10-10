################################
# Authors: Jaleel Sanders, Akonam Agbu, Soju Hokari
# Document Name: Lab 6 Homework
# Date: 7 October 2022
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

# --------------------------------
# 1. LocationType variable
# --------------------------------

# find percent of patients who went to an urban/suburban clinic
prop.table(table(gardasil$LocationType))

# a. 31.85% of patients went to an urban clinic
# b. 68.15% of patients went to a suburban clinic

# --------------------------------
# 2. LocationType variable
# --------------------------------

# a. We should run a hypothesis test to determine whether our sample has a
#    statistically different percentage of suburban patients

# b. Null hypothesis (Ho): p = 0.70
#    Alternative hypothesis (Ha): p != 0.70

# Factor gardasil$LocationType in order to ensure we have the right order
gardasil$LocationTypeF <- factor(gardasil$LocationType, levels=c(
  "suburban",
  "urban"
))

# Run the test
prop.test(table(gardasil$LocationTypeF), p = 0.70, correct = F)

# Find the z test statistic
sqrt(2.2957)

# c. p-value = 0.1297
#    z test statistic = -1.515157
#    95% confidence interval: (0.656773, 0.705300)

# d. Given the p-value is over 0.05, the sample statistic is within our 95%
#    confidence interval. Additionally, the test statistic is -1.515157, which
#    is less than the 1.96 standard deviations (or 95% confidence) away from
#    0.70.
#
#    We should ACCEPT the null hypothesis, which means that the data in the
#    study is representative of the local population.

# --------------------------------
# 3. 53% of women who receive the vaccine in the population are under 18
# --------------------------------

# a. We can use the AgeGroup variable

# Find the propoprtion of women who are under 18
prop.table(table(gardasil$AgeGroup))

# b. 49.61% of the women in our sample are under 18.

# c. Ho: p = 0.53
#    Ha: p != 0.53

# factor AgeGroup
gardasil$AgeGroupF <- factor(gardasil$AgeGroup, levels=c(
  "11-17",
  "18-26"
))

# Run the test
prop.test(table(gardasil$AgeGroupF), p = 0.53, correct = F)

# find the z test statistic
sqrt(6.5159)

# d. p-value: 0.01069
#    z test statistic: 2.552626
#    95% confidence interval: (0.4700839, 0.5221523)

# e. 0.53, the population parameter, is not within the 95% confidence interval,
#    indicating that we should reject the null hypothesis. This means that the
#    sample we have is NOT representative of the population.

# --------------------------------
# 4. Use z statistic from 3 and std normal dist. to calculate p-value
# --------------------------------

# Z test statistic: 2.552626

# Use pnorm to find the upper tail when the z-score is 2.552626, then multiply
# that upper tail by 2 to account for the lower tail
pnorm(2.552626, lower.tail = F) * 2

# We used the above code to get the p-value of 0.010669142.

# --------------------------------
# 5. 
# --------------------------------

# The Johns Hopkins team should worry that their sample isn't appropriate, as
# the percentage of women who are under 18 in the sample is not quite
# representative of the population, although it is close.




