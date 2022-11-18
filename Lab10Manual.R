################################
# Author: Soju Hokari
# Document Name: Lab 10 Manual -- ANOVA and 2 Sample T-test
# Date: 18 November 2022
################################

# chi sq and prop tests are useful for making inferences about cat or dich data

# ANOVA (Analysis of Variance): inferences about numerical response

# 2-sample t-test: inferences about quantitative variables in 2 groups

# --------------------------------
# Set up
# --------------------------------

# The working directory. Change this variable to match the location of this
# directory on your computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
mariokart <- read.csv("datasets/mariokart.csv", header = T)

# examine data
str(mariokart)
summary(mariokart)

# --------------------------------
# Getting Started/ANOVA
# --------------------------------

# Comparing shipping speed and price

# histogram and boxplot of total price
hist(mariokart$total_pr)
boxplot(mariokart$total_pr)

# There are some significant unusually high outliers. Let's look at them!
mariokart[mariokart$total_pr>100,]
# Because these two packages came with multiple games, we exclude them from the
# dataset
mkClean <- subset(mariokart, mariokart$total_pr<100)
hist(mkClean$total_pr)
# Now, the histogram looks normal

# Look at average cost for each shipping speed
# what in the world does tapply do???
?tapply
tapply(mkClean$total_pr, mkClean$ship_sp, mean)

# Look at number of observations for each shipping method
table(mkClean$ship_sp)

# Because a few categories only have a few observations, we're going to combine
# them all together

# Create new variable with less shipping categories
mkClean$newship <- factor(NA, levels=c(
  "FirstClass/Priority",
  "UPS",
  "Standard",
  "other"
))

# Assign each sale to new category
mkClean$newship[
  mkClean$ship_sp == "firstClass"
  | mkClean$ship_sp == "priority"
] <- "FirstClass/Priority"

mkClean$newship[
  mkClean$ship_sp == "ups3Day"
  | mkClean$ship_sp == "upsGround"
] <- "UPS"

mkClean$newship[
  mkClean$ship_sp == "media"
  | mkClean$ship_sp == "parcel"
  | mkClean$ship_sp == "other"
] <- "other"

mkClean$newship[
  mkClean$ship_sp == "standard"
] <- "Standard"

# Verify recoding
table(mkClean$newship, mkClean$ship_sp)

# The outcome variable of interest, total price, is normal at the dataset level.
# Is each group normal?
boxplot(mkClean$total_pr ~ mkClean$newship,
        main = 'Total Price by shipping Carrier',
        xlab = "Shipping Carrier",
        ylab = "Total Price ($)
        ")

# Inspect the average total price for each new shipping group
tapply(mkClean$total_pr, mkClean$newship, mean)

# Histograms!
hist(mkClean$total_pr[mkClean$newship == "FirstClass/Priority"])
hist(mkClean$total_pr[mkClean$newship == "UPS"])
hist(mkClean$total_pr[mkClean$newship == "Standard"])
hist(mkClean$total_pr[mkClean$newship == "other"])

# They are not perfectly normal, but the distributions are normal enough.

# Use the aov function to model a numerical response (total_pr) by the
# categorical grouping (newship) variable to see if our observed differences
# are statistically significant.
# Conduct ANOVA
anova.ship <- aov(mkClean$total_pr ~ mkClean$newship)
summary(anova.ship)

# p < 0.001, so we reject the null hypothesis.
# At least one mean differs from the others.
# Shipping type is associated with price.

# --------------------------------
# Pairwise comparisons
# --------------------------------

# Perform Tukey test to determine which pairs of means differ
TukeyHSD(anova.ship)
plot(TukeyHSD(anova.ship))

# Any pairwise test for which the confidence interval does not touch zero is
# significant:
# UPS and FirstClass/Priority
# other and FirstClass/Priority
# Standard and UPS

# --------------------------------
# Two-sample t-tests
# --------------------------------

# Comparing two shipping speeds using a two-sample t test

# Obtain total prices for UPS and Standard
UPS <- mkClean$total_pr[mkClean$newship == "UPS"]
Standard <- mkClean$total_pr[mkClean$newship == "Standard"]

# Perform two sample t-test with equal variance
t.test(UPS, Standard, var.equal=T)

# test statistic is 2.4992 with 63 degrees of freedom.
# p-value is 0.01507
# So at the 0.05 significance leve, we reject the null hypothesis that the
# average prices for UPS and Standard shipping are equal.
# We are 95% confident that the true average price difference is in the interval
# of 1.2146 to 10.9067.
# We observe that the p-values from the pairwise comparison of Standard-UPS
# from the ANOVA test and the two-sample t-test are approximately the same.
# Therefore, when comparing two groups, you can use either aov() or t.test, but
# with more than two groups, you should use the aov() function.




