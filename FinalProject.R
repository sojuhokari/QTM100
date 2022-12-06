################################
# Authors: Jaleel Sanders, Akonam Agbu, Soju Hokari
# Document Name: Preliminary Project
# Date: 6 December 2022
################################

# --------------------------------
# Import dataset
# --------------------------------

# For our preliminary project, we chose the National Study of Youth and Religion
# (NSYR) dataset. We'll import it as follows:

# The working directory. Change this variable to match the location of this
# directory on your computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
NSYR <- read.csv("datasets/NSYR_data.csv", header = T)

# --------------------------------
# Research Questions
# --------------------------------

# SEE SLIDESHOW

# --------------------------------
# Methods: Dataset and Variables
# --------------------------------

# DATASET
# The National Study of Youth and Religion is a research project started in
# August 2001 aimed at gaining a deeper understanding as to how religious lives
# form in adolescence to adulthood. Data collection took place through multiple
# telephone surveys conducted between 2001 and 2008. Here, we use the dataset
# gathered from Wave 3 of the study, collected between 2007 and 2008, in order
# to understand the relationship between living arrangements and attending
# religious services.

# VARIABLES
#
# currlive (Current living situation)
#   - categorical
#   - "Where do you live now? That is, where do you stay most often?"
#   - five levels are:
#         "Another person's home"
#         "Group quarters, like a dorm, sorority, or fraternity"
#         "Homeless"
#         "Your own place"
#         "Your parent's home"
#
# attreg (Attend regularly)
#   - dichotomous categorical
#   - "Do you attend religious services more than once or twice a year, not
#      counting weddings, baptisms, and funerals?"
#   - two levels are:
#         "Yes"
#         "No"
#
# trauma (Traumatic life events)
#   - dichotomous categorical
#   - "In the past two years have you suffered any traumatic life events – such
#      as someone you were close to dying or you or someone you were close to
#      having a serious accident or illness?"
#   - two levels are:
#         "Yes"
#         "No"
#
# bmi (Body Mass Index)
#   - continuous numerical
#   - "Body Mass Index (NIH calculation) (BMI)"
#
# workhrs (Work hours per week)
#   - continuous numerical
#   - "How many hours in a typical week are you currently working for pay?"

# --------------------------------
# Recoding Procedures
# --------------------------------

# CURRLIVE
# The `currlive` variable has some missing values, so we will recode it into a
# new factored variable, `currlive2`, that replaces those missing values with
# `NA`.
#
# The "Homeless" category has only one response, so we will combine it with 
# "Another person's home" to make "Another person or homeless"
#
# Additionally, when we turn it into a factor, we will order the levels,
# according to generally-agreed-upon ideas around housing stability (Fredrick
# et. al. 2014), from least to most stable housing situation, and rename some of
# the levels to make them more concise.
NSYR$currlive2 <- factor(NA, levels=c(
  "Another person or homeless",
  "Group quarters",
  "Parent's home",
  "Own place"
))
NSYR$currlive2[NSYR$currlive == "Homeless" | NSYR$currlive == "Another person's home"] <- "Another person or homeless"
NSYR$currlive2[NSYR$currlive == "Group quarters, like a dorm, sorority, or fraternity house"] <- "Group quarters"
NSYR$currlive2[NSYR$currlive == "Your parent's home"] <- "Parent's home"
NSYR$currlive2[NSYR$currlive == "Your own place"] <- "Own place"

# When we look at a table of the new variable, we see that the recoding worked:
addmargins(table(NSYR$currlive2, useNA = "ifany"))
#   We use `useNA = "ifany"` to make sure the table displays NA values as well.

# ATTREG
# `attreg` has missing values, so we will recode `attreg` into a new factored
# variable, `attreg2`.
NSYR$attreg2 <- factor(NA, levels=c("Yes", "No"))
NSYR$attreg2[NSYR$attreg == "Yes"] <- "Yes"
NSYR$attreg2[NSYR$attreg == "No"] <- "No"

# Looking at the table of the new variable, the recoding worked again!
addmargins(table(NSYR$attreg2, useNA = "ifany"))

# TRAUMA
# `trauma` has three missing values, so we will recode `trauma` into a new
# factored variable, `trauma2`
NSYR$trauma2 <- factor(NA, levels=c("Yes", "No"))
NSYR$trauma2[NSYR$trauma == "Yes"] <- "Yes"
NSYR$trauma2[NSYR$trauma == "No"] <- "No"

# Make sure the recoding worked
addmargins(table(NSYR$trauma2, useNA = "ifany"))

# BMI
# `bmi` is a character variable, so we will recode it to be numeric:
NSYR$bmi2 <- as.numeric(NSYR$bmi)

# We end up with one `NA`, which is ok. We will just have to deal with that NA
# whenever we use the variable using `na.rm = T`

# WORKHRS1
# Since NSYR$WORKHRS1 is originally a character variable, we are going to
# re-code it into a numerical variable that can be used for ANOVA analysis
NSYR$WORK <- as.numeric(NSYR$WORKHRS1)

# See above with respect to the `NA` warning that R gives us.

# --------------------------------
# Methods: Statistical Tests
# --------------------------------

# Question 1:
#     We used a chi-square test, and then 6 pairwise z-tests

# Question 2:
#     We used a two-sample T-test.

# Question 3:
#     We used ANOVA

# Question 4:
#     We performed a linear regression.

# --------------------------------
# Results: Research Question 1
# --------------------------------

# PLOT
# We will plot the relationship by using a stacked bar plot.
barplot(
  main = "Current Living Arrangements and Attending Services Regularly",
  prop.table(table(NSYR$attreg2, NSYR$currlive2), margin=2) * 100,
  xlab="Current Living Arrangements",
  legend.text=T,
  args.legend = list(
    title = "Attends Religious\nServices Regularly"
  ),
  ylab="Attends Religious Services Regularly (%)"
)

# OPTIONAL -- ORDER BY PROPORTION
# Run the following code, and then plot again in order to order the blocks in
# order of the proportion
    NSYR$currlive2 <- factor(NSYR$currlive2, levels=c(
      "Group quarters",
      "Parent's home",
      "Own place",
      "Another person or homeless"
    ))

# CHI SQUARE ANALYSIS
# Table of observed values:
addmargins(table(NSYR$currlive2, NSYR$attreg2))

# Table with proportions
addmargins(prop.table(table(NSYR$currlive2, NSYR$attreg2), margin=1))

# Perform the chi-square test:
chisq.test(NSYR$currlive2, NSYR$attreg2, correct=F)
# According to R, X-squared = 11.379, df = 3, p-value = 0.009845

# When we check the expected counts, every expected count is above 5 for each
# value, so the analysis is valid
addmargins(chisq.test(NSYR$currlive2, NSYR$attreg2, correct=F)$expected)

# PAIRWISE TESTS

# look at a table
table(NSYR$currlive2, NSYR$attreg2)
addmargins(prop.table(table(NSYR$currlive2, NSYR$attreg2), margin = 1))

# Perform pairwise z-tests
pairwise.prop.test(table(NSYR$currlive2, NSYR$attreg2))
# The one significant value is for Own place and Group quarters, p = 0.0088

# SUMMARY OF RESULTS

# We reject the null hypothesis that current living arrangements and attendance
# at religious services are independent (chi square X^2 = 11.379, p-value = 
# 0.009845). Further analysis using pairwise z-tests indicated that we can
# reject the null hypothesis for the pairing of "Group quarters" and "Own
# place" (p-value = 0.0088); however, we fail to reject the null hypothesis for
# any other pairing (p-values: 0.0864, 0.9424, 0.9424, 0.0748, 0.8866). For "Own
# place" and "Group quarters", 61.08% of people who lived at their "Own place"
# attended religious services regularly, while 70.56% of those who lived in
# "Group quarters" attended regularly.

# --------------------------------
# Results: Research Question 2
# --------------------------------

# PLOT
# We plot the association between the variable by using a side-by-side boxplot
boxplot(
  NSYR$bmi2 ~ NSYR$trauma2,
  main = "Body Mass Index and traumatic life events",
  xlab="Have you experienced a traumatic life event in the past two years?",
  ylab="Body Mass Index (BMI)",
  na.rm = T
)

# The data in the plots looks roughly normally distributed, with some outliers
# on the high end. The variance is also roughly the same.

# Find the total number of valid observations
length(NSYR$trauma2[
  !is.na(NSYR$trauma2)
  & !is.na(NSYR$bmi2)
])

# TWO-SAMPLE T-TEST
# The data satisfies the necessary assumptions:
# 1. independence of samples and observations
# 2. data is roughly normal
# 3. the variance is roughly the same for the two variables

# Table of mean BMI:
tapply(X = NSYR$bmi2, INDEX = NSYR$trauma2, FUN = mean, na.rm = T)

# Table of standard deviation of BMI:
tapply(X = NSYR$bmi2, INDEX = NSYR$trauma2, FUN = sd, na.rm = T)

# Make two new variables containing the two sets of data
trauma_yes <- NSYR$bmi2[NSYR$trauma2 == "Yes"]
trauma_no <- NSYR$bmi2[NSYR$trauma2 == "No"]

# Perform the two-sample T-test
t.test(trauma_yes, trauma_no, var.equal=T)
# According to R, t = 1.6447, df = 2450, and p-value = 0.1002.

# SUMMARY OF RESULTS
# We fail to reject the null hypothesis based on this data at a significance
# level of 0.05. The p-value (0.1002) is higher than 0.05, indicating that the
# difference in means for body mass index across the two groups could be natural
# variation between samples. We therefore fail to show a connection between
# traumatic life events over the past two years and body mass index of youth
# in this study.

# --------------------------------
# Results: Research Question 3
# --------------------------------

# PLOT
# To establish a visual of the relationship between our variables, we are using
# a side-by-side box plot
boxplot(
  NSYR$WORK~NSYR$currlive2,
  main= "The relationship between Work Hours and Living Situations",
  xlab= "Living Situation",
  ylab= "Work Hours"
)

# OPTIONAL -- ORDER BY PROPORTION
# Run the following code, and then plot again in order to order the blocks in
# order of the proportion
    NSYR$currlive2 <- factor(NSYR$currlive2, levels=c(
      "Group quarters",
      "Parent's home",
      "Another person or homeless",
      "Own place"
    ))

# View a table 
tapply(X = NSYR$WORK, INDEX = NSYR$currlive2, FUN = mean, na.rm = T)

# ANOVA Analysis
anova.currlive.workhrs <- aov(NSYR$WORK ~ NSYR$currlive2)
summary(anova.currlive.workhrs)
# With 3 degrees of freedom and a p-value of 0.00000000000000022, we reject the
# null hypothesis and accept the alternative hypothesis

# To make pairwise comparisons in our data set for ANOVA and see if there is
# significance in the difference of means, we will be using a Tukey HSD test and
# then we will use the plot function to present a visual of all the 95%
# confidence intervals in our data.
TukeyHSD(anova.currlive.workhrs)
plot(TukeyHSD(anova.currlive.workhrs))

# --------------------------------
# Results: Research Question 4
# --------------------------------

# PLOT
# Plot the two variables against each other in a scatterplot
plot(
  x = NSYR$WORK,
  y = NSYR$bmi2,
  main = "Work Hours vs. BMI",
  xlab= "Work Hours",
  ylab= "BMI"
)

# Test the correlation between the two variables
cor.test(NSYR$WORK, NSYR$bmi2)
# The p-value is 0.01076, which means that the correlation is significantly
# different from zero at the 0.05 confidence level. The estimate for the
# correlation is 0.05147771.

# LINEAR REGRESSION
m1<-lm(NSYR$bmi2 ~ NSYR$WORK)

# View a summary of the linear model
summary(m1)
# The least squares regression is estimated to be
# y_hat = 24.442280 + 0.013844work_hours

# The p-value for the slope is 0.0108, which is significant at the 0.05 level,
# indicating that the slope is significantly different from zero.

# Plot the linear regression line on to the plot we already have
abline(m1)

# TEST FOR CONDITIONS

# Plot a histogram of the residuals of the linear regression
hist(rstandard(m1)) 

# Plot a q-q plot in order to test for normality
qqnorm(rstandard(m1))
qqline(rstandard(m1))
# Data is close enough to normal

# Plot fitted values against standardized residuals in order to test for
# constant variance
plot(
  predict(m1),
  rstandard(m1),
  main = "Fitted values vs. Standardized residuals",
  xlab= "Fitted Values",
  ylab= "Standardized Residuals"
)

# Add line to the plot above
abline(h = 0, lty = 2)

# Variance seems to generally be relatively constant.
# Our linear regression is therefore valid.

# --------------------------------
# Discussion
# --------------------------------

# SEE SLIDESHOW

# --------------------------------
# References
# --------------------------------

# SEE SLIDESHOW
