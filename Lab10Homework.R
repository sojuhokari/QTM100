################################
# Authors: Jaleel Sanders, Akonam Agbu, Soju Hokari
# Document Name: Lab 10 Homework
# Date: 1 December 2022
################################

# --------------------------------
# Import dataset
# --------------------------------

# The working directory. Change this variable to match the location of this
# directory on your computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
sp13 <- read.csv("datasets/SurveySP13.csv", header = T)

# --------------------------------
# 1. days_drink and GPA
# --------------------------------

# Table of days_drink
table(sp13$days_drink)

# a. The distribution is right-skewed, with the largest proportion of students
#    typically never drinking.

# b. recode days_drink to be a factor variable with three categories:
sp13$days_drink_factor <- factor(NA, c("0 days", "1-2 days", "3 or more days"))
sp13$days_drink_factor[sp13$days_drink == 0] <- "0 days"
sp13$days_drink_factor[
  sp13$days_drink == 1
  | sp13$days_drink == 2
] <- "1-2 days"
sp13$days_drink_factor[
  sp13$days_drink == 3
  | sp13$days_drink == 4
  | sp13$days_drink == 5
  | sp13$days_drink == 6
  | sp13$days_drink == 7
] <- "3 or more days"

# boxplot of gpa and days_drink
boxplot(sp13$GPA ~ sp13$days_drink)

# Table of means
tapply(X = sp13$GPA, INDEX = sp13$days_drink, FUN = mean)

# c. There appears to be a very slight negative association between days_drink
#    and GPA when looking at the boxplot.
#    There does not, however, appear to be an association between the mean of 
#    days_drink and GPA. The mean GPA for those who drink 0 days is 3.488908,
#    while the mean GPA for those who drink 3 days is 3.515370 and the mean GPA
#    for those who drink 4 days is 3.373750.

# boxplot to examine distributions for the new variable
boxplot(sp13$GPA ~ sp13$days_drink_factor)

# histograms
hist(sp13$GPA[sp13$days_drink_factor == "0 days"])
hist(sp13$GPA[sp13$days_drink_factor == "1-2 days"])
hist(sp13$GPA[sp13$days_drink_factor == "3 or more days"])

# d. The distributions are all a little left-skewed, and not very normal for any
#    of the categories.

# --------------------------------
# 2. ANOVA of drinking and GPA
# --------------------------------

# a. Based on the analysis in Question 1, an ANOVA is appropriate, as long as
#    the expected counts are all over 5. There are a few outliers, but those
#    are part of the data. Even though the distributions are all a little left-
#    skewed, they are close enough to normal to use.

# ANOVA
anova.gpa <- aov(sp13$GPA ~ sp13$days_drink_factor)
summary(anova.gpa)

# b. The F-statistic is 0.877

# c. The p-value is 0.418

# d. Based on the findings, there is not a relationship between drinking and GPA

# --------------------------------
# 3. Pairwise comparison
# --------------------------------

# Perform Tukey test to determine which pairs of means differ
TukeyHSD(anova.gpa)
plot(TukeyHSD(anova.gpa))

# a. The measured difference in GPA between those who drink 0 times a week and
#    those who drink 3 or more times was -0.01039453

# b. The p-value associated with this difference was 0.9902620

# c. The pairwise comparison was not significant.

# d. The Tukey test was a waste of time, because the ANOVA output showed that
#    there was no statistically significant difference between groups.

# --------------------------------
# 4. GPA and gender
# --------------------------------

# a. The appropriate test is a two-sample t-test.

# b. The parameter of interest is the difference in means between the GPA of
#    male students and that of female students.

# c.
#    H0: Mu1 = Mu2, or the average GPA of female students equals the average GPA
#        of male students
#
#    HA: Mu1 != Mu2, or the averages are not equal.

# Boxplot
boxplot(sp13$GPA ~ sp13$gender)

# histograms
hist(sp13$GPA[sp13$gender == "Female"])
hist(sp13$GPA[sp13$gender == "Male"])

# d. The assumptions are satisfied because the data are roughly normal.

# perform test
female <- sp13$GPA[sp13$gender == "Female"]
male <- sp13$GPA[sp13$gender == "Male"]
t.test(female, male, var.equal=T)

# e. the test statistic t = 0.70099

# f. the p-value = 0.4841

# g. at alpha = 0.05, we fail to reject the null hypothesis, because the p-value
#    is larger than 0.05.

# h. The 95% confidence interval for the parameter of interest is
#    (-0.0781, 0.164).

# i. Using a two-sample t-test, we tested whether the average GPA of female
#    students differed from the average GPA of male students. We found that,
#    while the means did differ, the difference was not statistically
#    significant at a significance level of 0.05 (p-value 0.4841). We found that
#    the confidence interval for the true value of the difference between GPAs
#    for male and female students was (-0.0781, 0.164). This confidence interval
#    includes zero, which means that we cannot rule out that the true values are
#    actually the same. We therefore failed to reject the null hypothesis,
#    meaning that the GPAs of female and male students are not significantly
#    different.





















