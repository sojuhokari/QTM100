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
# Research Quesion 1:
#     Does one’s living situation determine how often they attend religious
#     services? 
# --------------------------------

# RATIONALE: One's living situation could determine attendance at religious
#            services in numerous ways: the more stable a living situation, the
#            more likely one is to have adequate transportation, a stable
#            schedule, and routines to make it easier to attend religious
#            services.

# HYPOTHESIS
#     Null: There is no correlation between living situation and religious
#           service attendance. The two variables are independent.
#     Alternative: There is a correlation between living situation and religious
#                  service attendance. The two variables are dependent
# RATIONALE: Religious services require some amount of stability in life. When
#            someone is homeless, living in another person's home, or in group
#            quarters, they may not have the ability to find transportation to
#            services, or may need to spend that time on other things.
#            Conversely, if one is living at their own place or at their
#            parent's home, they are more likely to have transportation and time
#            to be able to go to religious services.

# --------------------------------
# Research Quesion 2:
#     TODO
# --------------------------------

# --------------------------------
# Research Quesion 3:
#     TODO
# --------------------------------

# --------------------------------
# Research Quesion 4:
#     TODO
# --------------------------------

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
# TODO: ADD THE REST OF THE VARIABLES

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

# TODO: Recode all other variables used

# --------------------------------
# Methods: Statistical Tests
# --------------------------------

# Question 1:
#     We used a chi-square test, and then 6 pairwise tests

# TODO: Questions 2-4

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
# TODO

# --------------------------------
# Results: Research Question 3
# --------------------------------
# TODO

# --------------------------------
# Results: Research Question 4
# --------------------------------
# TODO

# --------------------------------
# Discussion
# --------------------------------

# TODO

# --------------------------------
# References
# --------------------------------

# Frederick, Tyler J., Michal Chwalek, Jean Hughes, Jeff Karabanow, and Sean
#     Kidd. 2014. “How Stable is Stable? Defining and Measuring Housing
#     Stability.” Journal of Community Psychology 42 (8): 964-979.

