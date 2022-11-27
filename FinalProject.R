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
#     Does experiencing a traumatic event influence Body Mass Index?
# --------------------------------

# RATIONALE: Body mass index and whether someone has experienced a traumatic
#            event in the last few years could be related, as experiencing a
#            traumatic event is known to influence eating in youth (Smyth et.
#            al. 2007), and PTSD is known to result in BMI increase in women
#            (Kubzansky et. al. 2014).

# HYPOTHESIS
#     Null: There is no correlation between suffering a traumatic life event in
#           the last two years and Body Mass Index.
#     Alternative: There is a correlation.
#
# RATIONALE: see above. Body Mass Index could be related to traumatic life
#            events. While we do not know what the association might be, we
#            might guess that traumatic life events result in higher BMI, as
#            PTSD in women has been shown to result in BMI increases in women
#            (Kubzansky et. al. 2014).

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
#
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

# TODO: Recode all other variables used

# --------------------------------
# Methods: Statistical Tests
# --------------------------------

# Question 1:
#     We used a chi-square test, and then 6 pairwise tests

# Question 2:
#     We used a two-sample T-test.

# TODO: Questions 3 and 4

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

# TWO-SAMPLE T-TEST
# The data satisfies the necessary assumptions:
# 1. independence of samples and observations
# 2. data is roughly normal
# 3. the variance is roughly the same for the two variables

# Table of mean BMI:
tapply(X = NSYR$bmi2, INDEX = NSYR$trauma2, FUN = mean, na.rm = T)

# Make two new variables containing the two sets of data
trauma_yes <- NSYR$bmi2[NSYR$trauma2 == "Yes"]
trauma_no <- NSYR$bmi2[NSYR$trauma2 == "No"]

# To ensure 

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
# Smyth, Joshua M., Kristin E. Heron, Stephen A. Wonderlich, Ross D. Crosby, and
#     Kevin M. Thompson. 2008. "The Influence of Reported Trauma and Adverse
#     Events on Eating Disturbance in Young Adults." International Journal of
#     Eating Disorders 41 (3): 195-202.
# Kubzansky, Laura D., Paula Bordelois, Hee Jin Jun, et. al. 2014. "The Weight
#     of Traumatic Stress: A Prospective Study of Posttraumatic Stress Disorder
#     Symptoms and Weight Status in Women." JAMA Psychiatry 71 (1):44-51.

