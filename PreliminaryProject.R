################################
# Authors: Jaleel Sanders, Akonam Agbu, Soju Hokari
# Document Name: Preliminary Project
# Date: 1 November 2022
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
# Explanatory and Response Variables
# --------------------------------

# EXPLANATORY VARIABLE
# For our explanatory variable, we chose `currlive`, which is a categorical
# variable with 5 levels. According to the codebook, the variable is:
#     "Where do you live now? That is, where do you stay most often?"
# The levels are:
#     "Another person's home"
#     "Group quarters, like a dorm, sorority, or fraternity"
#     "Homeless"
#     "Your own place"
#     "Your parent's home"

# A table to see a summary of the `currlive` variable:
addmargins(table(NSYR$currlive))

# The `currlive` variable has some missing values, so we will recode it into a
# new factored variable, `currlive2`, that replaces those missing values with
# `NA`. Additionally, when we turn it into a factor, we will order the levels,
# according to our subjective viewpoint, from least to most stable housing
# situation, and rename some of the levels to make them more concise.
NSYR$currlive2 <- factor(NA, levels=c(
  "Homeless",
  "Another person",
  "Group quarters",
  "Parent's home",
  "Own place"
))
NSYR$currlive2[NSYR$currlive == "Homeless"] <- "Homeless"
NSYR$currlive2[NSYR$currlive == "Another person's home"] <- "Another person"
NSYR$currlive2[NSYR$currlive == "Group quarters, like a dorm, sorority, or fraternity house"] <- "Group quarters"
NSYR$currlive2[NSYR$currlive == "Your parent's home"] <- "Parent's home"
NSYR$currlive2[NSYR$currlive == "Your own place"] <- "Own place"

# When we look at a table of the new variable, we see that the recoding worked:
addmargins(table(NSYR$currlive2, useNA = "ifany"))
# We use `useNA = "ifany"` to make sure the table displays NA values as well.


# RESPONSE VARIABLE
# For our response variable, we chose `attreg`, which is a dichotomous
# categorical variable with two options, "Yes", and "No". According to the
# codebook, the question asked was:
#     "Do you attend religious services more than once or twice a year, not
#     counting weddings, baptisms, and funerals?"

# A table to see a summary of the `attreg` variable:
addmargins(table(NSYR$attreg))

# We again have missing values, so we will recode `attreg` into a new factored
# variable, `attreg2`.
NSYR$attreg2 <- factor(NA, levels=c("Yes", "No"))
NSYR$attreg2[NSYR$attreg == "Yes"] <- "Yes"
NSYR$attreg2[NSYR$attreg == "No"] <- "No"

# Looking at the table of the new variable, the recoding worked again!
addmargins(table(NSYR$attreg2, useNA = "ifany"))

# --------------------------------
# Hypothesis and Rationale
# --------------------------------

# HYPOTHESIS
# The more stable the living situation, the more likely a person is to regularly
# attend religious services. Our guess is that homeless < another person's home
# < group quarters < your parent's home < your own place

# RATIONALE
# Religious services require some amount of stability in life. When someone is
# homeless, living in another person's home, or in group quarters, they may not
# have the ability to find transportation to services, or may need to spend that
# time on other things. Conversely, if one is living at their own place or at
# their parent's home, they are more likely to have transportation and time to
# be able to go to religious services.

# --------------------------------
# Create a Plot to Visualize the Relationship
# --------------------------------

# We will plot the relationship by using a stacked bar plot.
barplot(
  main = "Current Living Arrangements and Attending Services Regularly",
  table(NSYR$attreg2, NSYR$currlive2),
  xlab="Current Living Arrangements",
  legend.text=T,
  args.legend = list(
    title = "Attends Religious Services Regularly",
    x=0,
    xjust=0
  ),
  ylab="Count"
)

# --------------------------------
# Chi-Square Analysis
# --------------------------------

# While the bar plot shows some evidence that current living arrangement and
# attending services regularly are not independent, we need to run a chi-square
# analysis to be sure.
#
# Null hypothesis: current living arrangements and regularly attending religious
# services are independent
# Alternative hypothesis: the two are not independent

# Table of observed values:
addmargins(table(NSYR$currlive2, NSYR$attreg2))

# Perform the chi-square test:
chisq.test(NSYR$currlive2, NSYR$attreg2, correct=F)
# According to R, X-squared = 12.96, df = 4, p-value = 0.01147

# However, on closer examination, the expected counts are not over 5 for all
# cells:
addmargins(chisq.test(NSYR$currlive2, NSYR$attreg2, correct=F)$expected)
# We have two cells with expected counts below 5.

# Running Fisher's exact test gives us roughly the same results; however, the
# p-value is slightly lower:
fisher.test(NSYR$currlive2, NSYR$attreg2)
# p-value: 0.00718

# Either way, the p-value (0.01147 for the chi square test, 0.00718 for Fisher's
# exact test) is lower than 0.05, indicating that we can reject the null
# hypothesis at a significance level of 5%.
#
# Our hypothesis that the two variables are linked is therefore correct. We do
# not, however, have evidence that they are linked in the way we think they are;
# in fact, a look at the bar plot indicates that our subjective viewpoint on the
# order of the levels from least to most stable housing situation does not
# correspond to higher rates of regular attendance of religious services.

# --------------------------------
# Two-Sample Z Test
# --------------------------------

# Finally, we perform a 2-sample z test. In order to do so, we dichotomize our
# explanatory variable into "Own place" and "Living with others":
NSYR$currlive_dichotomized <- NA
NSYR$currlive_dichotomized[NSYR$currlive2 == "Own place"] <- "Own place"
NSYR$currlive_dichotomized[
  NSYR$currlive2 != "Own place" 
  & !is.na(NSYR$currlive2)
] <- "Living with others"

# Check to make sure the new variable creation worked
addmargins(table(NSYR$currlive_dichotomized, useNA = "ifany"))

# Now that the `currlive2` variable is dichotomized into `currlive_dichotomized`,
# we can perform the two-sample z test.

# First, we make a table, making sure that we list the variables in the correct
# order
attends_living_table <- table(NSYR$currlive_dichotomized, NSYR$attreg2)
# The table omits any entries that include an NA in either the `attreg2` or
# `currlive_dichotomized` variable

# Next, we can look at the table to make sure we did it right:
addmargins(attends_living_table)
addmargins(prop.table(attends_living_table, margin=1))

# Finally, we run the test:
prop.test(attends_living_table, correct=F)
# x-squared: 3.5368, df = 1, p-value = 0.06002
#
# In this case,the p-value is higher than 0.05, indicating that we fail to
# reject the null hypothesis at a significance level of 5%.
#
# While we were able to prove that our two variables were linked in the chi-
# square analysis above, once dichotomized, the two variables can no longer be
# proven to not be independent. In other words, the difference between the two
# variables could be explained by random chance at a 5% significance level.


