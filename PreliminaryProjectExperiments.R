################################
# Authors: Soju Hokari
# Document Name: Preliminary Project Experiments :)
# Date: 30 October 2022
################################

# DATASET CHOICE: NSYR (National Study of Youth and Religion)

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
NSYR <- read.csv("datasets/NSYR_data.csv", header = T)

# Choose one dichotomous categorical variable for your response variable
# attreg
#     "Do you attend religious services more than once or twice a year, not counting
#      weddings, baptisms, and funerals?" -- options are "Yes" and "No"

# Choose one categorical (3 or more levels) explanatory variable
# currlive
#     "Where do you live now? That is, where do you stay most often?"
#     -- options are "Another person's home", "Group quarters, like a dorm,
#        sorority...", "Homeless", "Your own place", and "Your parent's home"

# Research question: "What is the association between attending religious
# services more than once or twice a year and current living situation?"
# 
# Hypothesis: "The more stable the living situation, the more likely a person is
# to regularly attend religious services. My guess would be homeless < another
# person's home < group quarters < your parent's home < your own place"
#
# Rationale: "Religious services require some amount of stability in life. When
# someone is homeless, living in another person's home, or in group quarters,
# they may not have the ability to find transportation to services, or may need
# to spend that time on other things. Conversely, if one is living at their own
# place or at their parent's home, they are more likely to have transportation 
# and time to be able to go to religious services."

# CLEAN THE DATA

# see a table of the attreg variable
addmargins(table(NSYR$attreg))

# We have two missing values, so we turn it into NA and factor the variable
NSYR$attreg2 <- factor(NA, levels=c("Yes", "No"))
NSYR$attreg2[NSYR$attreg == "Yes"] <- "Yes"
NSYR$attreg2[NSYR$attreg == "No"] <- "No"

# look at the table again
addmargins(table(NSYR$attreg2))
# The NA doesn't show up. Making it show up:
addmargins(table(NSYR$attreg2, useNA = "ifany"))

# Next, a table of the currlive variable
addmargins(table(NSYR$currlive))

# Again, we have two missing values. In this case, we're going to factor from
# least to most stable, according to our hypothesis
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

# A table again!
addmargins(table(NSYR$currlive2, useNA = "ifany"))

# CREATE A PLOT TO VISUALIZE

# Plot them together
# divide the plot into one rows and 5 columns
par(mfrow = c(1,5))

?par

# set the range of the y-axis of the histograms so they are equal
ylimits <- c(0, 700)

# plot the histograms
levels = c(
  "Homeless",
  "Another person",
  "Group quarters",
  "Parent's home",
  "Own place"
)

for (i in 1:5) {
  barplot(
    main = levels[i],
    height = table(subset(NSYR, currlive2 == levels[i])$attreg2),
    ylim = ylimits,
    xlab = "Attend Regularly?"
  )
}

par(mfrow = c(1,1))

# OR PLOT LIKE THIS:

?barplot

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

# CHI-SQUARE ANALYSIS
# 
# Null hypothesis: current living arrangements do not affect whether a person
# goes to religious services regularly
# If we reject the null hypothesis, then our hypothesis could be true
#
chisq.test(NSYR$attreg2, NSYR$currlive2, correct=F)
# X-squared = 12.96, df = 4, p-value = 0.01147

# The p-value is 0.01147, indicating that we can reject the null hypothesis at a
# significance level of 5%

# So our hypothesis could be correct -- and looking at the data, it is at least
# partially correct

# See the expected counts for all cells
addmargins(chisq.test(NSYR$attreg2, NSYR$currlive2, correct=F)$expected)

# Compare to actual counts
addmargins(table(NSYR$attreg2, NSYR$currlive2))

# Not all expected counts are 5. So instead should we use the Fisher exact test?

# Fisher's Exact test:
fisher.test(NSYR$attreg2, NSYR$currlive2)
# With Fisher's exact test, the p-value is 0.00718. Which is even lower?

# DICHOTOMIZE, THEN PERFORM A 2-SAMPLE Z TEST

# Dichotomize into two:
NSYR$currliveDichotomized <- NA
NSYR$currliveDichotomized[NSYR$currlive2 == "Own place"] <- "Own place"
NSYR$currliveDichotomized[NSYR$currlive2 != "Own place" & !is.na(NSYR$currlive2)] <- "Living with others"
# Try to explain WHY we dichotomize into these two categories

# Now we can perform a two-sample test

# Make a table
attends_living_table <- table(NSYR$currliveDichotomized, NSYR$attreg2)

# See the table
addmargins(attends_living_table)
addmargins(prop.table(attends_living_table, margin=1))

prop.test(attends_living_table, correct=F)
# x-squared: 3.5368, df = 1, p-value = 0.06002
# Which means that the null hypothesis is NOT rejected with a significance level
# of 5%, which means that our hypothesis is NOT proven.


