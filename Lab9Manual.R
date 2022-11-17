################################
# Author: Soju Hokari
# Document Name: Lab 8 Manual -- Inference for Related Samples and Errors in
#                                Inference
# Date: 11 November 2022
################################

# t-tests: when do we use the?

# - for categorical and dichotomous data, chi-square and proportion tests
# - for quantitative data, t-test!

# PAIRED T TEST HYPOTHESES
# H0: mu(diff) = 0
# HA: mu(diff) != 0

# --------------------------------
# Set up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
evals <- read.csv("datasets/CourseEvals.csv", header = T)

# --------------------------------
# Paired T-Test
# --------------------------------

# We want to compare scores that professors received on course evals with scores
# that classes received. This data is *paired* because the same student will
# have evaluated both the prof and the class

# First, we need to calculate the difference between these two scores for each
# professor
evals$diff <- evals$course_eval - evals$prof_eval

# Look at summary
mean(evals$diff)
sd(evals$diff)
hist(evals$diff)

# Average diff is -0.18. Negative value means course evals tend to be lower than
# prof evals

# Use a t-test to evaluate whether this difference is statistically significant
# from zero

# use the diff variable
t.test(evals$diff)
# OR we could use the two eval variables
t.test(evals$course_eval, evals$prof_eval, paired = T)

# Note: for both one-sample t test and paired t test:
# H0: mu(diff) = 0

# Test statistic is -19.16 with 462 df. At the alpha - 0.05 significance level,
# we reject the null hypothesis that the average difference in evaluations is 0

# Therefore, statistically significant evidence that profs get higher personal
# evaluations than course evaluations

# --------------------------------
# Errors in Inference
# --------------------------------

# Evals data set has 463 ovs and 18 vars. For this second part of the lab, we
# consider these 463 individuals to be the ENTIRE POPULATION OF INTEREST

# First, we import functions from TestingFunctions.R from Canvas
# We can run the code in the r script in a separate file,
# OR
# We can just run the following code:
source("TestingFunctions.R")

# ERRORS
# Type I: Rejecting the null when the null is actually correct
# Type II: Failing to reject the null when the null is not correct

# IDENTIFY THE POPULATION DISTRIBUTION

# See histogram and summary
hist(evals$cls_perc_eval)
mean(evals$cls_perc_eval)
sd(evals$cls_perc_eval)
# left skewed, average percent of 74.4, sd of 16.8

# PERFORM INFERENCE ON MULTIPLE RANDOM SAMPLES FROM THE POPULATION
# Taking multiple samples from the numerical variable to see if we get any
# errors

# We will also perform a hypothesis test about mu. By default, inference.means
# function tests the null hypothesis that mean equals mu

# H0: mu = 74.4

# Store the results of the inference in a new data frame
sim1 <- inference.means(
  variable = evals$cls_perc_eval,
  sample.size = 50,
  alpha = 0.05,
  num.reps = 100
)

# View results
View(sim1)

# The simulation produces a data frame with 7 columns and 100 rows (one row
# for each of the 100 samples drawn and tested)

# ASSUME ASSUMPTIONS FOR INFERENCE
# Assumptions for inference are satisfied
# independence? Simple random sample.
# observations are independent? yup.
# is the sampling distribution of the mean normally distributed? check.

# EXAMINE PERFORMANCE OF HYPOTHESIS TESTING

# hist of sample means
hist(sim1$samp.est, main = "Sample Means")
hist(sim1$test.stat, main = "t test statistics")
hist(sim1$p.val, main = "p-values")

# Hist of our samples means is approximately normal, as is t-statistice.
# Hist of p-value is roughly uniform.

# Frequency table of decisions
table(sim1$decision)

# In our case, 2 of 98 tests rejected H0, indicating an observed Type 1 error
# rate of 4%. This is pretty close to the targeted level (5%)

# EXAMINE PERFORMANCE OF CONFIDENCE INTERVAL ESTIMATION
# Now examine the inference results related to conf int estimation by
# visualizing the conf ints with the plot.ci function

# Generate plot of conf ints
plot.ci(results = sim1, true.val = 74.4)
# Here, we see two conf ints in red that do not actually capture the true
# parameter values of mu - 74.4, which means TYPE I ERROR

# Frequency table displaying number of times the true mean was captured
table(sim1$capture)

# AGREEMENT BETWEEN THE CONF INT AND THE HYPOTHESIS TEST

# Contingency table of capture vs decision:
table(sim1$capture, sim1$decision)

# LONG RUN PERFORMANCE
# Let's run the simulation 10000 times!
sim1long <- inference.means(
  variable = evals$cls_perc_eval,
  sample.size = 50,
  alpha = 0.05,
  num.reps = 10000
)

# Freq table of captures
table(sim1long$capture)

#Frequency table of decisions
table(sim1long$decision)

# WHY IS MINE ALWAYS BEING TOO LOW???

proportions <- repeat(NA, 10000)

for (i in 1:10000) {
  
}
