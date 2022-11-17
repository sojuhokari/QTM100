################################
# Authors: Jaleel Sanders, Akonam Agbu, Soju Hokari
# Document Name: Lab 9 Homework
# Date: 16 November 2022
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
yrbss <- read.csv("datasets/yrbss2013.csv", header = T)

# Import the required file
source("TestingFunctions.R")

# --------------------------------
# 1. Explore inferential results when we repeatedly sample from height_m
# --------------------------------

# Examine population distribution of height_m
hist(yrbss$height_m)

# mean
summary(yrbss$height_m)

# standard deviation
sd(yrbss$height_m)

# a. The population distribution is roughly normal. The population mean is
#    1.687 and the population standard deviation is 0.10298.

# b. If we take samples of size n = 20 from the population, the sampling
#    distribution assumptions will be satisfied for valid inference because
#    the population distribution is approximately normal, even though the
#    samples are not size n = 30 or larger.

# c. We are testing H0: XBAR = MU versus Ha: XBAR != MU. In the hypothesis test,
#    we run the risk of committing a TYPE I error because in reality the null
#    hypothesis is actually TRUE. The targeted TYPE I error rate is 0.05 (5%)
#    and the targeted confidence interval coverage is 95%. Because sampling
#    distribution assumptions ARE satisfied, we expect the observed Type I error
#    rate and confidence interval coverage from simulation results to EQUAL the
#    targeted levels.

# Take 100 samples of size n = 20 at the 0.05 significance level
sim1 <- inference.means(
  variable = yrbss$height_m,
  sample.size = 20,
  alpha = 0.05,
  num.reps = 100
)

# See the shape of the distributions of sample means, t test stats, and p values
hist(sim1$samp.est)
hist(sim1$test.stat)
hist(sim1$p.val)

# See how often we rejected the null
table(sim1$decision)

# d. The shape of the distribution of sample means:      roughly normal
#                                  of t test statistics: roughly normal
#                                  of p-values:          uniform
#    7% of the samples commit an error in hypothesis test results (type I)

# Plot the confidence intervals
plot.ci(results = sim1, true.val = 1.687)

# e. 93% of samples have confidence intervals that capture the true parameter
#    value. The confidence intervals tend to be similar in length but are not
#    the same length. They are generally pretty randomly scattered about the
#    true mean. They also generally provide reasonable bounds for the mean.

# f. Everyone has similar results. We believe the inferential methods are
#    performing as expected, as everyone had around 5% of samples that made a
#    Type I error (although some were higher and some were lower)

# --------------------------------
# 2. Explore inferential results when we repeatedly sample from days smoke
# --------------------------------

# Examine population distribution of height_m
hist(yrbss$days_smoke)

# mean
summary(yrbss$days_smoke)

# standard deviation
sd(yrbss$days_smoke)

# a. The population distribution is right skewed and definitively not normal.
#    The population mean is 1.496 and the population standard deviation is
#    5.700752.

# b. If we take samples of size n = 20 from the population, the sampling
#    distribution assumptions will not be satisfied for valid inference because
#    the population distribution is not normal. To satisfy the conditions, we
#    would have to take samples of size n = 30 or larger.

# c. We are testing H0: XBAR = MU versus Ha: XBAR != MU. In the hypothesis test,
#    we run the risk of committing a TYPE I error because in reality the null
#    hypothesis is actually TRUE. The targeted TYPE I error rate is 0.05 (5%)
#    and the targeted confidence interval coverage is 95%. Because sampling
#    distribution assumptions ARE NOT satisfied, we expect the observed Type I
#    error rate and confidence interval coverage from simulation results to NOT
#    EQUAL the targeted levels.

# Take 100 samples of size n = 20 at the 0.05 significance level
sim2 <- inference.means(
  variable = yrbss$days_smoke,
  sample.size = 20,
  alpha = 0.05,
  num.reps = 100
)

# See the shape of the distributions of sample means, t test stats, and p values
hist(sim2$samp.est)
hist(sim2$test.stat)
hist(sim2$p.val)

# See how often we rejected the null
table(sim2$decision)

# d. The shape of the distribution of sample means:      right skew
#                                  of t test statistics: left skew
#                                  of p-values:          uniform? But with a
#                                                        concentration on the
#                                                        left. So maybe right
#                                                        skew?
#    37% of the samples commit an error in hypothesis test results (type I)

# Plot the confidence intervals
plot.ci(results = sim2, true.val = 1.496)

# e. 63% of samples have confidence intervals that capture the true parameter
#    value. The confidence intervals differ widely in length, with some shorter
#    ones that have a mean just above zero. This makes sense because we had a
#    lot of values near zero in our population due to the right skew, so given
#    the small sample size, those values were easy to accidentally overrepresent

# f. Everyone has similarly weird and wacky results. The inferential methods are
#    not performing well, but we did not expect them to perform well considering
#    the small sample size and the non-normal population distribution.

# --------------------------------
# 3. Simulation exercise
# --------------------------------

# Sim with n = 20
height_m_sim_n20 <- inference.means(
  variable = yrbss$height_m,
  sample.size = 20,
  alpha = 0.05,
  num.reps = 10000
)
# See how often we rejected the null
table(height_m_sim_n20$decision)
# Plot the confidence intervals
plot.ci(results = height_m_sim_n20, true.val = 1.687)

# Sim with n = 50
height_m_sim_n50 <- inference.means(
  variable = yrbss$height_m,
  sample.size = 50,
  alpha = 0.05,
  num.reps = 10000
)
# See how often we rejected the null
table(height_m_sim_n50$decision)
# Plot the confidence intervals
plot.ci(results = height_m_sim_n50, true.val = 1.687)

# Sim with n = 100
height_m_sim_n100 <- inference.means(
  variable = yrbss$height_m,
  sample.size = 100,
  alpha = 0.05,
  num.reps = 10000
)
# See how often we rejected the null
table(height_m_sim_n100$decision)
# Plot the confidence intervals
plot.ci(results = height_m_sim_n100, true.val = 1.687)

# height_m                              | n = 20 | n = 50 | n = 100
# -------------------------------------------------------------------
# Assumptions satisfied?                | yes    | yes    | yes
# Observed Type I error rate            | 5.19%  | 4.78%  | 4.83%
# Observed confidence interval coverage | 94.81% | 95.22% | 95.17%
# Valid inference?                      | yes    | yes    | yes


# Sim with n = 20
days_smoke_sim_n20 <- inference.means(
  variable = yrbss$days_smoke,
  sample.size = 20,
  alpha = 0.05,
  num.reps = 10000
)
# See how often we rejected the null
table(days_smoke_sim_n20$decision)
# Plot the confidence intervals
plot.ci(results = days_smoke_sim_n20, true.val = 1.496)

# Sim with n = 50
days_smoke_sim_n50 <- inference.means(
  variable = yrbss$days_smoke,
  sample.size = 50,
  alpha = 0.05,
  num.reps = 10000
)
# See how often we rejected the null
table(days_smoke_sim_n50$decision)
# Plot the confidence intervals
plot.ci(results = days_smoke_sim_n50, true.val = 1.496)

# Sim with n = 100
days_smoke_sim_n100 <- inference.means(
  variable = yrbss$days_smoke,
  sample.size = 100,
  alpha = 0.05,
  num.reps = 10000
)
# See how often we rejected the null
table(days_smoke_sim_n100$decision)
# Plot the confidence intervals
plot.ci(results = days_smoke_sim_n100, true.val = 1.496)

# days_smoke                            | n = 20 | n = 50 | n = 100
# -------------------------------------------------------------------
# Assumptions satisfied?                | no     | yes    | yes
# Observed Type I error rate            | 30.82% | 13.31% | 9.10%
# Observed confidence interval coverage | 69.18% | 86.69% | 90.90%
# Valid inference?                      | no     | no     | maybe

# --------------------------------
# 4. Big picture
# --------------------------------

# a. Increasing the sample size does not fix our results. Increasing the sample
#    does not lower the Type I error rate. This is because the odds of picking
#    the right sample of values in a normally distributed population is 95%,
#    given our confidence interval.

# b. When assumptions are satisfied, we are much closer to having a "valid"
#    inference. However, we do not always have a valid inference. This means
#    that the Central Limit Theorem and other assumptions required are not hard
#    boundaries (i.e. there is nothing magic about the nujmber 30); they are
#    simply guidelines to help us have a valid inference.