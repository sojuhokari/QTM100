################################
# Authors: Jaleel Sanders, Akonam Agbu, Soju Hokari
# Document Name: Lab 7 Homework
# Date: 26 October 2022
################################

# --------------------------------
# Set up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
pharynx <- read.csv("datasets/pharynx.csv", header = T)

# --------------------------------
# 1. Investigate if there is an association between survival past 500 days and
#    treatment group
# --------------------------------

# Create a new variable, SURVIVED_PAST_500
pharynx$SURVIVED_PAST_500 <- NA

# If the patient survived more than 500 days from day of diagnosis, set
# SURVIVED_PAST_500 to TRUE. Otherwise, set it to FALSE.
pharynx$SURVIVED_PAST_500[pharynx$TIME >= 500] <- T
pharynx$SURVIVED_PAST_500[pharynx$TIME < 500] <- F

# a. SURVIVED_PAST_500 is a variable that indicates whether each observation
#    survived >= 500 days.

# b. The two tests that could be used to answer this question are a
#    chi-squared test and a two sample z test.
#
#    For the chi-squared test, H0: Proportions of patients in the standard
#    treatment group who survived past 500 days and didn't survive past 500 days
#    will be the same as the proportions in the test treatment group.
#    Ha: One or moreproportions of patients are different across treatment
#    groups.
# 
#    For the two sample z test, H0: Proportion of patients in the standard
#    treatment group who survived past 500 days equals proportion of patients in
#    the test treatment group who survived past 500 days.
#    Ha: Proportion of patients in the standard treatment group who survived
#    past 500 days does not equal proportion of patients in the test treatment
#    group who survived past 500 days.
#
# The chi-squared test will work with any number of levels of a variable,
# while the two sample z test will only work with a variable that has two
# levels.

# Make a table (not necessary, but I want to see the data)
addmargins(table(pharynx$SURVIVED_PAST_500, pharynx$TX))

# Perform the chi-squared test
chisq.test(pharynx$SURVIVED_PAST_500, pharynx$TX, correct=F)

# c. The test treatment group had a lower rate of survival than the standard
#    treatment group. However, the p-value for the test was 0.1607, indicating
#    that the lower rate of survival was not statistically significant, as it
#    falls within a 95% margin of error (84% of samples would be expected to
#    be closer to the standard treatment rate of survival).

# d. The analysis performed is not sufficient to make definitive conclusions
#    regarding the efficacy of the two treatment groups because the p-value is
#    larger than 0.05, indicating that the rate of survival for the test
#    treatment falls within the 95% confidence interval of the rate of survival
#    for the standard treatment.

# --------------------------------
# 2. Check our assumptions for this test
# --------------------------------

# a. All expected cell counts must be over 5 in order for valid inference to
#    occur.

# See expected cell counts
addmargins(chisq.test(pharynx$SURVIVED_PAST_500, pharynx$TX, correct=F)$expected)

# b. All expected cell counts are over 5, so conditions for valid inference are
#    satisfied.

# See expected cell counts for survival past 500 days and stage of tumor
addmargins(chisq.test(pharynx$SURVIVED_PAST_500, pharynx$T_STAGE, correct=F)$expected)

# c. The assumptions regarding cell counts for comparing survival past 500 days
#    and stage of the tumor are violated, because the expected counts for tumor
#    stage 1 (primary tumor measuring 2cm or less in largest diameter) are
#    lower than 5.
#
#    An appropriate alternative test is Fisher's exact test.

# --------------------------------
# 3. Calculate p-values for given test values and degrees of freedom
# --------------------------------

# X^2 = 1, df = 1
1 - pchisq(1, df=1)

# a. 0.3173

# X^2 = 3, df = 1
1 - pchisq(3, df=1)

# b. 0.0833

# X^2 = 5, df = 1
1 - pchisq(5, df=1)

# c. 0.0253

# X^2 = 1, df = 2
1 - pchisq(1, df=2)

# d. 0.6065

# X^2 = 3, df = 2
1 - pchisq(3, df=2)

# e. 0.2231

# X^2 = 5, df = 2
1 - pchisq(5, df=2)

# c. 0.0821

# As the test statistic increases, the p-value DECREASES. Therefore, larger test
# test statistics present MORE evidence against the null hypothesis. The same
# test statistic value with different degrees of freedom CAN result in a
# different conclusion for a specified level of significance (e.g. sigma = 0.05)