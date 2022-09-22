################################
# Authors: Jaleel Sanders, Akonam Agbu, Soju Hokari
# Document Name: Lab 3 Homework
# Date: 21 September 2022
################################

# --------------------------------
# Set up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
fruitfly <- read.csv("datasets/fruitfly.csv", header = T)

# Turn `fruitfly$type` into a factor
fruitfly$typeF <- factor(fruitfly$type, labels = c(
  "no females",
  "1 newly pregnant female",
  "8 newly pregnant females",
  "1 virgin female",
  "8 virgin females"
))

# --------------------------------
# 1. Compare the distribution of `lifespan` among the five experimental groups
#    of fruit flies
# --------------------------------

# make a side-by-side boxplot of lifespan by type, and add a title and labels
boxplot(
  fruitfly$lifespan ~ fruitfly$typeF,
  main="Fruitfly lifespan by type of experimental assignment",
  xlab="type of experimental assignment",
  ylab="lifespan"
)

# a. To compare the distribution of `lifespan` among the five groups, we
#    produced a side-by-side box plot

# Assign the standard deviation and mean of the lifespan of type 5 to the
# variables `lifespanSD5` and `lifespanMean5` respectively
lifespanSD5 <- sd(fruitfly$lifespan[fruitfly$typeF == "8 virgin females"])
lifespanMean5 <- mean(fruitfly$lifespan[fruitfly$typeF == "8 virgin females"])

# Print the result to the console
lifespanSD5
lifespanMean5

# b. The group with 8 virgin females has the lowest average lifespan.
#    The standard deviation is 12.1 days, and the mean is 38.7 days.


# --------------------------------
# 2. Compare the distribution of `lifespan` among the five experimental groups
#    of fruit flies
# --------------------------------

# The mean and the standard deviation of the group supplied with 8 virgin
# females are 38.72 and 12.10207 respectively, so we can use that to calculate
# the probabilities

# Find the probability of surviving less than 30 days, based on the mean and
# standard deviation
pnorm(q=30, mean=lifespanMean5, sd=lifespanSD5)

# P(30-50 days)
pnorm(q=50, mean=lifespanMean5, sd=lifespanSD5) - pnorm(q=30, mean=lifespanMean5, sd=lifespanSD5)

# P(50-70 days)
pnorm(q=70, mean=lifespanMean5, sd=lifespanSD5) - pnorm(q=50, mean=lifespanMean5, sd=lifespanSD5)

# P(>70 days)
1 - pnorm(q=70, mean=lifespanMean5, sd=lifespanSD5)

# a.         | Supplied with 8 newly | Supplied with 8
#            |      pregnant females |  virgin females
#    Days    | N (63.4, 14.5)        | N(38.72, 12.10207)
#    --------|-----------------------|------------------
#    <= 30   | 0.01                  | 0.24
#    30 - 50 | 0.17                  | 0.59
#    50 - 70 | 0.50                  | 0.17
#    > 70    | 0.32                  | 0.00

# b. If five fruit flies escaped from their experimental conditions in a
#    different lab and were found to have survived 81, 65, 70, and 56 days,
#    we would assume they came from the group 'supplied with 8 newly pregnant
#    females' because the mean of the four escaped flies' lifespans is around
#    68 or so, which matches with the fact that 50% of flies in the group
#    'supplied with 8 newly pregnant females' survived between 50 and 70 days.
#    Only 17% of flies in the group 'supplied with 8 virgin females' survived
#    over 50 days, while 100% of the escaped flies survived over 50 days.

# --------------------------------
# 3. Create a subset of the dataset
# --------------------------------

# Create a subset that contains only observations of fruit flies in the group
# with the shortest lifespan
fruitflysubset<-subset(fruitfly,typeF=="8 virgin females")

# Find the theoretical value for the 10th percentile of the group 'supplied
# with 8 virgin females'
qnorm(p=0.10, mean=mean(fruitflysubset$lifespan), sd=sd(fruitflysubset$lifespan))

# Find the observed value for the 10th percentile of the same group
quantile(x=fruitflysubset$lifespan, probs=0.10)

# 25th percentile theoretical
qnorm(p=0.25, mean=mean(fruitflysubset$lifespan), sd=sd(fruitflysubset$lifespan))

# 25th percentile observed
quantile(x=fruitflysubset$lifespan, probs=0.25)

# 50th percentile theoretical
qnorm(p=0.50, mean=mean(fruitflysubset$lifespan), sd=sd(fruitflysubset$lifespan))

# 50th percentile observed
quantile(x=fruitflysubset$lifespan, probs=0.50)

# 75th percentile theoretical
qnorm(p=0.75, mean=mean(fruitflysubset$lifespan), sd=sd(fruitflysubset$lifespan))

# 75th percentile observed
quantile(x=fruitflysubset$lifespan, probs=0.75)

# 90th percentile theoretical
qnorm(p=0.90, mean=mean(fruitflysubset$lifespan), sd=sd(fruitflysubset$lifespan))

# 90th percentile observed
quantile(x=fruitflysubset$lifespan, probs=0.90)

#            | Supplied with 8 newly  | Supplied with 8        | 
#            |       pregnant females |       virgin females   |
#            |     N (63.4, 14.5)     |     N(38.72, 12.10207) |
#            |                        |                        |
# Percentile | Theoretical | Observed | Theoretical | Observed |
# -----------|-------------|----------|-------------|----------|
# 10th       | 44.8        | 41.8     | 23.2        | 21.8     |
# 25th       | 53.6        | 56.0     | 30.6        | 32       |
# 50th       | 63.4        | 65.0     | 38.7        | 40       |
# 75th       | 73.2        | 77.0     | 46.9        | 47       |
# 90th       | 82.0        | 79.4     | 54.2        | 54       |

# --------------------------------
# 4. Compare the `lifespan` distribution between the group supplied with 8
#    virgin females and the group supplied with 8 newly pregnant females
# --------------------------------

# Find the theoretical proportion of fruit flies that survived at least 50 days
1 - pnorm(q=50, mean=mean(fruitflysubset$lifespan), sd=sd(fruitflysubset$lifespan))

# Find the observed proportion of fruit flies that survived at least 50 days
length(fruitflysubset$lifespan[fruitflysubset$lifespan >= 50]) / length(fruitflysubset$lifespan)

# a. 20% of fruit flies supplied with 8 virgin females srvived at least 50 days,
#    according to the observed data (17.6% according to the theoretical normal
#    distribution)

# Use the binomial distribution to calculate the probability that 0 out of 10
# flies in the group 'supplied with 8 virgin females' lives 50 days or more
dbinom(x = 0, size = 10, prob = 0.20)

# Use a for loop to calculate the probability that `i` out of 10 flies in the
# group lives 50 days or more, where `i` is a number in the range 1 to 10.
# Basically, do the same thing we did in the last line of code, but do it in
# a loop so we don't have to write as much code
for (i in 1:10) {
  print(i)
  print(
    dbinom(x = i, size = 10, prob = 0.20)
  )
}

# b.   | Supplied with 8 newly | Supplied with 8     |
#      |      pregnant females |      virgin females |
#      | Bin(10, 0.76)         | Bin(10, 0.20)       |
#    --|-----------------------|---------------------|
#    0 | 0.00                  | 0.11                |
#    1 | 0.00                  | 0.27                |
#    2 | 0.00                  | 0.30                |
#    3 | 0.00                  | 0.20                |
#    4 | 0.01                  | 0.09                |
#    5 | 0.05                  | 0.02                |
#    6 | 0.13                  | 0.01                |
#    7 | 0.24                  | 0.00                |
#    8 | 0.29                  | 0.00                |
#    9 | 0.20                  | 0.00                |
#   10 | 0.06                  | 0.00                |

# c. The probability of 6 fruit flies surviving at least 50 days from the group
#    'supplied with 8 newly pregnant females' is 13%. The probability of the same
#    happening in the group 'supplied with 8 virgin females' is 1%.
#    The group 'supplied with 8 newly pregnant females' has a higher probability
#    that exactly 6 fruit flies will survive at least 50 days.

# d. Most likely, 8 out of 10 fruit flies will survive in the group 'supplied
#    with 8 newly pregnant females, while 2 out of 10 will survive in the group
#    'supplied with 8 virgin females'. The group 'supplied with 8 newly pregnant
#    females' is expected to have more fruit flies survive at least 50 days.

# e. The probability of at least 5 fruit flies surviving past 50 days in the
#    group 'supplied with 8 newly pregnant females' is 5% + 13% + 24% + 29% +
#    20% + 6% = 97% or so (there is some error due to rounding -- if we count it
#    a different way, then 100% - 1% = 99%).
#    The probability of at least 5 fruit flies surviving past 50 days in the
#    group 'supplied with 8 virgin females' is 2% + 1% = 3% or so (again, there
#    is some error due to rounding).
#    The group 'supplied with 8 newly pregnant fruit flies' is therefore most
#    likely to have at least 5 fruit flies survive past 50 days.