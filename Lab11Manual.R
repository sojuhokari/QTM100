################################
# Author: Soju Hokari
# Document Name: Lab 11 Manual -- Linear Regression
# Date: 2 December 2022
################################

# --------------------------------
# Set up
# --------------------------------

# The working directory. Change this variable to match the location of this
# directory on your computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
mariokart <- read.csv("datasets/mariokart.csv", header = T)

# examine data
str(mariokart)
summary(mariokart)

# --------------------------------
# Experiments
# --------------------------------

# Looking for the relationship between total selling price of the mario kart
# package (total_pr) and other variables

# Histogram of total_pr
hist(mariokart$total_pr)

# boxplot of total_pr 
boxplot(mariokart$total_pr)

# A few packages sold for WAYYY more than any other packages.
# Inspect those packages:
mariokart[mariokart$total_pr>100,]

# Both of these packages had *multiple* games, so we will exclude those packages
mkClean <- subset(mariokart, mariokart$total_pr<100)

# Now examine data
hist(mkClean$total_pr)

# --------------------------------
# Correlation
# --------------------------------

# Scatterplot of number of bids by total price
plot(mkClean$n_bids, mkClean$total_pr)

# Appears to be a random scatter

# When we estimate the correlation, we get this:
cor(mkClean$n_bids, mkClean$total_pr)
# Weak, negative correlation.

# Is the correlation significantly different from zero?
cor.test(mkClean$n_bids, mkClean$total_pr)
# No, the correlation is not significantly different from zero (p = 0.3534)

# For cor and cor.test, the order in which you put the variables in does not
# matter.

# --------------------------------
# Simple Linear Regression
# --------------------------------

# Let's try a linear regression!
# lm = "linear model"

# Estimate regression model
m1 <- lm(mkClean$total_pr ~ mkClean$n_bids)
summary(m1)
# Least squares regression is estimated to be
# y_hat = 49.0979 - 0.1245number_bids
# 
# The intercept is statistically significantly greater than zero (p<0.001),
# the slope is not statistically significantly different from zero (p=0.3534)

# Add the estimated regression line to our scatterplot:
abline(m1)

# We can also learn a lot of other things! For instance, we can extract
# confidence intervals:
confint(m1)

# ***This relationship is non-significant***


# --------------------------------
# Residuals
# --------------------------------

# Get a list of residuals
m1$residuals # regular residuals
resid(m1) # regular residuals
rstandard(m1) # standardized residuals = residuals / standard dev. of residuals
predict(m1) # predicted values

# We can inspect the residuals visually
hist(rstandard(m1))
qqnorm(rstandard(m1)) # qq plot
qqline(rstandard(m1)) # add line to 11 plot

# numeric summary of residuals
summary(rstandard(m1))
sd(rstandard(m1))

# looking closer at the relationship between data, regression model, residuals,
# and fitted values by examining the first observation in the data set
mkClean[1,] # obtain first row (observation) of the data set
predict(m1)[1] # obtain predicted value for first obs. based on model 1 (m1)
resid(m1)[1] # obtain residual for first obs based on m1

# To assess assumptions regarding linearity and constant variance, we compare
# residuals to the fitted (or predicted) values and to the variables included
# in the model.
plot(predict(m1), rstandard(m1), xlab = "Fitted values", ylab = "Standardized
     Residuals") # plot residuals against fitted values
abline(h = 0, lty = 2) # line y = 0
# We see a good scatter about the line y = 0, so the assumptions regarding
# linear relationship and constant variance are satisfied overall

# --------------------------------
# OPTIONAL: Multivariate Regression
# --------------------------------

# Do shipping options still affect the total price of a Mariokart game if we
# account for the number of bids?

# The following recoding was done in Lab 10
#Create new variable with less shipping categories
mkClean$newship <- factor(NA, levels= c("FirstClass/Priority", "UPS", "Standard", "other"))
mkClean$newship[mkClean$ship_sp=="firstClass" |
                  mkClean$ship_sp=="priority"] <- "FirstClass/Priority"
mkClean$newship[mkClean$ship_sp=="ups3Day" | mkClean$ship_sp=="upsGround"] <- "UPS"
mkClean$newship[mkClean$ship_sp=="media" |
                  mkClean$ship_sp=="parcel" |
                  mkClean$ship_sp=="other"] <- "other"
mkClean$newship[mkClean$ship_sp=="standard"] <- "Standard"
table(mkClean$newship,mkClean$ship_sp)

# Look at a bunch of plots for relationships
pairs(~ total_pr + n_bids + newship, data = mkClean)
# the three plots in the top right are the same as the three in the bottom left

# Running a regression with both variables
m2 <- lm(total_pr ~ n_bids + newship, data = mkClean)
summary(m2)
# Every category gets compared to one category that we do NOT see in the final
# model. In this case, it's the "FirstClass/Priority" category that does NOT
# appear. So for example, a UPS package is 9.6727 dollars MORE than a Priority
# package.


# If we wanted, we could keep adding variables:
m3 <- lm(total_pr ~ n_bids + wheels + cond + start_pr + newship,data=mkClean)
summary(m3)

# Including new factors/variables causes the shipping variables to drop out of 
# significance. Once everything is accounted for (price, used/new, #wheels
# included, etc.), shipping speed is no longer a good predictor

# If we want to compare, we can look at the R^2
# for R^2, 0 means none of the variation is explained by the model, while 1
# means all of it is explained by the model. So a hgher R^2 usually means a
# better-fitting model.

# This last model, with a TON of variables in it, did the best job explaining
# the total price of Mariokart (R-squared = 0.7557)


