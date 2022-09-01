################################
# Author: Soju Hokari
# Document Name: Lab 1 Manual
# Date: 1 September 2022
################################

# --------------------------------
# MARK: Set-up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
arbuthnot <- read.csv("datasets/arbuthnot.csv", header=T)

# --------------------------------
# MARK: Experiments with calculating data!
# --------------------------------

# Look at the data:
arbuthnot

# Look at the first six data entries:
head(arbuthnot)

# Look at the last six data entries:
tail(arbuthnot)

# See the dimensions of this data frame:
dim(arbuthnot)
# dimensions are shown in the format (rows, columns)

# See a summary of the data:
summary(arbuthnot)
# The summary gives us a bunch of quick, useful info: minimum, maximum, mean, median,
# 1st quartile, and 3rd quartile.

# Access the data in a single column of a data frame:
arbuthnot$boys
# This returns a `vector` of integers, which is unstructured data in a list
# --- a set of numbers
# numbers in brackets on the left indicate position in the vector

# basic addition!
5218 + 4683

# But now, we do not-so-basic addition!
# Adding together each of the values in the following vectors (adding together; NOT
# summing the vector)
arbuthnot$boys + arbuthnot$girls

# Calculate the proportion of baptisms that were girls:
4683 / (4683 + 5218)

# Save the proportion as another column in the arbuthnot dataset!
arbuthnot$propGirls <- arbuthnot$girls / (arbuthnot$girls + arbuthnot$boys)

arbuthnot
arbuthnot$propGirls

# Using greater than, less than, equality, inequality:
# Calculate whether there are more girls than boys in each year:
arbuthnot$girls > arbuthnot$boys

# In R, TRUE = 1 and FALSE = 0
# So, summing up our data, we will get an answer of 0:
sum(arbuthnot$girls > arbuthnot$boys)

# --------------------------------
# MARK: Experiments with plotting data!
# --------------------------------

# Plot the year vs the proportion of babies that were girls
plot(x = arbuthnot$year, y = arbuthnot$propGirls)

# Connect the data with lines
plot(x = arbuthnot$year, y = arbuthnot$propGirls, type = "l")

# To find all the possible arguments to a function:
?plot

# Graphical parameter settings!
# Everything that you can control with the plot
# Some things, you have to call par for
?par

# Let's change the color of the plot!
plot(x = arbuthnot$year, y = arbuthnot$propGirls, type = "l", col = 2)
plot(x = arbuthnot$year, y = arbuthnot$propGirls, type = "l", col = "plum")

# And let's get all the possible colors:
colors()
