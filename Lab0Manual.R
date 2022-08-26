################################
# Author: Soju Hokari
# Document Name: Lab 0 Manual
# Date: 8/26/22
################################

# highlight code to run it (or ctrl-enter to run the whole line)
2 + 2

# a range -- all numbers between 1 and 20 INCLUSIVE
1:20

# an *object*
x <- c(2, 3, 5, 7)

# indices start at 1. Which is weird, but I think makes sense for stats
x[2] == 3

# Multiply every number in list "x" by 5
# really easy to operate on every number in a list
x2 <- x*5
x2[2] == 15

# to plot x and x2, use plot()
# arguments are apparently named in R when using functions
plot(x=x, y=x2)

# to access the docs of the plot() function:
?plot

# to import a dataset,
# Point and click: make sure to check the "heading" option to use the variable 
# names included in the csv dataset

# Types! chr - string, int - integer, num - float

# Use code to import a dataset:
# same thing with header as the point and click method
# how do you import with a relative path?
yrbss2013 <- read.csv("/Users/sojuhokari/code/QTM100/datasets/yrbss2013.csv", header=T)

# Set a working directory!
setwd("~/code/QTM100")
yrbss2013 <- read.csv("datasets/yrbss2013.csv", header=T)

# mean
mean(yrbss2013$age)

# summary of mean, min, max, quartiles, median
summary(yrbss2013$height_m)

# basic plot
plot(x = yrbss2013$height_m, y = yrbss2013$weight_kg)

# adding more things!
# the "type" argument determines the type of plot
# xlab stands for "x label"
plot(x = yrbss2013$height_m,
     y = yrbss2013$weight_kg,
     type = "p",
     xlab = "Height (m)",
     ylab = "Weight (kg)")
