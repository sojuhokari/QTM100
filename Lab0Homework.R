################################
# Author: Soju Hokari
# Document Name: Lab 0 Homework
# Date: 8/26/22
################################

# Facts:
# 29.4506012732846% of youth in the dataset are sad.
# The mean age of youth in the dataset is 15.8035840603631 years, and the median
# age is 16 years.
# There are 4277 females, 4205 males, and 0 people of other genders in the dataset.

# Plot:
# The command produced a plot that plotted age in years (an integer value) on the 
# x axis, and height in meters (a floating-point value) on the y axis. There seems
# to be a slight positive trend.

# --------------------------------
# MARK: Set-up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
yrbss2013 <- read.csv("datasets/yrbss2013.csv", header=T)


# --------------------------------
# MARK: Calculate the percent of youth who are sad
# --------------------------------

# Create a total_sad variable to count the total number of sad youth
total_sad <- 0

# For each entry in the "sad" column of the dataset, if the entry is "yes", then
# add one to the "total_sad" variable
for (x in yrbss2013$sad) {
  if (x == "yes") {
    total_sad <- total_sad + 1
  }
}

# Get the total number of youth (both sad and not sad)
total_youth <- length(yrbss2013$sad)

# Calculate the percentage of youth who are sad
percent <- (total_sad / total_youth) * 100

# print out the result
print(paste(percent, "% of youth in the dataset are sad."))


# --------------------------------
# MARK: Get the median and mean for age
# --------------------------------

# Print the mean age
print(paste("The mean age is", mean(yrbss2013$age), "years"))

# Print the median age
print(paste("The median age is", median(yrbss2013$age), "years"))


# --------------------------------
# MARK: Get the number of people of each gender in the dataset
# --------------------------------

# Create variables to count gender
total_male <- 0
total_female <- 0
total_other <- 0

# For each entry in the "gender" column of the dataset, add one to the appropriate
# variable to count totals of each gender
for (x in yrbss2013$gender) {
  if (x == "male") {
    total_male <- total_male + 1
  } else if (x == "female") {
    total_female <- total_female + 1
  } else {
    total_other <- total_other + 1
  }
}

# Get the total number of youth
total_youth <- length(yrbss2013$gender)

# Print number of females
print(paste("There are", total_female, "females in the dataset"))

# Print number of males
print(paste("There are", total_male, "males in the dataset"))

# Print number of people of other genders
print(paste("There are", total_other, "people of other genders in the dataset"))


# --------------------------------
# MARK: Display one plot of age vs. height
# --------------------------------

# display a plot, with labels
plot(x = yrbss2013$age, y = yrbss2013$height_m, type = "p", xlab = "Age (years)", ylab = "Height (m)")


