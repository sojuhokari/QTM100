################################
# Author: Soju Hokari
# Document Name: Lab 1 Homework
# Date: 1 September 2022
################################

# --------------------------------
# 1. Set-up
# --------------------------------

# Load the data
source("http://www.openintro.org/stat/data/present.R")

# --------------------------------
# 2. Find the variable names in the data set
# --------------------------------

# find the variable names of the data
names(present)

# The variables are "year", "boys", "girls"


# --------------------------------
# 3. Find the dimensions of the data frame
# --------------------------------

# find the dimensions of the data
dim(present)

# There are 63 observations of 3 variables


# --------------------------------
# 4. Find the years included in this data set
# --------------------------------

# get a summary of the data present in the `year` variable of the dataset
summary(present$year)

# The earliest year this dataset covers is 1940. The latest is 2002.


# --------------------------------
# 5. Were boys born in greater proportion than girls in this CDC dataset?
# --------------------------------

# create a vector of bool values, where true means more boys than girls
moreBoysThanGirls <- present$boys > present$girls

# sum the vector
sum(moreBoysThanGirls) # returned 63

# find the number of observations in the data set
length(present$year) # returned 63

# Of the 63 years present in the dataset, boys outnumbered girls in all 63 years
# The relationship of boys being born in greater proportion than girls therefore holds.


# --------------------------------
# 6. Make a plot that displays the proportion of boys over time
# --------------------------------

# Create a new variable in the data that measures the proportion of boys
present$propBoys <- present$boys / (present$boys + present$girls)

# Plot the data, and label the axes
plot(
  x = present$year,
  y = present$propBoys,
  xlab = "Year",
  ylab = "Proportion of births that were male"
)

# There is a general downward trend in the proportion of babies that are born male


# --------------------------------
# 7. Give the plot a title
# --------------------------------

# Plot the data, but this time add a title
plot(
  x = present$year,
  y = present$propBoys,
  xlab = "Year",
  ylab = "Proportion of births that were male",
  main = "Proportion of boys over time",
)

# I used the `main` argument to give the plot a title


# --------------------------------
# 8. Find the number of years in which the proportion of boys exceeded 0.512
# --------------------------------

# create variable `number_of_years`
number_of_years <- 0

# loop through all proportions of boys in the `present$propBoys` vector.
# If the proportion is more than 0.512, add one to variable `number_of_years`
for (prop in present$propBoys) {
  if (prop > 0.512) {
    number_of_years = number_of_years + 1
  }
}

# print variable `number_of_years`
number_of_years

# The proportion of boys exceeded 0.512 in 49 of the 63 years

# --------------------------------
# 9. Find the year in which we saw the largest total number of births in the US
# --------------------------------

# create a new variable in the data that measures total births
present$totalBirths <- present$boys + present$girls

# create variables for the year with the most births, and the number of births that year
max_births_year <- 0
max_births_number <- 0

# Loop over all numbers between 1 and the number of observations in the dataset, inclusive.
# For each observation in the dataset, if there are more births that year than
# there were in the current `max_births_number`, update the `max_births_year`
# and `max_births_number`
for (i in 1:nrow(present)) {
  if (present$totalBirths[i] > max_births_number) {
    max_births_year = present$year[i]
    max_births_number = present$totalBirths[i]
  }
}

# print the year with the most births
max_births_year

# The most births were in 1961.

