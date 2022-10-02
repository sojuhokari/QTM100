################################
# Author: Soju Hokari
# Document Name: Lab 5 Manual -- Data Cleaning & Manipulation
# Date: 30 September 2022
################################

# Before analyzing data, we might want to...
#   Clean -> remove outliers/erroneous values or categories
#   Convert it from one variable type to another

# --------------------------------
# Set up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
acs <- read.csv("datasets/acs.csv", header = T)

# --------------------------------
# Re-code values
# --------------------------------

# Table and histogram of age to see whether there are any implausible values
table(acs$Age)
hist(acs$Age)

# 130 is an implausible value, and a complete outlier
# 0 may seem implausible, but there are a lot of 0s, so it seems to be fine

# Create a new Age variable that recodes ages of 130 as missing
acs$Age2 <- acs$Age

# identify implausible observations
acs$Age == 130
summary(acs$Age>100)
summary(acs$Age2)

# View position 157 in Age2
acs$Age2[157]

# Position 157 is age 130, which is implausible

# Re-code 157th entry of Age2 to NA
acs$Age2[157] <- NA

# Or, to make things more simple, we can recode all values over 100 to be NA
acs$Age2[acs$Age>100] <- NA

# Verify re-coding of Age2
summary(acs$Age2)

# We could index in to the acs dataset without using the variable name as well
acs[157, 2] # acs row 157, column 2

acs[157, ] # acs row 157, all columns

# Or, we could use a condition
acs[acs$Age>100,]

# Might need to use additional arguments in some R functions, due to the missing
# value
mean(acs$Age2) # Cannot calculate mean

# argument to ignore missing values
mean(acs$Age2, na.rm=T)

# --------------------------------
# Create new variables
# --------------------------------

# Classifying people into age groups

# Create new variable
acs$AgeCategory <- factor(NA, levels=c("child", "adult", "senior citizen"))

# This is what it looks like:
acs$AgeCategory

# Assign values of age category
acs$AgeCategory[acs$Age2 <= 18] <- "child"
acs$AgeCategory[acs$Age2 > 18 & acs$Age2 <= 55] <- "adult"
acs$AgeCategory[acs$Age2 > 55] <- "senior citizen"

# Check coding of age category
table(acs$Age2, acs$AgeCategory)


# Re-coding `race` to be "white" vs "non-white"

# Create new variable for race
acs$RaceNew <- factor(NA, levels=c("white", "non-white"))

# Re-assign values of the new race variable
acs$RaceNew[acs$Race == "white"] <- "white"
acs$RaceNew[acs$Race == "asian" | acs$Race == "black" | acs$Race == "other"] <- "non-white"

# Check the re-coding
table(acs$Race, acs$RaceNew)
