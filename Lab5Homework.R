################################
# Authors: Jaleel Sanders, Akonam Agbu, Soju Hokari
# Document Name: Lab 5 Homework
# Date: 5 October 2022
################################

# --------------------------------
# 1. Import dataset and examine a summary
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
abductees <- read.csv("datasets/abductees.csv", header = T)

# View a summary of the dataset
summary(abductees)

# --------------------------------
# 2. Create an `age` variable
# --------------------------------

# Create a new abductees$age variable
abductees$age <- 90 - abductees$yearbir

# Check that the creation worked
summary(abductees$age)

# --------------------------------
# 3. Create an `age` variable
# --------------------------------

# Create a new abductees$edulevel variable
abductees$edulevel <- factor(NA, levels=c(
  "high school or less",
  "college",
  "more than college"
))

# assign values to abductees$edulevel
abductees$edulevel[abductees$educat <= 12] <- "high school or less"
abductees$edulevel[abductees$educat > 12 & abductees$educat <= 16] <- "college"
abductees$edulevel[abductees$educat > 16] <- "more than college"

# Check that it worked
summary(abductees$edulevel)

# --------------------------------
# 4. Re-code marital status as "Married" or "Other"
# --------------------------------

# Create new variable
abductees$maritalstatus <- factor(NA, levels=c(
  "Married",
  "Other"
))

# assign values to abductees$martialstatus
abductees$maritalstatus[abductees$marstat == "Married"] <- "Married"
abductees$maritalstatus[
  abductees$marstat == "Divorced"
  | abductees$marstat == "Single"
  | abductees$marstat == "Separated"
] <- "Other"

# Check to see that it worked
summary(abductees$maritalstatus)

# --------------------------------
# 5. Examine relationship between age and abduction experience
# --------------------------------

# Produce a side-by-side boxplot
boxplot(abductees$age ~ abductees$abdfeel)

# a. There isn't an easily identified trend, as the boxplots are not in order.

# Re-factor abdfeel as an ordinal categorical variable
# make sure that "Entirely negative" is the first factor, and "entirely positive" is the last.
abductees$abdfeelF <- factor(NA, levels=c(
  "Entirely negative",
  "Mostly negative",
  "About equally positive and negative",
  "Mostly positive",
  "Entirely positive"
))

# assign values
abductees$abdfeelF[abductees$abdfeel == "Entirely negative"] = "Entirely negative"
abductees$abdfeelF[abductees$abdfeel == "Mostly negative"] = "Mostly negative"
abductees$abdfeelF[abductees$abdfeel == "About equally positive and negative"] = "About equally positive and negative"
abductees$abdfeelF[abductees$abdfeel == "Mostly positive"] = "Mostly positive"
abductees$abdfeelF[abductees$abdfeel == "Entirely positive"] = "Entirely positive"

# check to see it worked
summary(abductees$abdfeelF)

# display a boxplot, this time in order
boxplot(abductees$age ~ abductees$abdfeelF)

# b. Now, there is a clear trend where those who had a more positive experience
#    are likely to be older than those who had a mostly negative experience.

# --------------------------------
# 6. Answer some questions
# --------------------------------

# Find the average age of male respondents
mean(abductees$age[abductees$sex == "male"], na.rm = T)

# a. The average age of male respondents at time of survey is 47.47 years.

# Find average number of abduction times among those with "Other" as marital status
mean(abductees$abdtimes[abductees$maritalstatus == "Other"], na.rm = T)

# b. The average number of abduction times among those classified as "Other" for
#    marital status is 3.25.

# Find number with college education (but no more)
length(abductees$edulevel[abductees$edulevel == "college" & !is.na(abductees$edulevel)])

# c. 29 individuals have a college education (but no more)

# d. The general trend is that those who had more positive experiences are likely
#    to be older than those who had more negative experiences.
