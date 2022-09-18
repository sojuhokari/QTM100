################################
# Authors: Jaleel Sanders, Akonam Agbu, Soju Hokari
# Document Name: Lab 2 Homework
# Date: 16 September 2022
################################

# --------------------------------
# 1. How many observations in the ADNI.txt data set?
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
adni <- read.table("datasets/ADNI.txt", header = T)

# Find the number of observations by measuring the length of `adni$AGE`
length(adni$AGE)

# There are 276 observations in the data set

# --------------------------------
# 2. describe variables and type
# --------------------------------

# Get info on the variables and types
str(adni)

# Variables and types:
# 
# DESCRIPTION | DX          | AGE        | APOE4        | GENDER      | MMSE      | adas      | WholeBrain |
# In reality  | Categorical | Numerical  | Categorical  | Categorical | Numerical | Numerical | Numerical  |
# In R        | chr         | num        | int          | chr         | int       | num       | int        |

# --------------------------------
# 3. Recode APOE4 as a factor, and determine which variant is the least common
# --------------------------------

# Turn adni$APOE4 into a factor, and assign it to a new variable in the data set: adni$APOE4
# The three options are now "No copies", "One copy", and "Two copies"
adni$APOE4F <- factor(adni$APOE4, labels = c("No copies", "One copy", "Two copies"))

# Get a summary of the adni$APOE4F variable
summary(adni$APOE4F)

# Two copies is the least common

# --------------------------------
# 4. Visualize the distribution of age
# --------------------------------

# make a box plot, and label it with age in years
boxplot(adni$AGE, ylab="Age (years)")

# A boxplot is the right graph to visualize the distribution of age, because it is a single variable!

# --------------------------------
# 5. examine the relationship between Alzheimer’s diagnosis (DX)
#    and the two cognitive impairment tests (MMSE and adas).
# --------------------------------

# Make a side-by-side boxplot comparint MMSE to DX
boxplot(adni$MMSE ~ adni$DX, xlab="Alzhemer's Diagnosis", ylab="MMSE")

# Make a side-by-side boxplot comparint adas to DX
boxplot(adni$adas ~ adni$DX, xlab="Alzhemer's Diagnosis", ylab="adas")

# a. A side-by-side box plot is the best option to compare the relationship between
#    Alzheimers and the two cognitive impairment tests.

# b. The median and quartiles of MMSE are highest for Normal diagnoses,
#    followed by MCI diagnoses. They are lowest for AD diagnoses.
#    The median and quartiles of adas are all lowest for Normal diagnoses, with
#    AD diagnoses highest, and MCI in the middle.

# c. Adas identifies potential outliers, that are higher in value than the whiskers
#    of the box plot.

# --------------------------------
# 6. Identify descriptive statistics for the overall characteristics of the study participants, and for each diagnosis group.
# --------------------------------

# a. divide the WholeBrain volume by 100,000
adni$WholeBrainSmall <- adni$WholeBrain / 100000

# Get the number in each diagnosis group
table(adni$DX)

# Create a data subset that only includes observations where the diagnosis is "AD"
adniAD <- subset(adni, DX == "AD")

# Create a data subset that only includes observations where the diagnosis is "MCI"
adniMCI <- subset(adni, DX == "MCI")

# Create a data subset that only includes observations where the diagnosis is "Normal"
adniNormal <- subset(adni, DX == "Normal")

# Find the mean of the age of the "AD" subset
mean(adniAD$AGE)

# Find the standard deviation of the age of the "AD" subset
sd(adniAD$AGE)

# Find the mean and standard deviation of the age for "MCI" and "Normal" as well
mean(adniMCI$AGE)
sd(adniMCI$AGE)
mean(adniNormal$AGE)
sd(adniNormal$AGE)

# Find the mean and standard deviation of the age for all diagnoses
mean(adni$AGE)
sd(adni$AGE)

# Make a contingency table comparing gender and diagnosis
APOE4AndGENDER <- table(adni$GENDER, adni$DX)

# add a column for the sum
addmargins(APOE4AndGENDER)

# make a table with proportions
prop.table(APOE4AndGENDER, margin = 2)

# Find the mean, standard deviation of the brain volume x10^5 mm^3 for all diagnoses
mean(adni$WholeBrainSmall)
sd(adni$WholeBrainSmall)
mean(adniAD$WholeBrainSmall)
sd(adniAD$WholeBrainSmall)
mean(adniMCI$WholeBrainSmall)
sd(adniMCI$WholeBrainSmall)
mean(adniNormal$WholeBrainSmall)
sd(adniNormal$WholeBrainSmall)

# Make a contingency table for APOE4 and DX to find the numbers and percentages
APOE4AndDX <- table(adni$APOE4F, adni$DX)

# Add margins to get the totals
addmargins(APOE4AndDX)

# make a table with row proportions
prop.table(APOE4AndDX, margin = 2)

# b.                      | Overall      |      Diagnosis Group
#                         |              | AD           | MCI          | Normal
#                         | n = 276      | n = 54       | n = 128      | n = 94
# ------------------------|--------------|--------------|--------------|--------------
#     Age                 | 73.6 +/- 7.0 | 73.9 +/- 8.0 | 72.9 +/- 7.3 | 74.3 +/- 5.8
#     Gender(male)        | 153 (55.4%)  | 30 (55.6%)   | 75 (58.6%)   | 48 (51.1%)
# Brain volume x10^5 mm^3 | 10.2 +/- 1.1 | 9.7 +/- 1.2  | 10.3 +/- 1.1 | 10.4 +/- 1.0
#     APOE4               | 
#           No copies     | 137 (100.0%) | 18 (33.3%)   | 57 (44.5%)   | 62 (66.0%)
#           One copy      | 109 (100.0%) | 25 (46.3%)   | 56 (43.8%)   | 28 (29.8%)
#           Two copies    | 30 (100.0%)  | 11 (20.4%)   | 15 (11.7%)   | 4 (4.3%)


# --------------------------------
# 7. Fill in the blanks
# --------------------------------

# The ADNI study has 276 participants. The average age is 73.6 years and 
# 55.4% are male. The Alzheimer's group has a lower average brain volume than
# the Normal group (9.7 vs 10.4 mm^3). Patients with Alzheimer's diagnosis have a
# higher prevalence of two copies of the APOE4 allele compared to normal diagnosis
# patients (20.4% vs 4.3%).