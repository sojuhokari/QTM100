################################
# Author: Soju Hokari
# Document Name: Lab 2 Manual -- SUMMARIZING AND VISUALIZING DATA
# Date: 9 September 2022
################################

# --------------------------------
# MARK: Set-up
# --------------------------------

# The working directory. Change this variable when running on a different computer.
workingdir <- "~/code/QTM100"

# Set the working directory
setwd(workingdir)

# Import dataset
babies <- read.table("datasets/babies.txt", header=T)

# --------------------------------
# MARK: Review
# --------------------------------

# Find the data set structure (types of variables, names of variables)
str(babies)

# View a summary of the data set
summary(babies)

# Recode `parity` and `smoke` as categorical variables --- called FACTORING
babies$parityF <- factor(babies$parity, labels = c("first born", "otherwise"))
babies$smokeF <- factor(babies$smoke, labels = c("not now", "yes now"))

# Now let's try the structure command again
str(babies)
# smokeF and parityF are now of type Factor, with two levels!

# try the summary again
summary(babies)
# parityF and smokeF now have useful data!

# "NA's" in the summary tables means that there was no data provided there

# More summarizing
summary(babies$bwt)

# Taking the mean only
mean(babies$bwt)

# --------------------------------
# MARK: NEW EXPERIMENTS with NUMERIC data
# --------------------------------

# Getting the STANDARD DEVIATION
sd(babies$bwt)

# USEFUL SINGLE NUMERIC SUMMARIES:
# min
# max
# median
# range
# IQR
# mean
# sd
IQR(babies$bwt)

# TAPPLY TO COMPARE VARIABLES
# Comparing average birth rate among smoking and non-smoking mothers
tapply(X = babies$bwt, INDEX = babies$smokeF, FUN = sd)
# First argument is *numeric variable*, second argument is *categorical variable*
# Third argument is the function you want to calculate -- in this case, standard deviation

# Calculate the mean of bwt in groups of parityF:
tapply(X = babies$bwt, INDEX = babies$parityF, FUN = mean)

# We can also use variables of type `int` as the INDEX (the categories)!
tapply(X = babies$bwt, INDEX = babies$parity, FUN = mean)

# --------------------------------
# MARK: NEW EXPERIMENTS with VISUALIZING NUMERIC data
# --------------------------------

# histograms
hist(babies$bwt)

# boxplots
boxplot(babies$bwt)

# Adding arguments to change colors or axis labels:
?hist
hist(
  babies$bwt,
  col = "#f97e7d",
  border = "#843b62",
  main = "Birthweights of babies",
  xlab = "birthweight (ounces)",
  ylab = "number",
)
# Or even color the axis itself:
Axis(
  babies$bwt,
  col = "#f97e7d",
  side = 2
)

# side-by-side boxplot:
boxplot(babies$bwt ~ babies$smokeF)
# Tilde means to break down the birth weight by smoking status
# Very confusing syntax you've got going on here, R

# scatterplot!
plot(babies$gestation, babies$bwt)
# x axis, y axis

# --------------------------------
# MARK: NEW EXPERIMENTS with CATEGORICAL variables
# --------------------------------

# summarize the frequency of occurence of each categorical variable level
table(babies$smokeF)

# Seeing the sum of observations by adding margins!
smk.tab <- table(babies$smokeF)
smk.tab
addmargins(smk.tab)

# proportions of observations in each group
prop.table(smk.tab)

# We can even pass these functions to each other apparently!
# (Maybe there is a problem with this but I don't see it yet :)
addmargins(prop.table(smk.tab))

# Seeing the relationship between two categorical variables in a contingency table!
table(babies$smokeF, babies$parityF)

# add margins and proportions
smk.par.tab <- table(babies$smokeF, babies$parityF)
addmargins(smk.par.tab)
prop.table(smk.par.tab)
# But... prop.table isn't helpful because it gives overall proportions

# Calculate row or column proportions!
prop.table(smk.par.tab, margin = 1) # row proportions!
prop.table(smk.par.tab, margin = 2) # Column proportions!

# --------------------------------
# MARK: NEW EXPERIMENTS with VISUALIZING CATEGORICAL variables
# --------------------------------

# barplot
barplot(smk.tab)

# side-by-side bar plots
barplot(smk.par.tab, beside = T, legend.text = T)

# barplot with PROPORTIONS
barplot(prop.table(smk.par.tab, margin=2), beside = F, legend.text = T)

# YAY DONE!
