
#######################33
# Packages
library(tidyverse)
library(xlsx)
library(MASS)

#########################
# loading the data
init.data = read.xlsx("PGG_individual_data.xlsx",sheetName = "Sheet1",header = TRUE,stringsAsFactors = FALSE)
# Cleaning the data
ind_data = init.data
head(ind_data)
names(ind_data)
unique(ind_data$GroupID)

# Fixing the group ID values.
ind_data$GroupID[ind_data$GroupID == "DG036 (says DG035 on data sheet) "] = "DG036"
ind_data$GroupID[ind_data$GroupID == "DG037 (says DG036 on film) "] = "DG037"
ind_data$GroupID[ind_data$GroupID == "DG038 (says DG037 on camera) "] = "DG038"
ind_data$GroupID[ind_data$GroupID == "DG039 (says DG038 on camera) "] = "DG039"
unique(ind_data$GroupID)
## What do these labels mean?

## We're good with country
unique(ind_data$Country)

## Fix the condition variable.
table(ind_data$Condition)
ind_data$Condition[ind_data$Condition == "Full Risk "] = "Full Risk"
ind_data$Condition[ind_data$Condition == "Full Risk"] = "full"
ind_data$Condition[ind_data$Condition == "Half Risk"] = "half"
ind_data$Condition[ind_data$Condition == "No Risk"] = "no"
ind_data$Condition[ind_data$Condition == "Uncertain"] = "uncertain"

ind_data$Condition = as.factor(ind_data$Condition)
ind_data$Condition = relevel(ind_data$Condition, ref = "no")

# Fixing the age.  They have adult in the variable.
table(ind_data$age,useNA = "always")
# It's only one though.
ind_data$age[ind_data$age == "adult"] = 20
ind_data$age = as.numeric(ind_data$age)
which(is.na(ind_data$age))
# There are 12 Missing values
# We can multiple imputation pretty easily.  One entire group though is missing
# Another group is mostly missing.
# One of the groups is weird because they all donated 4 everytime.


## Working on the sex one.
table(ind_data$sex,useNA = "always")
## Missing the sex of 13, but they are generally overlapped with the age ones
which(is.na(ind_data$sex))
## The only one that isn't is observation 486


comp.data = ind_data[!is.na(ind_data$sex),]

#######################################################
# EDA
# donation.sum by age
ggplot(comp.data, aes(x = age, y = donation.sum)) + geom_jitter() +
  ggtitle("Donation Sum by Age") +
  labs(y = "Donation Amount")

# donation.sum by sex
ggplot(comp.data, aes(x = sex, y = donation.sum)) + geom_boxplot() +
  ggtitle("Donation Amount by Sex") +
  labs(y = "Donation Amount")
# donation.sum by country
ggplot(comp.data, aes(x = Country, y = donation.sum)) + geom_boxplot() +
  ggtitle("Donation Amount by Country") +
  labs(y = "Donation Amount")
# donation.sum by Rsk
ggplot(comp.data, aes(x = Condition, y = donation.sum)) + geom_boxplot() +
  ggtitle("Donation Sum by Risk condition") +
  labs(y = "Donation Amount")

########################################################
# Complete Data Analysis. There could be bias in this case
########################################################
# Basic Linear Model
base.model = lm(donation.sum ~ Country + Condition + age + sex, comp.data)
summary(base.model)
# DRC is associated with a significant decrease when compared to China
# Full Risk is associated with a 4.57 point increase on average
# Half risk is associated with a 3.0279 point increase on average
# Uncertain is associated with a 4.0045 point increase.
# All these are significant.
# Age has a positive slope of .3597 and is significant.

# Would a GEE be useful here if we're interested in the marginal association/effect.

int.model = lm(donation.sum ~ Country + Condition + age + sex + Country:Condition + Country:age + Country:sex, comp.data)
summary(int.model) 
table(comp.data$Condition,comp.data$Country)
# Are all of these uncertain ones actually half? The numbers might make sense for at least some of them?
## Let's run with that.

#################################################
# Segway.  Change the uncertain to the half
#################################################
test.data = comp.data
test.data$Condition[test.data$Condition == "uncertain"] = "half"

summary(lm(donation.sum ~ Country + Condition + age + sex, test.data))
# Changing this makes everything signifciant except he sex
summary(lm(donation.sum ~ Country + Condition + age + sex + Country:Condition + Country:age + Country:sex, test.data))
# These results are better.  Not everything is significant

#########################################################
# Poisson Model
#########################################################
p.base = glm(donation.sum ~ Country + Condition + age + sex,data = comp.data,family = poisson(link = log))
summary(p.base)


# Interaction Model
p.interaction = glm(donation.sum ~ Country + Condition + age + sex + Country:age + Country:sex + age:sex + Condition:age + Condition:sex
                      , data = comp.data, family = poisson(link = log))
summary(p.interaction)
#########################################################
# Negative Binomial
#########################################################
nb.base = glm.nb(donation.sum ~ Country + Condition + age + sex,data = comp.data)
summary(n.base)

nb.interaction = glm.nb(donation.sum ~ Country + Condition + age + sex + Country:age + Country:sex + age:sex + Condition:age + Condition:sex)
summary(nb.interaction)

########################################################
# Bayesian Estimation
########################################################
# 
# 

########################################################
# The plots and everything?
########################################################
# To assess the plots