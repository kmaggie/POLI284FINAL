
#####################
# Group 9 Final Project
#
# By: Sarah George, Maggie Kennedy, & Nate Marsh
#
# 12/09/23

rm(list = ls())
setwd("/Users/maggie/Downloads/")
anes <- read.csv("anes_FTF_2016.csv")
library(ggplot2)
library(dplyr)
library(stargazer)


#The following command takes the ANES dataset and reduces it to only the variables we are interested in
anes <- anes %>% filter((V162034a <=2 & V162034a > 0 )) %>% 
  select(V162034a,V161002, V161270, V161267, V161244, V161140)

#III: Prep Work:The Dependent Variable
#The following command takes the dependent variable and codes it to be binary
anes$V162034a[anes$V162034a == 1] <- 0 #makes 0 clinton
anes$V162034a[anes$V162034a == 2] <- 1 # makes 1 trump
#the following line of code makes an object called trumporhilary, represented by the voting outcome variable, so we can make a figure.
trumporhilary <- anes$V162034a
#the following command creates a figure showing the distribution of responses for the dependent variable, voting outcome.
dependentdistribution <- ggplot(anes, aes(x=trumporhilary)) + geom_bar()
print(dependentdistribution)

#IV: Prep Work: the Independent variable
#The following command takes our gender column and makes it into an object of its own.
gender <- anes$V161002
#the following commands create a bar graph for gender with 1 being male and 2 being female
independentdistribution <- ggplot(anes, aes(x=gender)) + geom_bar()
print(independentdistribution)
#Based on our figure, we simplified the gender category into a 1 or 0 choice instead of 2 or 1.
anes$V161002[anes$V161002 == 1] <- 0 #makes 0 male
anes$V161002[anes$V161002 == 2] <- 1#makes 1 female


#V: Initial Analysis: Data Visualization
# Extract levels of the factor variable
gender_levels <- levels(anes$V161002)

# Specify colors in the same order as levels
gender_colors <- c("Male" = "blue", "Female" = "pink")

# Calculating average gender split for each candidate
gender_avg <- anes %>%
  group_by(V162034a, V161002) %>%
  summarise(count = n()) %>%
  group_by(V162034a) %>%
  mutate(percentage = count / sum(count) * 100)

# Creating a bar graph
ggplot(gender_avg, aes(x = V162034a, y = percentage, fill = factor(V161002))) +
  geom_bar(position = "stack", color = "black", stat = "identity") +
  ggtitle("Percentage of Presidential Votes by Gender") +
  xlab("Presidential Candidate (Clinton v Trump)") +
  ylab("Percentage (male(0) v. female(1))") +
  theme_minimal()




#VI: Initial Regression Analysis
mod1 <- lm(V162034a ~ V161002, data=anes) #creates linear model of V162034a, candidate, on V161002, gender
summary <- summary(mod1) #assign mod1 regression summary to variable 'summary'

coefficients <- coef(mod1) #extract coefficients
standard_errors <- summary$coefficients[, "Std. Error"] #extract standard errors
r_squared <- summary(mod1)$r.squared #extract multiple R-squared
observations <- count(anes) #counts the number of observations

#create the table
regression_table <- data.frame(
  Coefficient = coefficients,
  `Std. Error` = standard_errors,
  R_Squared = r_squared,
  N_Observation = observations
)

regression_table #print regression table



#VIII: Prep Work Confounders
#A)

#Cleaning the Religion Variable (V161244). We elected to make this a binary variable based on whether a person was o wasn't religious.
#This code makes the "Don't Know" and "Refused to Answer" responses missing.

anes$V161244[anes$V161244 < 0]<- NA
#This portion of code makes the "No" values equal to  and the "Yes" values equal to 1.
anes$religion <- NA
anes$religion[anes$V161244 ==2] <- 0
anes$religion[anes$V161244 == 1]<- 1
anes$V161244 <- anes$religion
religion <-anes$V161244

#Cleaning the Age Variable (V161267). We elected to make this a continuous variable. 
#This code makes the "Refused to Answer" responses NA.
anes$V161267[anes$V161267 < 0] <- NA
age <- anes$V161267

#Cleaning and Re-coding the Education Variable (V161270). We elected to make this a categorical variable. 
#This code makes the "Other  responses NA.
#The second portion of the code cleans the existing Education variable to have only 4 possible responses
anes$V161270[anes$V161270 == 95] <- NA
anes$education <- NA
anes$education[anes$V161270 >= 1 & anes$V161270 <= 9] <- 1
anes$education[anes$V161270 == 10] <- 2
anes$education[anes$V161270 >= 11 & anes$V161270 <= 13] <- 3
anes$education[anes$V161270 >= 14 & anes$V161270 <= 16] <- 4
anes$V161270 <- anes$education
education <- anes$V161270

#Cleaning and Recoding the Economy Variable (V161140). We elected to make this a categorical variable. 
#This code makes the "Refused to Answer" and "Don't Know" responses NA.
anes$V161140[anes$V161140 < 0] <- NA
#The following code recodes the economy variable so that negative views are -1, positive views are 1, and neutral views are 0.
anes$economy <- NA
anes$economy[anes$V161140 == 1] <- 1
anes$economy[anes$V161140 == 2] <- 0
anes$economy[anes$V161140 == 3] <- -1 
anes$V161140 <- anes$economy
economy <- anes$V161140

#c)
#The following code creates a table of summary statistics for each of the confounding variables using the psych package.

describe(anes[,c( "V161244", "V161267", "V161270", "V161140")], fast = TRUE)


#D)

#The following code creates a data frame in order to make a table of the 
# correlation between the IV and the confounders,. and the DV and the confounders
# Calculating correlations between independent variable and other variables
cor_religion_IV <- cor(anes$V161002, anes$V161244, use = "complete.obs")
cor_age_IV <- cor(anes$V161002, anes$V161267, use = "complete.obs")
cor_education_IV <- cor(anes$V161002, anes$V161270, use = "complete.obs")
cor_economy_IV <- cor(anes$V161002, anes$V161140, use = "complete.obs")
cor_IV <- c(cor_religion_IV, cor_age_IV, cor_education_IV, cor_economy_IV)
# Calculating correlations between dependent variable and other variables
cor_religion_DV <- cor(anes$V162034a, anes$V161244, use = "complete.obs")
cor_age_DV <- cor(anes$V162034a, anes$V161267, use = "complete.obs")
cor_education_DV <- cor(anes$V162034a, anes$V161270, use = "complete.obs")
cor_economy_DV <- cor(anes$V162034a, anes$V161140, use = "complete.obs")
cor_DV <- c(cor_religion_DV, cor_age_DV, cor_education_DV, cor_economy_DV)
# Creating a table with correlations
correlation_table <- cbind(Indpendent_Correlations = cor_IV,
                           Dependent_Correlations = cor_DV)
row.names(correlation_table) <- c("Religion", "Age", "Education", "Economy")

# Print the correlation table
print(correlation_table)


#IX: Multivariate Regression

#The following Section of code makes a 3 column table showing the results of the each of our regressions
#Bivariate regression
originalregression <- lm(V162034a ~ V161002, data = anes)
#Addition of the economy variable
regression2 <- lm(V162034a ~ V161002 + V161140, data = anes)
#addition of religion, age, education, and (again) economy
multiregression <- lm(V162034a ~ V161002 + V161244 + V161267 + V161270 + V161140, data=anes)

stargazer(type = "text", originalregression, regression2, multiregression, title = "Multivariate Results", align = TRUE, out = "POLI281Multivariate.txt")
