# Insurance dataset 
# Load the dataset into a data frame first

insurance_data <- read.csv("insurance.csv", na = "")
str(insurance_data)

# several variables need to be converted
# sex - male = 0, female = 1
# Smoker - yes = 1, no = 0
# Region contains 4 categories
# N = 4, so we need n-1 indicator variables
# = 3 indicator variables
# Code variables in alphabetical order
head(insurance_data$region, 15)

# Convert variables as described above
names(insurance_data)
attach(insurance_data)
# create dummy variables
insurance_data$sex <- factor(sex,
                             levels = c("male", "female"), 
                             ordered = FALSE)

insurance_data$smoker <- factor(smoker, 
                                levels = c("yes", "no"), 
                                ordered = FALSE)

insurance_data$region <- factor(region,  
                                levels = c("northeast", "northwest", "southeast", "southwest"), 
                                ordered = FALSE)

str(insurance_data)

# View the split of males and females within the data frame
table(insurance_data$sex)
table(insurance_data$smoker)
table(insurance_data$region)

pairs(insurance_data)
# Initial investigation of data variables
install.packages("psych")
library(psych)

# Seems there could be a positive correlation between 
# smoker and charges, perhaps charges and age
# and BMI and charges
pairs.panels(insurance_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals
# describe what the pairs.panal shows you, what it doesnt show you and why.
# if we build the model now, R will automatically split the
# factor variables
# Alternatively we will control this process

# in linear regresison, mode lis:
# y = b0 + B1x1 + B2x2 + B3x3 + e
# where y = insurance charges
# x1 = age of the person
# x2  sex of the person
# x3 = bmi of the person
# x4 = children
# x5 = smoker
# x6 = region
# It is clear that x1, and x3 are continuous and x2, x4, x5, x6 are categorical
# therefore we need to create dummy variables for the categorical
# variables
# For the sex variable x2
# x2 = 1 if person is male
# x2 = 0 if person is female

# r will convert categorical data into dummy variables automitically (n-1)
set.seed(1)
model <- lm(formula = charges ~ age + sex + bmi + 
              children + smoker + region, data = insurance_data)
summary(model)
# compare models using adjusted R-squared
# remove variables that do not impact the model
# age, bmi, children, smokerno have an
# influence over the dependant variable "charges"
# p-value is less than 0.05 therefore not significant
# sexfemale is not influencial on the model
# keep region data for now as I need to use it to answer
#the research question

insurance_data <- insurance_data[c(1, 3:7)]
insurance_data$bmi <- round(insurance_data$bmi, 1)
insurance_data$charges <- round(insurance_data$charges, 2)

#create the model again with the ammended changes
model <- lm(formula = charges ~ age + bmi + children
            + smoker + region, data = insurance_data)
summary(model)
# lookat the p-values and discuss which variables will influence the model. stars and p-value
# this justifies dropping variables from model
# now we need to check assumptions


# linearity

scatter.smooth(x = age,
               y = charges,
               main = "Insurance charges ~ age",
               ylab = "Insurance charges (,000)",
               xlab = "Ages (years)")

scatter.smooth(x = bmi,
               y = charges,
               main = "Insurance charges ~ bmi",
               ylab = "Insurance charges (,000)",
               xlab = "Ages (years)")

scatter.smooth(x = children,
               y = charges,
               main = "Insurance charges ~ children",
               ylab = "Insurance charges (,000)",
               xlab = "Ages (years)")

# no information available for plot of continuous vs categorical
scatter.smooth(x = smoker,
               y = charges,
               main = "Insurance charges ~ smoker",
               xlab = "Insurance charges (,000)",
               ylab = "smoker")

# instead we can examin relationship between dependant and

plot(smoker, charges, main = "charges by smoker ststus",
     xlab = "Smoker",
     ylab = "Insurance charges")
# the black line in the middle of a boxplot is the median value
# this plot shows that the median value for non-smokers is lower
# than the min value for smokers
# smoking is having a greater influence on charges than non-smoking

plot(region, charges, main = "charges by region",
     xlab = "Region",
     ylab = "Insurance charges")
# median is the same for all regions
# southeast has a wider range
# not likely to have an influence on charges as plots for each region are similar


# only use this method if you are sure about the underlying nature of the variable you are correlation
# probably better to use boxplot
# creates a new variable where smoker "yes" = 1 and enything else = 0
insurance_data$cor_smoker <- ifelse(smoker == "yes", 1, 0)
cor(charges, cor_smoker)

# normality

# with command allows us to combine qqnorm and qqline ein the same chart
with(insurance_data, {
  qqnorm(age, 
         main = "Normality analysis of age data")
  qqline(age)
  })
# could indicate that the data is not normally distributed as extremes are not on the mean line
# could indicate that the data is normally distributed as most pts are on mean line

# repeat for other continuous data

# normality of categorical data
with(insurance_data, {
  qqnorm(charges[smoker == "yes"], 
         main = "Normality analysis of smoker = yes data")
  qqline(charges[smoker == "yes"])
  })

with(insurance_data, {
  qqnorm(charges[smoker == "no"], 
         main = "Normality analysis of smoker = no data")
  qqline(charges[smoker == "no"])
})

# regions
with(insurance_data, {
  qqnorm(charges[region == "southwest"], 
         main = "Normality analysis of region = southwest data")
  qqline(charges[region == "southwest"])
})

with(insurance_data, {
  qqnorm(charges[region == "southeast"], 
         main = "Normality analysis of region = southeast data")
  qqline(charges[region == "southeast"])
})

with(insurance_data, {
  qqnorm(charges[region == "northwest"], 
         main = "Normality analysis of region = nortwest data")
  qqline(charges[region == "northwest"])
})

with(insurance_data, {
  qqnorm(charges[region == "northeast"], 
         main = "Normality analysis of region = northeast data")
  qqline(charges[region == "northeast"])
})

# same as above
qqnorm(age)
qqline(age, col = "red")

# check for normality using r methods
normality_test <- shapiro.test(insurance_data$age)
normality_test$p.value

normality_test <- shapiro.test(charges[smoker == "yes"])
normality_test$p.value

normality_test <- shapiro.test(charges[smoker == "no"])
normality_test$p.value

normality_test <- shapiro.test(charges[region == "southwest"])
normality_test$p.value

normality_test <- shapiro.test(charges[region == "southeast"])
normality_test$p.value

normality_test <- shapiro.test(charges[region == "northwest"])
normality_test$p.value

normality_test <- shapiro.test(charges[region == "northeast"])
normality_test$p.value

# We can check the normality in each variable
# using the tapply() function instead
# We cannot use the test for dichotomous data
# so we refer to the histogram instead
with(insurance_data, tapply(charges, smoker, shapiro.test))


# measure colinearity which is the relationship between multiple variables
# the tolerance is an indication of the % variance in the predictor that cannot 
# be accounted for by the other predictors
# hence very small values indicate a predictor is redundant

# VIF score should be close to 1 but under 5
# 10+ indicates that the variable is not needed
# and can be removed from the model

library(car)
vif(model)
