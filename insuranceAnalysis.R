insurance_data <- read.csv("insurance.csv", na = "")
str(insurance_data)

# several variables need to be converted
# sex - male = 0, female = 1
# smoker - yes = 1, no = 0
# region - 4 regions

# convert the sex variables
insurance_data$sex <- factor(insurance_data$sex, levels = c("male", "female"), ordered = FALSE)
str(insurance_data)

# convert smoker variable
insurance_data$smoker <- factor(insurance_data$smoker, levels = c("yes", "no"), ordered = FALSE)
str(insurance_data)

# convert region variable
insurance_data$region <- factor(insurance_data$region, 
                                levels = c("southwest", "southeast", "northwest", "northeast"), 
                                ordered = FALSE) # dont need levels if order is False
str(insurance_data)

# examine correlation between the variables
install.packages("psych")
library(psych)

# correlation plots
pairs.panels(insurance_data,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# if we ask r to build the modelat this point, 
# rwill automatically split the factor variables
# a;ternatively we want to control the model
attach(insurance_data)
set.seed(1)
# the thing we are trying to predict goes on the left of the ~
# we dont have full control when we create our mode this way. 
# it does n-1 and removes one of your categories
# dont build your model this way
model <- lm(formula = charges ~ age + sex + bmi + smoker + region,
            data = insurance_data)
summary(model)

# instead we control the variables
# we will create our own new variables
# if sex contains male insert 1 otherwise insert 0
insurance_data$male <- ifelse(insurance_data$sex == "male", 1, 0)
insurance_data$female <- ifelse(insurance_data$sex == "female", 1, 0)


insurance_data$smokes <- ifelse(insurance_data$smoker == "yes", 1, 0)
insurance_data$nonsmoker <- ifelse(insurance_data$smoker == "no", 1, 0)

# region
insurance_data$sw <- ifelse(insurance_data$region == "southwest", 1, 0)
insurance_data$se <- ifelse(insurance_data$region == "southeast", 1, 0)
insurance_data$nw <- ifelse(insurance_data$region == "northwest", 1, 0)
insurance_data$ne <- ifelse(insurance_data$region == "northeast", 1, 0)

names(insurance_data)
# drop unneeded variables
# using age, male, female, bmi, smokes
# nonsmoker, ne, nw, se, sw
insurance_data <- insurance_data[c(1, 3, 8:15)]

# round BMI and charges
insurance_data$bmi <- round(insurance_data$bmi, 1)
insurance_data$charges <- round(insurance_data$charges, 1)

# plot this to see if there is a correlation
# x = the thing you are trying to predict
scatter.smooth(x=charges,
               y = age,
                main = "Insurance charges ~ age",
                xlab = "Insurances charges",
                ylab = "Age (years)")

scatter.smooth(x=charges,
               y = bmi,
               main = "Insurance charges ~ BMI",
               xlab = "Insurances charges",
               ylab = "Age (years)")

scatter.smooth(x=charges,
               y = male,
               main = "Insurance charges ~ male",
               xlab = "Insurances charges",
               ylab = "Age (years)")

scatter.smooth(x=charges,
               y = female,
               main = "Insurance charges ~ female",
               xlab = "Insurances charges",
               ylab = "Age (years)")

scatter.smooth(x=charges,
               y = insurance_data$smokes,
               main = "Insurance charges ~ smokes",
               xlab = "Insurances charges",
               ylab = "Age (years)")

# no info available for plot of continuous variable
# against categorical variable

# test for correlation
# anything below 0.3 is week
cor (charges, male)
cor(charges, female)
cor(charges, bmi)
cor(charges, age)
cor(charges, children)

paste("correlation for charges and age: ", cor(charges, age))

# need to check also for outliers, normality, colinearity

# Normality
with(insurance_data, qqplot(charges,
                            male,
                            main = "Comparing 2 samples of activity data",
                            xlab = "Charges",
                            ylab = "Males"))
abline(0,1)

# measure of colinearity
# this is a measure of relationship between variables

# check colinearity of the model
model <- lm(formula = charges ~ age + bmi + male + female + ne + sw + se + nw)# builds the model to predict charges

summary(model)

install.packages("car")
library(car)

qqplot(model, labels = row.names(insurance_data), id.nethod = "identify",
       simulate = TRUE)
