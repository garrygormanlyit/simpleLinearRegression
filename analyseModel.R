# simple Linear regression
str(women)

# aiming to predict weight from height
#dependant variable = weight
# independant variable = height

# building the model
simple_linear_model <- lm(weight ~  height, data = women)
simple_linear_model

# creates a summary of the model
summary(simple_linear_model)

# weight = -87.52 + 3.45 x height

# ploting simple lm onto data used to create model
plot(women$height, women$weight,
     xlab = "Height in inches",
     ylab = "Weigth in lbs",
     main = "Scatter plot showing the regression line for weight pridicted from height")
abline(simple_linear_model)
# looks like the data fits the model

# analyse the correlationcoefficient
# measures the levelof association between 2 variables
# -1 perfect negative correlation
# +1 perfect positive correlation

# a value of -0.2 < x < -0.2 suggests that much of the 
# variation in outcome variable is not explained by the predictor
# # then we should look for better predictor variables
confint((simple_linear_model))


cor(women$height, women$weight) # 0.995. Very strong positive correlation between variable

# model accuracy- goodness of fit
# 3 quantities
# residual standard error (RSE)
# r-squared (r2)
# F-statistic

# evaluate the model for comparison purposes. Then pick the best one
summary(simple_linear_model)
# RSE = 1.525 = prediction error rate
# when comparing 2 models, smallest RSE is best
# in this model, observed weight values deviate from the true
# regression by 1.525 unites(1.525 lbs) on average 
# (the gap between the point and the prediction line)

# r2
# ranges from 0 - 1
# we use the adjusted r2
# high r2 = good indicator that the model variability in the outcome can be explained
# by the model
# an r2 close to 0 = model does not explain much of the variability

# f-statistic
# overall significance of the model. 
# a large f-statistic corresponds to a significant p-value
# (p<0.05) #. high f and low p means good overall significance of the model

# predict() feed in a dataframe with a column named height