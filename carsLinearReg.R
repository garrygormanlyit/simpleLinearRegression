# Cars linear regression example

scatter.smooth(x = cars$dist, 
               y = cars$speed, 
               main = "Distance ~ Speed",
               xlab = "Stopping distance",
               ylab = "Car speed")

# This is a high correlation value and suggests 
# a high positive correlation between both variables.
cor(cars$speed, cars$dist)

# Check for outliers in variables
# Generally, any data point that lies outside the 
# 1.5 * interquartile-range (1.5 * IQR) is considered 
# an outlier, Where:

# IQR is calculated as the distance between the 
# 25th percentile and 75th percentile values for that 
# variable.

# We’ll need to check both distance and speed.

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # divide graph area in 2 columns
attach(cars)
boxplot(speed, 
        main = "Speed", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(speed)$out)) # box plot for 'speed'

boxplot(dist, 
        main = "Distance", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(dist)$out)) # box plot for 'distance'

detach(cars)
par <- opar

# The boxplots suggest that there is 1 outlier 
# in the data in the distance variable where the 
# speed is 120.

nrow(cars)

# Remove the outlier row
cars <- subset(cars, cars$dist!=120)

# Check that outlier row has been removed
nrow(cars)

# Rerun the boxplot to verify that outliers have been removed
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # divide graph area in 2 columns
attach(cars)
boxplot(speed, 
        main = "Speed", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(speed)$out)) # box plot for 'speed'

boxplot(dist, 
        main = "Distance", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(dist)$out)) # box plot for 'distance'

detach(cars)
par <- opar

# check normality with histogram and q-q plot
# skewness function from e1071 library
library(e1071)

par(mfrow = c(1,2))
# density plot for speed
plot(density(cars$speed), main = "density plot: Speed",
     xlab = "Frequency",
     ylab = "Speed",
     sub = paste("Skewness: ", round(e1071::skewness(cars$speed), 2)))

# fill the area under the plot with a colour
# in this example it is red
polygon(density(cars$speed), col = "red")

# skewness that is < -1 or > 1 is highly skewed
# -1 to 0.5 and 0.5 to 1 = moderately skewed
# -0.5 to 0.5 = aprox symetry

plot(density(cars$dist), main = "density plot: Speed",
     xlab = "Frequency",
     ylab = "Distance",
     sub = paste("Skewness: ", round(e1071::skewness(cars$dist), 2)))

polygon(density(cars$dist), col = "red")

hist(cars$dist, main = "Normality proportion of distance",
     xlab = "Distance",
     col = "blue")

qqnorm(cars$dist)
qqline(cars$dist)

# you would do shapiro-wilks and smirnof now to confirm normality

# split data into training and testing
# meant to be done randomly
# to prove the effectiveness of a model we remove this randomness
# so that the results can be reproduced by another researcher
# set the initial random variability seed
set.seed(1) # setting randomness pattern

# split data into 70% training, 30% testing
no_rows_data <- nrow(cars)
sample_data <- sample(1:no_rows_data, size = round(0.7*no_rows_data), replace = FALSE)

# create dataframes
training_data <- cars[sample_data,]
testing_data <- cars[-sample_data,]

nrow(training_data)
nrow(testing_data)

linear_model <- lm(dist~speed, data = training_data)
summary(linear_model)
# p=3.92*10**-10
# this is statistically significant as p<0.05 as number of stars is 3

# to compare models use this method. The lower number is the better model
AIC(linear_model)
BIC(linear_model)

# predict values using the model
# distance will be predicted and compared to
# test

predicted_distance <- predict(linear_model, testing_data)
actuals_predictions <- data.frame(cbind(Actuals = testing_data$dist,
                                       Predicted = predicted_distance))

actuals_predictions

# test the accuracy of the model
correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy
# 0.47 = 47% accurate - less than flipping a coin
# plot these to demonstrate
# explain why it went wrong

# MAPE represents the mean forecast by which
# the results will produce an error
mape <- mean(abs(actuals_predictions$Predicted - actuals_predictions$Actuals)/
               actuals_predictions$Actuals)
mape
# 0.3295 = off on average by 33%. Lower is better. 33% is rubbish
# < 0.1 is excellent
# < 0.2 is good


# cross-validation for linear regration
install.packages("DAAG")
library(DAAG)
par(mfrow = c(1,1))
cv_results <- suppressWarnings(CVlm(data = cars, 
                                    form.lm = dist ~ speed, 
                                    dots = FALSE,
                                    seed = 1,
                                    printit = FALSE,
                                    legend.pos = "topleft",
                                    main = "Small symbols are predicted values while bigger ones are actuals"))

# predict a single value from model
predicted_data <- data.frame(speed = c(10))

predicted_distance <- predict(linear_model, predicted_data)
predicted_distance

