# build a model to predict stopping distance from speed
# using the cars dataset

# 1st step - check the model assumptions
# these are the core assumptions
# linearity among the variables
# normality - normal distribution -  histogram
# no collinearity - vars are not a linear combination of others
# independence - residuals are independant and not coorelated.

# check linearity first using scatter plot
# x-axis = independant variable
# y-axis = dependant var
# if relationship existsthen linearity is validated

scatter.smooth(x = cars$speed, y = cars$dist,
               main = "Distance ~ speed",
               xlab = "Car speed (mph)",
               ylab = "Stopping distance")

cor(cars$speed, cars$dist)
# we look for a value outside of between -0.2 and 0.2
# cor = 0.81 - good positive correlation

# check for outliers
# an outlier = 1.5 x interquartile range
# IQR = dist between 25th and 75th percentage
# we need to check speed and dist for outliers

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
attach(cars)
boxplot(speed,
        main = "Speed", sub = paste("Outlier rows: ",
                                   boxplot.stats(speed)$out))
boxplot(dist,
        main = "Speed", sub = paste("Outlier rows: ",
                                   boxplot.stats(dist)$out))

detach(cars)
par <- opar

# remove the outlier at line 120
cars <- subset(cars, cars$dist != 120)

# plot again without outlier
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
attach(cars)
boxplot(speed,
        main = "Speed", sub = paste("Outlier rows: ",
                                    boxplot.stats(speed)$out))
boxplot(dist,
        main = "Speed", sub = paste("Outlier rows: ",
                                    boxplot.stats(dist)$out))

detach(cars)
par <- opar
