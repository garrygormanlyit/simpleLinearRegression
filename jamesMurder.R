# USA murder rate dataset
states <- as.data.frame(state.x77)

colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"

variables_of_interest <- c("Murder",
                           "Population",
                           "HS_Grad",
                           "Illiteracy",
                           "Income",
                           "Life_Exp",
                           "Area",
                           "Frost")
pairs(states[variables_of_interest])

scatter.smooth(x = states$Population,
               y = states$Murder,
               xlab = "Population (,000)",
               ylab = "Murder %", 
               main = "Correlation of murder ~ population")

cor(states$Murder, states$Population)

scatter.smooth(x = states$Frost,
               y = states$Murder,
               main = "Correlation of Murder ~ Frost",
               xlab = "Frost",
               ylab = "Murder %")

cor(states$Murder, states$Frost)

paste("Correlation for Murder and Frost: ", cor(states$Murder, states$Frost))
paste("Correlation for Murder and Illiteracy: ", cor(states$Murder, states$Illiteracy))
paste("Correlation for Murder and Population: ", cor(states$Murder, states$Population))
paste("Correlation for Murder and HS Grad: ", cor(states$Murder, states$HS_Grad))
paste("Correlation for Murder and Income: ", cor(states$Murder, states$Income))
paste("Correlation for Murder and Life Exp: ", cor(states$Murder, states$Life_Exp))
paste("Correlation for Murder and Area: ", cor(states$Murder, states$Area))

opar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(states)
boxplot(Murder,
        main = "Murder",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Murder)$out)) # box plot for 'murder'
boxplot(Population,
        main = "Population",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Population)$out)) # box plot for 'Population'
boxplot(states$HS_Grad,
        main = "Graduation",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$HS_Grad)$out)) # box plot for 'HS Grad'
boxplot(Illiteracy,
        main = "Illiteracy",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Illiteracy)$out)) # box plot for 'HS Grad'
boxplot(Income,
        main = "Income",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Income)$out)) # box plot for 'HS Grad'
boxplot(Frost,
        main = "Frost",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Frost)$out)) # box plot for 'HS Grad'
boxplot(states$Life_Exp,
        main = "Life Exp",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$Life_Exp)$out)) # box plot for 'HS Grad'
detach(states)
par(opar)

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Population)$out # outlier values.
paste("Population outliers: ", paste(outlier_values, collapse=", "))

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Income)$out # outlier values.
paste("Income outliers: ", paste(outlier_values, collapse=", "))

# Remove population outliers
states <- subset(states,
                 states$Population != 21198
                 & states$Population != 11197
                 & states$Population != 18076
                 & states$Population != 11860
                 & states$Population != 12237)
# Remove Income outliers
states <- subset(states, states$Income != 6315)

# Skewness function to examine normality
#install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(4,2)) # divide graph area into 1 row x 2 cols
# density plot for speed
# minimally skewed to the left
# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical

plot(density(states$Population),
     main = "Density plot : Population",
     ylab = "Frequency", xlab = "Population",
     sub = paste("Skewness : ", round(e1071::skewness(states$Population), 2)))

# fill the area under the plot

polygon(density(states$Population), col = "red")

plot(density(states$Murder),
     main = "Density plot : Murder",
     ylab = "Frequency", xlab = "Murder",
     sub = paste("Skewness : ", round(e1071::skewness(states$Murder), 2)))
polygon(density(states$Murder), col = "red")

plot(density(states$HS_Grad),
     main = "Density plot : HS grade",
     ylab = "Frequency", xlab = "HS grade",
     sub = paste("Skewness : ", round(e1071::skewness(states$HS_Grad), 2)))
# fill the area under the plot
polygon(density(states$HS_Grad), col = "red")

plot(density(states$Illiteracy),
     main = "Density plot : Illiteracy",
     ylab = "Frequency", xlab = "Illiteracy",
     sub = paste("Skewness : ", round(e1071::skewness(states$Illiteracy), 2)))
polygon(density(states$Illiteracy), col = "red")

plot(density(states$Income),
     main = "Density plot : Income",
     ylab = "Frequency", xlab = "Income",
     sub = paste("Skewness : ", round(e1071::skewness(states$Income), 2)))
# fill the area under the plot
polygon(density(states$Income), col = "red")

plot(density(states$Frost),
     main = "Density plot : Frost",
     ylab = "Frequency", xlab = "Feost",
     sub = paste("Skewness : ", round(e1071::skewness(states$Frost), 2)))
# fill the area under the plot
polygon(density(states$Frost), col = "red")
par <- opar

opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))

hist(states$Murder, main = "Normality proportion for murder", 
     xlab = "murder rate")
qqnorm(states$Murder)
qqline(states$Murder)
# run normality test
# make decisions on whether or not to keep vars

attach(states)
murder_model <- lm(Murder ~ Illiteracy + 
                     Population +
                     HS_Grad +
                     Income +
                     Frost)
summary(murder_model)

par <- opar

# probability plot - if all points fall close to line
# we have met normality assumption fairly well
library(car)
qqPlot(murder_model, labels = row.names(states), id.method = "identify", 
       simulate = TRUE,
       main = "Q-Q plot")
# 18 and 25 are causing a problem with the model. 
# they are outliers within the model

# split dataset into training and testing data
# 70 30 splitb
set.seed(1)
no_rows_data <- nrow(states)
sample_data <- sample(1:no_rows_data,
                      size = round(0.7 * no_rows_data),
                      replace = FALSE)

training_data <- states[sample_data,]
testing_data <- states[-sample_data,]

attach(states)
murder_model <- lm(Murder ~ Illiteracy + 
                     Population +
                     HS_Grad +
                     Income +
                     Frost,
                   data = training_data)
summary(murder_model)

qqPlot(murder_model, labels = row.names(states), id.method = "identify", 
       simulate = TRUE,
       main = "Q-Q plot")

# examine the outliers to investigate the issues
training_data["Nevada",]
training_data["Rhode Island",]
fitted(murder_model)["Nevada"]
fitted(murder_model)["Rhode Island"]

student_murder_model <- rstudent(murder_model)
hist(student_murder_model, breaks = 10, freq = FALSE,
     xlab = "Studentised residual",
     main = "Distribution of errors")

curve(dnorm(x, mean = mean(student_murder_model),
            sd = sd(student_murder_model)),
      add = TRUE,
      col = "blue",
      lwd = 2)

lines(density(student_murder_model)$x,
      density(student_murder_model)$y,
      col = "red",
      lty = 2,
      lwd = 2)
# outlier test of the murder model
outlierTest(murder_model)

# remove nevada from the dataset
states <- subset(states, states$)