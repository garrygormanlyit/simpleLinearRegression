murderData <- as.data.frame.matrix(state.x77)
class(murderData)

colnames(murderData)[4] <- "Life_exp"
colnames(murderData)[6] <- "HS_grade"
# alternative method
# colnames(murderData)[colnames(murderData) == "Life Exp"] <- "Life_exp"
# colnames(murderData)[colnames(murderData) == "HS grade"] <- "HS_grade"

# move state name into a new variable
murderData$name <- state.name

install.packages("psych")
library(psych)

# correlation plots
pairs.panels(murderData,
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
             ci = TRUE)   

# scatter plots
comparisonData <- as.data.frame(state.x77)
pairs(comparisonData)
