# Set working directory
setwd("/home/kalil/Documents/Graduacao/FGV/ME")

# Load data
data <- read.csv("seven_wonders_duel.csv")

# Fit logistic regression model
model <- glm(Sucesso ~ Ciência * Exército, data = data, family = binomial)
summary(model)

# Plotting interaction between Cívico and Ciência colored by Sucesso
plot(data$Cívico, data$Ciência, col = data$Sucesso + 1,
     xlab = "Cívico", ylab = "Ciência", main = "Cívico vs Ciência")

# Get column names (assuming there are more than 8 columns in your dataset)
var <- colnames(data)[1:8]
print(var)

# Close any open graphics devices
graphics.off()

# Nested loop to plot relationships between all pairs of variables
for (i in seq_along(var)) {
  for (j in seq_along(var)) {
    if (i != j) {
      plot(data[[var[i]]], data[[var[j]]], col = data$Sucesso + 1,
           xlab = var[i], ylab = var[j], main = paste(var[i], "vs", var[j]))
      legend("topright", legend = paste(0:1), col = 1:2, pch = 19, bty = "n")
      ## Ensure the plot is displayed
      #print(plot(data[[var[i]]], data[[var[j]]], col = data$Sucesso + 1,
      #           xlab = var[i], ylab = var[j], main = paste(var[i], "vs", var[j])))
      #legend("topright", legend = paste(0:1), col = 1:2, pch = 19, bty = "n")
    }
  }
}
