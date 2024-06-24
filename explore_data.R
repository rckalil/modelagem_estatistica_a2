# Set working directory
setwd("/home/kalil/Documents/Graduacao/FGV/ME/modelagem_estatistica_a2")

# Load data
data <- read.csv("seven_wonders_duel.csv")

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
    }
  }
}

# Load required library for correlation plot
library(corrplot)

# Create correlation matrix and plot it
cor_matrix <- cor(data[1:8], use="complete.obs")
print(cor_matrix)
# Save the correlation plot as an image file
png("correlation_plot.png")
p <- corrplot(cor_matrix, method="color", addCoef.col="black", tl.col="black", tl.srt=45, 
     title="Matriz de Correlação", mar=c(0,0,1,0))
print(p)
dev.off()

# Remove rows with negative or NA values
data_clean <- data[complete.cases(data) & rowSums(data < 0, na.rm = TRUE) == 0, ]

# Verificar quantas observações pertencem a cada Jogo
# Substitua 'Jogo' pelo nome da coluna que identifica os jogos no seu dataset
jogo_counts <- table(data_clean$Jogo)
print(jogo_counts)
exercito_counts <- table(data_clean$Exército)
print(exercito_counts)

# Imprimir de forma informativa
cat("Contagem de observações por jogo:\n")
for (jogo in names(jogo_counts)) {
  cat(paste(jogo, ":", jogo_counts[jogo], "observações\n"))
}
cat("Contagem de observações por exercito:\n")
for (exercito in names(exercito_counts)) {
  cat(paste(exercito, ":", exercito_counts[exercito], "observações\n"))
}

# Save the cleaned data to a new CSV file
write.csv(data_clean, "seven_wonders_duel_cleaned.csv", row.names = FALSE)


