# Set working directory
setwd("/home/kalil/Documents/Graduacao/FGV/ME/modelagem_estatistica_a2")

# Load data
data <- read.csv("seven_wonders_duel_cleaned.csv")

# Certificar que a variável 'Sucesso' é binária
data$Sucesso <- as.factor(data$Sucesso)

# Definir as variáveis selecionadas para o modelo
variaveis_selecionadas <- read.csv("variaveis_selecionadas_com_interacoes.csv")
selected_vars <- as.vector(variaveis_selecionadas$Variaveis)

# Criar a fórmula para o modelo probit
formula <- as.formula(paste("Sucesso ~", paste(selected_vars, collapse = " + ")))
print(formula)

# Ajustar o modelo probit
probit_model <- glm(formula, data = data, family = binomial(link = "probit"))
summary(probit_model)

# Verificar quantas observações pertencem a cada Jogo
jogo_counts <- table(data$Jogo)
print(jogo_counts)

cat("Contagem de observações por jogo:\n")
for (jogo in names(jogo_counts)) {
  cat(paste(jogo, ":", jogo_counts[jogo], "observações\n"))
}

# Particionar os dados em três amostras com base na variável 'Jogo'
jogo_niveis <- unique(data$Jogo)
data1 <- subset(data, Jogo == jogo_niveis[1])
data2 <- subset(data, Jogo == jogo_niveis[2])
data3 <- subset(data, Jogo == jogo_niveis[3])

# Obter previsões do modelo probit para cada amostra
predictions1 <- predict(probit_model, newdata = data1, type = "response")
predictions2 <- predict(probit_model, newdata = data2, type = "response")
predictions3 <- predict(probit_model, newdata = data3, type = "response")

# Aplicar o teste K-S para cada par de distribuições de previsões
ks_test_12 <- ks.test(predictions1, predictions2)
ks_test_13 <- ks.test(predictions1, predictions3)
ks_test_23 <- ks.test(predictions2, predictions3)

# Imprimir os resultados dos testes K-S
cat("Teste K-S entre amostra 1 e amostra 2:\n")
print(ks_test_12)

cat("\nTeste K-S entre amostra 1 e amostra 3:\n")
print(ks_test_13)

cat("\nTeste K-S entre amostra 2 e amostra 3:\n")
print(ks_test_23)

# Combinar previsões em um dataframe para visualização
pred_data <- data.frame(
  Predictions = c(predictions1, predictions2, predictions3),
  Jogo = factor(rep(jogo_niveis, times = c(length(predictions1), length(predictions2), length(predictions3))))
)

# Plotar boxplot das previsões por grupo de jogo
boxplot(Predictions ~ Jogo, data = pred_data, main = "Distribuição das Previsões por Jogo",
    xlab = "Jogo", ylab = "Previsões", col = c("black", "white", "gray"))

# Salvar o plot em um arquivo
png("boxplot.png")
boxplot(Predictions ~ Jogo, data = pred_data, main = "Distribuição das Previsões por Jogo",
    xlab = "Jogo", ylab = "Previsões", col = c("black", "white", "grey"))
dev.off()

# ANOVA para comparar as médias das previsões entre os grupos de jogo
anova_results <- aov(Predictions ~ Jogo, data = pred_data)
summary(anova_results)
print(summary(anova_results))
