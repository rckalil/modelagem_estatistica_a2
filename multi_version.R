# Configurar o diretório de trabalho e carregar os dados
setwd("/home/kalil/Documents/Graduacao/FGV/ME/modelagem_estatistica_a2")
data <- read.csv("seven_wonders_duel.csv")
var <- colnames(data)

# Se a variável dependente 'Sucesso' não for binária, você pode precisar transformá-la. Supondo que 'Sucesso' é binária (0 ou 1)
data$Sucesso <- as.factor(data$Sucesso)  # Certificar que é um fator binário
data$Exército <- as.factor(data$Exército)
data$Jogo <- as.factor(data$Jogo)

# Carregar as variáveis selecionadas de um arquivo CSV
variaveis_selecionadas <- read.csv("variaveis_selecionadas_com_interacoes.csv")
# Converter o dataframe em um vetor de variáveis
selected_vars <- as.vector(variaveis_selecionadas$Variaveis)

AICc <- function(model) {
  k <- length(coefficients(model))
  n <- length(data$Sucesso)
  AIC(model) + 2 * k * (k + 1) / (n - k - 1)
}

formula <- as.formula(paste("Sucesso ~", paste(selected_vars, collapse = " + "), " + Jogo"))
print(formula)
model_intercept <- glm(formula, data = data, family = binomial(link = "probit"))
#summary(model_intercept)
print(summary(model_intercept))
print("AICc do modelo intercept:")
print(AICc(model_intercept))

# Criar a fórmula com todas as variáveis interagindo com "Jogo"
formula <- as.formula(paste("Sucesso ~", paste(paste(selected_vars, "*", "Jogo"), collapse = " + ")))

# Ajustar o modelo e imprimir os resultados
model <- glm(formula, data = data, family = binomial(link = "probit"))
print(summary(model))
print("AICc do modelo:")
print(AICc(model))


formula <- as.formula(paste("Sucesso ~", paste(selected_vars[!selected_vars %in% "Exército"], collapse = " + "), " + Jogo"))
print(formula)
model_intercept <- glm(formula, data = data, family = binomial(link = "probit"))
#summary(model_intercept)
print(summary(model_intercept))
print("AICc do modelo intercept:")
print(AICc(model_intercept))

# Criar a fórmula com todas as variáveis interagindo com "Jogo", exceto "Exército"
formula <- as.formula(paste("Sucesso ~", paste(paste(selected_vars[!selected_vars %in% "Exército"], "*", "Jogo"), collapse = " + ")))
# Ajustar o modelo e imprimir os resultados
model_no_exercito <- glm(formula, data = data, family = binomial(link = "probit"))
print(summary(model_no_exercito))
print("AICc do modelo sem Exército:")
print(AICc(model_no_exercito))