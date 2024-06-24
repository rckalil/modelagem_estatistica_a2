# Configurar o diretório de trabalho e carregar os dados
setwd("/home/kalil/Documents/Graduacao/FGV/ME/modelagem_estatistica_a2")
data <- read.csv("seven_wonders_duel.csv")
var <- colnames(data)

# Se a variável dependente 'Sucesso' não for binária, você pode precisar transformá-la. Supondo que 'Sucesso' é binária (0 ou 1)
data$Sucesso <- as.factor(data$Sucesso)  # Certificar que é um fator binário
data$Exército <- as.factor(data$Exército)

# Carregar as variáveis selecionadas de um arquivo CSV
variaveis_selecionadas <- read.csv("variaveis_selecionadas_com_interacoes.csv")
# Converter o dataframe em um vetor de variáveis
selected_vars <- as.vector(variaveis_selecionadas$Variaveis)

AICc <- function(model) {
  k <- length(coefficients(model))
  n <- length(data$Sucesso)
  AIC(model) + 2 * k * (k + 1) / (n - k - 1)
}

# Criar a fórmula para o modelo
formula <- as.formula(paste("Sucesso ~", paste(selected_vars, collapse = " + ")))
print(formula)

# Ajustar modelo logit
model_logit <- glm(formula, data = data, family = binomial)
summary(model_logit)
print(summary(model_logit))
print("AICc do modelo logit:")
print(AICc(model_logit))

# Ajustar modelo probit
model_probit <- glm(formula, data = data, family = binomial(link = "probit"))
summary(model_probit)
print(summary(model_probit))
print("AICc do modelo probit:")
print(AICc(model_probit))
