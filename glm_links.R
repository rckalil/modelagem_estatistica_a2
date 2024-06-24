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

# Obtenha o coeficiente do modelo Probit
coef_probit <- coef(model_probit)

# Multiplicar os coeficientes Probit por 1.6 para obter os equivalentes Logit
coef_logit_equivalente <- coef_probit * 1.6

# Calcular a probabilidade associada a cada coeficiente Probit
prob_probit_equivalente <- pnorm(coef_probit)

# Calcular a probabilidade associada a cada coeficiente logit equivalente
prob_logit_equivalente <- pnorm(coef_logit_equivalente)

# Crie a nova tabela com os parâmetros na escala dos dados
param_table <- data.frame(
  Coeficiente_Probit = coef_probit[c("(Intercept)", "Exército2", "Exército5", "Exército10")],
  Probabilidade_Probit_Equivalente = prob_probit_equivalente[c("(Intercept)", "Exército2", "Exército5", "Exército10")]
)

print(param_table)

# Calcular os valores médios de cada variável do modelo
mean_guilda <- mean(as.numeric(data$Guilda), na.rm = TRUE)
mean_ciencia <- mean(as.numeric(data$Ciência), na.rm = TRUE)
mean_dinheiro <- mean(as.numeric(data$Dinheiro), na.rm = TRUE)

# Calcular o valor de Z médio usando os coeficientes Probit
z_mean_probit <- coef_probit["(Intercept)"] +
  coef_probit["Guilda"] * mean_guilda +
  coef_probit["Ciência"] * mean_ciencia +
  coef_probit["Dinheiro"] * mean_dinheiro +
  coef_probit["Dinheiro:Ciência"] * (mean_dinheiro * mean_ciencia)

# Calcular o valor de Z médio usando os coeficientes Logit equivalentes
z_mean_logit_equivalente <- coef_logit_equivalente["(Intercept)"] +
  coef_logit_equivalente["Guilda"] * mean_guilda +
  coef_logit_equivalente["Ciência"] * mean_ciencia +
  coef_logit_equivalente["Dinheiro"] * mean_dinheiro +
  coef_logit_equivalente["Dinheiro:Ciência"] * (mean_dinheiro * mean_ciencia)

# Converter Z para probabilidade usando a função de distribuição cumulativa normal (Probit)
prob_sucesso_media_probit <- pnorm(z_mean_probit)

# Converter Z para probabilidade usando a função de distribuição cumulativa logística (Logit)
prob_sucesso_media_logit <- plogis(z_mean_logit_equivalente)

# Exibir as probabilidades médias de sucesso
print(paste("A probabilidade média de sucesso (Probit) é de", round(prob_sucesso_media_probit * 100, 2), "%"))
print(paste("A probabilidade média de sucesso (Logit Equivalente) é de", round(prob_sucesso_media_logit * 100, 2), "%"))

# Calcular o impacto das variáveis na probabilidade de sucesso (derivadas)
deriv_guilda <- coef_logit_equivalente["Guilda"] * dlogis(z_mean_logit_equivalente)
deriv_ciencia <- (coef_logit_equivalente["Ciência"] + coef_logit_equivalente["Dinheiro:Ciência"] * mean_dinheiro) * dlogis(z_mean_logit_equivalente)
deriv_dinheiro <- (coef_logit_equivalente["Dinheiro"] + coef_logit_equivalente["Dinheiro:Ciência"] * mean_ciencia) * dlogis(z_mean_logit_equivalente)
deriv_interacao <- coef_logit_equivalente["Dinheiro:Ciência"] * (mean_ciencia + mean_dinheiro) * dlogis(z_mean_logit_equivalente)

# Criar a tabela de impacto das variáveis
impact_table <- data.frame(
  Variável = c("Guilda", "Ciência", "Dinheiro", "Dinheiro:Ciência"),
  Derivada_Logit_Equivalente = c(deriv_guilda, deriv_ciencia, deriv_dinheiro, deriv_interacao),
  Prob_Sucesso_Media_Probit = prob_sucesso_media_probit,
  Prob_Sucesso_Media_Logit = prob_sucesso_media_logit
)

print(impact_table)
