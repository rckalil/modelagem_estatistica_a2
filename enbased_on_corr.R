# Configurar o diretório de trabalho e carregar os dados
setwd("/home/kalil/Documents/Graduacao/FGV/ME/modelagem_estatistica_a2")
data <- read.csv("seven_wonders_duel.csv")
var <- colnames(data)

# Verificar o resumo dos dados
summary(data)

# Se a variável dependente 'Sucesso' não for binária, você pode precisar transformá-la. Supondo que 'Sucesso' é binária (0 ou 1)
data$Sucesso <- as.factor(data$Sucesso)  # Certificar que é um fator binário
data$Exército <- as.factor(data$Exército)

# Criar a fórmula para o modelo inicial
formula <- as.formula(paste("Sucesso ~ Exército + Cívico + Guilda + Ciência*Maravilha + Dinheiro*Comércio"))
print(formula)

# Ajustar o modelo inicial
model <- glm(formula, data = data, family = binomial)
summary(model)

AICc <- function(model) {
    k <- length(coefficients(model))
    n <- length(data$Sucesso)
    AIC(model) + 2 * k * (k + 1) / (n - k - 1)
}

print("AICc do modelo inicial:")
print(AICc(model))
