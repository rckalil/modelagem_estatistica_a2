# Configurar o diretório de trabalho e carregar os dados
setwd("/home/kalil/Documents/Graduacao/FGV/ME")
data <- read.csv("seven_wonders_duel.csv")
var <- colnames(data)

# Verificar o resumo dos dados
summary(data)

# Se a variável dependente 'Sucesso' não for binária, você pode precisar transformá-la. Supondo que 'Sucesso' é binária (0 ou 1)
data$Sucesso <- as.factor(data$Sucesso)  # Certificar que é um fator binário
data$Exército <-as.factor(data$Exército)

# Criar a fórmula para o modelo inicial
formula <- as.formula(paste("Sucesso ~", paste(var[1:8], collapse = " + ")))
print(formula)

# Ajustar o modelo inicial
t_model <- glm(formula, data = data, family = binomial)
summary(t_model)

# Inicializar lista para armazenar os valores de AIC
aic_values <- list()

# Calcular o AIC para cada variável individualmente e selecionar a melhor
for (i in seq_along(var)) {
  if (var[i] != "Sucesso") {  # Excluir a variável resposta
    formula <- as.formula(paste("Sucesso ~", var[i]))
    model <- glm(formula, data = data, family = binomial)
    aic_values[[var[i]]] <- AIC(model)
  }
}

print(aic_values)
best_var <- names(which.min(aic_values))
x_1 <- best_var
print(x_1)

# Lista de variáveis selecionadas
selected_vars <- c(x_1)

# Função para atualizar o modelo e calcular o AIC
update_aic <- function(selected_vars, var) {
  aic_values <- list()
  for (i in seq_along(var)) {
    if (!(var[i] %in% c("Sucesso", selected_vars))) {  # Excluir a variável resposta e as variáveis já selecionadas
      formula <- as.formula(paste("Sucesso ~", paste(selected_vars, collapse = " + "), "+", var[i]))
      model <- glm(formula, data = data, family = binomial)
      aic_values[[var[i]]] <- AIC(model)
    }
  }
  return(aic_values)
}

# Iterativamente adicionar variáveis ao modelo até que nenhuma melhoria significativa no AIC seja alcançada
improvement <- TRUE

while (improvement) {
  aic_values <- update_aic(selected_vars, var)
  print(aic_values)
  
  if (length(aic_values) == 0) break  # Nenhuma variável restante para adicionar
  
  best_var <- names(which.min(aic_values))
  
  if (length(selected_vars) == 1 || AIC(glm(as.formula(paste("Sucesso ~", paste(selected_vars, collapse = " + "))), data = data, family = binomial)) > aic_values[[best_var]]) {
    selected_vars <- c(selected_vars, best_var)
    print(selected_vars)
  } else {
    improvement <- FALSE
  }
}

# Resultado final
print("Variáveis selecionadas para o modelo final:")
print(selected_vars)

first_model <- glm(as.formula(paste("Sucesso ~", paste(selected_vars, collapse = " + "))), data = data, family = binomial)
summary(first_model)

# Salvar as variáveis selecionadas em um arquivo CSV
write.csv(data.frame(Variaveis = selected_vars), "variaveis_selecionadas.csv", row.names = FALSE)

# Exibir o modelo final
summary(first_model)
