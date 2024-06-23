# Configurar o diretório de trabalho e carregar os dados
setwd("/home/kalil/Documents/Graduacao/FGV/ME")
data <- read.csv("seven_wonders_duel.csv")
var <- colnames(data)
allowed_vars = var[1:8]

# Carregar as variáveis selecionadas de um arquivo CSV
variaveis_selecionadas <- read.csv("variaveis_selecionadas.csv")

# Converter o dataframe em um vetor de variáveis
selected_vars <- as.vector(variaveis_selecionadas$Variaveis)

# Certificar que a variável dependente é binária
data$Sucesso <- as.factor(data$Sucesso)
#Descomenmte a linha abaixo se quiser considerar Exército uma variável hierárquica
#data$Exército <- as.factor(data$Exército)

# Exibir as variáveis selecionadas
print("Variáveis selecionadas para o modelo final:")
print(selected_vars)

# Criar o modelo com as variáveis selecionadas
final_model <- glm(as.formula(paste("Sucesso ~", paste(selected_vars, collapse = " + "))), data = data, family = binomial)
summary(final_model)

# Função para avaliar interações e retornar o AIC
evaluate_interactions <- function(selected_vars, var_index, allowed_vars) {
  aic_values <- list()
  var <- selected_vars[var_index]
  for (i in seq_along(allowed_vars)) {
    if (!(allowed_vars[i] %in% selected_vars)) {  # Garantir que a variável não seja considerada em selected_vars
      formula <- as.formula(paste("Sucesso ~", paste(selected_vars[-var_index], collapse = " + "), "+", var, "*", allowed_vars[i]))
      model <- glm(formula, data = data, family = binomial)
      aic_values[[allowed_vars[i]]] <- AIC(model)
    }
  }
  return(aic_values)
}

# Iterar sobre cada variável já incluída no modelo
print("Variáveis iniciais")
print(selected_vars)
new_vars <- selected_vars
for (variable in seq_along(selected_vars)) {
  # Obter os valores de AIC para as interações possíveis
  aic_values <- evaluate_interactions(selected_vars, variable, allowed_vars)
  
  # Identificar a melhor interação
  if (length(aic_values) > 0) {
    best_interaction <- names(which.min(aic_values))
    
    # Verificar se a melhor interação realmente melhora o AIC
    current_aic <- AIC(final_model)
    best_aic <- aic_values[[best_interaction]]
    
    if (best_aic < current_aic) {
      # Adicionar a melhor interação ao modelo
      interaction_formula <- as.formula(paste("Sucesso ~", paste(selected_vars, collapse = " + "), "+", selected_vars[variable], "*", best_interaction))
      final_model <- glm(interaction_formula, data = data, family = binomial)
      
      # Adicionar a interação à lista de variáveis selecionadas
      selected_vars <- c(selected_vars, paste(selected_vars[variable], best_interaction, sep = "*"))
      
      print(paste("Interação adicionada:", selected_vars[variable], "*", best_interaction))
    }
  }
}
print("Variáveis ao final")
print(selected_vars)

# Exibir o modelo final
summary(final_model)
print(interaction_formula)

# Salvar as variáveis selecionadas e interações em um arquivo CSV
write.csv(data.frame(Variaveis = new_vars), "variaveis_selecionadas_com_interacoes.csv", row.names = FALSE)
