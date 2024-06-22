# Configurar o diretório de trabalho e carregar os dados
setwd("/home/kalil/Documents/Graduacao/FGV/ME")
data <- read.csv("seven_wonders_duel.csv")
var <- colnames(data)

summary(data)
formula <- as.formula(paste("Sucesso ~", paste(var[1:8], collapse = " + ")))
print(formula)
t_model <- glm(formula, data=data)
summary(t_model)

# Inicializar lista para armazenar os valores de AIC
aic_values <- list()

# Calcular o AIC para cada variável individualmente e selecionar a melhor
for (i in seq_along(var)) {
  if (var[i] != "Sucesso") {  # Excluir a variável resposta
    formula <- as.formula(paste("Sucesso ~", var[i]))
    model <- glm(formula, data=data)
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
      model <- glm(formula, data=data)
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
  
  if (length(selected_vars) == 1 || AIC(glm(as.formula(paste("Sucesso ~", paste(selected_vars, collapse = " + "))), data=data)) > aic_values[[best_var]]) {
    selected_vars <- c(selected_vars, best_var)
    print(selected_vars)
  } else {
    improvement <- FALSE
  }
}

# Resultado final
print("Variáveis selecionadas para o modelo final:")
print(selected_vars)

first_model <- glm(as.formula(paste("Sucesso ~", paste(selected_vars, collapse = " + "))), data=data)
summary(first_model)

plot(data$Guilda, data$Sucesso, main="Projeção e reta estimada", xlab="Guilda", ylab="Sucesso")
abline(a=0.008621, b=0.028619, col="red")

plot(data$Dinheiro, data$Sucesso, main="Projeção e reta estimada", xlab="Dinheiro", ylab="Sucesso")
abline(a=0.008621, b=0.025991, col="red")

plot(data$Maravilha, data$Sucesso, main="Projeção e reta estimada", xlab="Maravilha", ylab="Sucesso")
abline(a=0.008621, b=0.027930, col="red")

plot(data$Exército, data$Sucesso, main="Projeção e reta estimada", xlab="Exército", ylab="Sucesso")
abline(a=0.008621, b=-0.023385, col="red")

plot(data$Comércio, data$Sucesso, main="Projeção e reta estimada", xlab="Comércio", ylab="Sucesso")
abline(a=0.008621, b=-0.018250, col="red")

library(corrplot)
# Supondo que 'data' seja seu dataframe com as variáveis numéricas
cor_matrix <- cor(data[1:8], use="complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method="color", addCoef.col="black", tl.col="black", tl.srt=45, 
         title="Matriz de Correlação", mar=c(0,0,1,0))

#F1 score


for (j in seq_along(selected_vars)) {
  for (i in seq_along(var)
}
selected_interactions <- 
