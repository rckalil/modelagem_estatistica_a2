# Usando AIC value para inserir variáveis sem interação no modelo linear

setwd("/home/kalil/Documents/Graduacao/FGV/ME")
data <- read.csv("seven_wonders_duel.csv")
var = colnames(data)

aic_values <- list()

for (i in seq_along(var)){
  formula <- as.formula(paste("Sucesso ~", var[i]))
  model <- glm(formula, data=data)
  aic_values[[var[i]]]<-AIC(model)
}
print(aic_values)
print(which.min(aic_values))
x_1 = var[which.min(aic_values)]
print(x_1)

for (i in seq_along(var)){
  formula <- as.formula(paste("Sucesso ~", x_1, " + ", var[i]))
  model <- glm(formula, data=data)
  aic_values[[var[i]]]<-AIC(model)
}

print(aic_values)
x_2 = var[which.min(aic_values)]

x <- list()
x[[1]] <- x_1
x[[2]] <- x_2

for (i in seq_along(var)){
  for (i in seq_along(x)){
    
  }
  formula <- as.formula(paste("Sucesso ~", x_1, " + ", x_2, " + ", var[i]))
  model <- glm(formula, data=data)
  aic_values[[var[i]]]<-AIC(model)
}
print(aic_values)
