setwd("/home/kalil/Documents/Graduacao/FGV/ME")
data <- read.csv("seven_wonders_duel.csv")

model <- glm(data$Sucesso ~ data$Ciência*data$Exército, family=binomial)
summary(model)

print(colnames(data[]))
var = colnames(data)
print(var[2])

aic_values <- list()

for (i in seq_along(var)){
  formula <- as.formula(paste("Sucesso ~", var[i]))
  model <- glm(formula, data=data)
  aic_values[[var[i]]]<-AIC(model)
}
print(aic_values)


aic_values <- list()

for (i in seq_along(var)){
  formula <- as.formula(paste("Sucesso ~ Guilda + ", var[i]))
  model <- glm(formula, data=data, family=binomial)
  aic_values[[var[i]]]<-AIC(model)
}
print(aic_values)


aic_values <- list()

for (i in seq_along(var)){
  formula <- as.formula(paste("Sucesso ~ Guilda + Ciência + ", var[i]))
  model <- glm(formula, data=data)
  aic_values[[var[i]]]<-AIC(model)
}
print(aic_values)


aic_values <- list()

for (i in seq_along(var)){
  formula <- as.formula(paste("Sucesso ~ Guilda + Ciência + Dinheiro + ", var[i]))
  model <- glm(formula, data=data)
  aic_values[[var[i]]]<-AIC(model)
}
print(aic_values)


aic_values <- list()

for (i in seq_along(var)){
  formula <- as.formula(paste("Sucesso ~ Guilda + Ciência + Dinheiro + Maravilha + ", var[i]))
  model <- glm(formula, data=data)
  aic_values[[var[i]]]<-AIC(model)
}
print(aic_values)


aic_values <- list()

for (i in seq_along(var)){
  formula <- as.formula(paste("Sucesso ~ Guilda + Ciência + Dinheiro + Maravilha + Exército +", var[i]))
  model <- glm(formula, data=data)
  aic_values[[var[i]]]<-AIC(model)
}
print(aic_values)


aic_values <- list()

for (i in seq_along(var)){
  formula <- as.formula(paste("Sucesso ~ Guilda + Ciência + Dinheiro + Maravilha + Exército + Comércio+", var[i]))
  model <- glm(formula, data=data, family=binomial)
  aic_values[[var[i]]]<-AIC(model)
}
print(aic_values)