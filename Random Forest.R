# Tratamiento de datos
# ==============================================================================
library(ISLR)
library(dplyr)
library(tidyr)
library(skimr)
library(readxl)
library(rpart)
library(rpart.plot)

# Gráficos
# ==============================================================================
library(ggplot2)
library(ggpubr)

# Preprocesado y modelado
# ==============================================================================
#library(tidymodels)
library(ranger)
library(doParallel)

# Datos
setwd("C:/Users/omar.luna/Desktop/Arboles_de_decision")
dataset = read_excel('01_int_c1_2022.xlsx',sheet = 2)
dataset = dataset[c(12:24,27:32)]
dataset$COD_ACT = as.factor(dataset$COD_ACT)
dataset$C1_DR = as.factor(ifelse(is.na(dataset$C1_DR)
                                  ,0,dataset$C1_DR))
dataset$C1_FA = as.factor(ifelse(is.na(dataset$C1_FA)
                                 ,0,dataset$C1_FA))
skim(dataset)


# creamos la tabla de entrenamiento y de prueba
#75% datos para entrenar

library(caTools)
set.seed(123)
split = sample.split(dataset$COD_ACT, SplitRatio = 0.75)
datos_train = subset(dataset, split == TRUE)
datos_test = subset(dataset, split == FALSE)


# Bucle para entrenar un modelo con cada valor de num_trees y extraer su error
# de entrenamiento y de Out-of-Bag.

# Valores evaluados
num_trees_range <- seq(1, 400, 20)

train_errors <- rep(NA, times = length(num_trees_range))
oob_errors   <- rep(NA, times = length(num_trees_range))

for (i in seq_along(num_trees_range)){
  modelo  <- ranger(
    formula   = datos_train$COD_ACT ~ .,
    data      = datos_train[,-1],
    num.trees = num_trees_range[i],
    oob.error = TRUE,
    seed      = 123
  )
  
  predicciones_train <- predict(
    modelo,
    data = datos_train[,-1]
  )
  predicciones_train <- predicciones_train$predictions
  oob_error   <- modelo$prediction.error
  oob_errors[i]   <- sqrt(oob_error)
  
}


# Gráfico con la evolución de los errores
df_resulados <- data.frame(n_arboles = num_trees_range, oob_errors)
ggplot(data = df_resulados) +
  #geom_line(aes(x = num_trees_range, y = train_errors, color = "train rmse")) + 
  geom_line(aes(x = num_trees_range, y = oob_errors, color = "oob rmse")) +
  geom_vline(xintercept = num_trees_range[which.min(oob_errors)],
             color = "firebrick",
             linetype = "dashed") +
  labs(
    title = "Evolución del out-of-bag-error vs número árboles",
    x     = "número de árboles",
    y     = "out-of-bag-error (rmse)",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

paste("Valor óptimo de num.trees:", num_trees_range[which.min(oob_errors)])



# Creación y entrenamiento del modelo con el número óptimo de árboles
# ==============================================================================
set.seed(123)
modelo <- ranger(
  formula   = datos_train$COD_ACT ~ .,
  data      = datos_train[,-1],
  num.trees = 381,
  seed      = 123
)

print(modelo)


# Error de test del modelo
# ==============================================================================
predicciones <- predict(
  modelo,
  data = datos_test[,-1]
)

predicciones <- predicciones$predictions
cm = data.frame(predicciones,datos_test[,1])

matriz_conf = table(cm$COD_ACT,cm$predicciones)

aciertos = 0
total = 0
for (i in 1:7) {
  for (j in 1:7) {
    total = total + matriz_conf[i,j]
    if (i == j) {
      aciertos = aciertos + matriz_conf[i,j]
  }
  }
}
predicciones_correctas = aciertos/total*100
predicciones_correctas

#saveRDS(modelo,file = 'modelo.rds')
#write.csv(datos_test,'datos_prueba.csv')



