# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ROCR")
require("ggplot2")
require(caret)

# Poner la carpeta de la materia de SU computadora local
setwd("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/DM-EyF")  #Establezco el Working Directory
#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")
# Poner sus semillas
semillas <- c(100621,
              102149,
              202061,
              257093,
              584723)

ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
  )
}


# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202101]
# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

# Seteamos nuestra primera semilla
set.seed(semillas[1])

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dataset$clase_binaria,
                                          p = 0.70, list = FALSE)
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]

#### grid search ####
resultados_grid_search <- data.table()

# Complete los valores que se van a combinar para cada parámetro a explorar

for (cp in c(-1, 0.1,0.01,0.001)) {# 0.1 para abajo
  for (md in c(4:15)) {# 4 a 15
    for (ms in c(1:50)) {
      for (mb in c(1, as.integer(ms / 2),as.integer(ms / 3),as.integer(ms / 4))) {# otros divisores
        
        t0 <- Sys.time()
        gan_semillas <- c()
        for (s in semillas) {
          set.seed(s)
          in_training <- caret::createDataPartition(dataset[,
                                                            get("clase_binaria")],
                                                    p = 0.70, list = FALSE)
          dtrain  <-  dataset[in_training, ]
          dtest   <-  dataset[-in_training, ]
          
          modelo <- rpart(clase_binaria ~ .,
                          data = dtrain,
                          xval = 0,
                          cp = cp,
                          minsplit = ms,
                          minbucket = mb,
                          maxdepth = md)
          
          pred_testing <- predict(modelo, dtest, type = "prob")
          gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
          
          gan_semillas <- c(gan_semillas, gan)
        }
        tiempo <-  as.numeric(Sys.time() - t0, units = "secs")
        
        resultados_grid_search <- rbindlist(list(
          resultados_grid_search,
          data.table(
            tiempo = tiempo,
            cp = cp,
            mb = mb,
            ms = ms,
            md = md,
            gan = mean(gan_semillas))
        ))
        
        write.csv(resultados_grid_search,"resultados_v2.csv", row.names = FALSE)
        print(resultados_grid_search)
      }
    }
  }
}

resultados_grid_search <- read.csv("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/DM-EyF/resultados.csv")
resultados_grid_search<-as.data.table(resultados_grid_search)

# Visualizo los parámetros de los mejores parámetros
View(resultados_grid_search[gan == max(gan), ])

#### search 2 ####
resultados_search_2<-data.table()
gain=0
for (cp in c(-1, 0.1,0.01,0.001)) {# 0.1 para abajo
  gan_semillas <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset[,
                                                      get("clase_binaria")],
                                              p = 0.70, list = FALSE)
    dtrain  <-  dataset[in_training, ]
    dtest   <-  dataset[-in_training, ]
    
    modelo <- rpart(as.factor(clase_binaria) ~ .,
                    data = dtrain,
                    xval = 0,
                    cp = cp)
    
    pred_testing <- predict(modelo, dtest, type = "prob")
    gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
    
    gan_semillas <- c(gan_semillas, gan)
  }
  
  resultados_search_2 <- rbindlist(list(
    resultados_search_2,
    data.table(
      cp = cp,
      mb = 0,
      ms = 0,
      md = 0,
      gan = mean(gan_semillas))
  ))
  
  if(mean(gan_semillas)>gain){
    gain=mean(gan_semillas)
    cpF=cp
  }
}

for (md in c(4:15)) {# 4 a 15
  print(md)
  gan_semillas <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset[,
                                                      get("clase_binaria")],
                                              p = 0.70, list = FALSE)
    dtrain  <-  dataset[in_training, ]
    dtest   <-  dataset[-in_training, ]
    
    modelo <- rpart(clase_binaria ~ .,
                    data = dtrain,
                    xval = 0,
                    cp = cpF,
                    maxdepth=md)
    
    pred_testing <- predict(modelo, dtest, type = "prob")
    gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
    
    gan_semillas <- c(gan_semillas, gan)
  }
  
  resultados_search_2 <- rbindlist(list(
    resultados_search_2,
    data.table(
      cp = cpF,
      mb = 0,
      ms = 0,
      md = md,
      gan = mean(gan_semillas))
  ))
  
  if(mean(gan_semillas)>=gain){
    gain=mean(gan_semillas)
    mdF=md
  }
  print(mean(gan_semillas))
  
}

for (ms in c(1:50)) {
  print(ms)
  gan_semillas <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset[,
                                                      get("clase_binaria")],
                                              p = 0.70, list = FALSE)
    dtrain  <-  dataset[in_training, ]
    dtest   <-  dataset[-in_training, ]
    
    modelo <- rpart(clase_binaria ~ .,
                    data = dtrain,
                    xval = 0,
                    cp = cpF,
                    maxdepth=mdF,
                    minsplit = ms)
    
    pred_testing <- predict(modelo, dtest, type = "prob")
    gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
    
    gan_semillas <- c(gan_semillas, gan)
  }
  
  resultados_search_2 <- rbindlist(list(
    resultados_search_2,
    data.table(
      cp = cpF,
      mb = 0,
      ms = ms,
      md = mdF,
      gan = mean(gan_semillas))
  ))
  
  if(mean(gan_semillas)>gain){
    gain=mean(gan_semillas)
    msF=ms
  }
  print(mean(gan_semillas))
}

for (mb in c(1, as.integer(ms / 2),as.integer(ms / 3),as.integer(ms / 4))) {# otros divisores
  print(mb)
  gan_semillas <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset[,
                                                      get("clase_binaria")],
                                              p = 0.70, list = FALSE)
    dtrain  <-  dataset[in_training, ]
    dtest   <-  dataset[-in_training, ]
    
    modelo <- rpart(clase_binaria ~ .,
                    data = dtrain,
                    xval = 0,
                    cp = cpF,
                    minsplit = msF,
                    minbucket = mb,
                    maxdepth = mdF)
    
    pred_testing <- predict(modelo, dtest, type = "prob")
    gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
    
    gan_semillas <- c(gan_semillas, gan)
  }

  resultados_search_2 <- rbindlist(list(
    resultados_search_2,
    data.table(
      
      cp = cpF,
      mb = mb,
      ms = msF,
      md = mdF,
      gan = mean(gan_semillas))
  ))
  
  if(mean(gan_semillas)>gain){
    gain=mean(gan_semillas)
    mbF=mb
  }
  print(mean(gan_semillas))
  
  
}
mbF=1
# empiezo de nuevo

for (cp in c(-1, 0.1,0.01,0.001)) {# 0.1 para abajo
  gan_semillas <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset[,
                                                      get("clase_binaria")],
                                              p = 0.70, list = FALSE)
    dtrain  <-  dataset[in_training, ]
    dtest   <-  dataset[-in_training, ]
    
    modelo <- rpart(as.factor(clase_binaria) ~ .,
                    data = dtrain,
                    xval = 0,
                    cp = cp,
                    minsplit = msF,
                    minbucket = mbF,
                    maxdepth = mdF)
    
    pred_testing <- predict(modelo, dtest, type = "prob")
    gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
    
    gan_semillas <- c(gan_semillas, gan)
  }
  
  resultados_search_2 <- rbindlist(list(
    resultados_search_2,
    data.table(
      cp = cp,
      mb = mbF,
      ms = msF,
      md = mdF,
      gan = mean(gan_semillas))
  ))
  
  if(mean(gan_semillas)>gain){
    gain=mean(gan_semillas)
    cpF=cp
  }
  print(mean(gan_semillas))
  
}

for (md in c(4:15)) {# 4 a 15
  print(md)
  gan_semillas <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset[,
                                                      get("clase_binaria")],
                                              p = 0.70, list = FALSE)
    dtrain  <-  dataset[in_training, ]
    dtest   <-  dataset[-in_training, ]
    
    modelo <- rpart(clase_binaria ~ .,
                    data = dtrain,
                    xval = 0,
                    cp = cpF,
                    minsplit = msF,
                    minbucket = mbF,
                    maxdepth = md)
    
    pred_testing <- predict(modelo, dtest, type = "prob")
    gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
    
    gan_semillas <- c(gan_semillas, gan)
  }
  
  resultados_search_2 <- rbindlist(list(
    resultados_search_2,
    data.table(
      cp = cpF,
      mb = mbF,
      ms = msF,
      md = md,
      gan = mean(gan_semillas))
  ))
  
  if(mean(gan_semillas)>=gain){
    gain=mean(gan_semillas)
    mdF=md
  }
 
   print(mean(gan_semillas))
  
}

for (ms in c(1:30)) {
  print(ms)
  gan_semillas <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset[,
                                                      get("clase_binaria")],
                                              p = 0.70, list = FALSE)
    dtrain  <-  dataset[in_training, ]
    dtest   <-  dataset[-in_training, ]
    
    modelo <- rpart(clase_binaria ~ .,
                    data = dtrain,
                    xval = 0,
                    cp = cpF,
                    maxdepth=mdF,
                    minbucket = mbF,
                    minsplit = ms)
    
    pred_testing <- predict(modelo, dtest, type = "prob")
    gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
    
    gan_semillas <- c(gan_semillas, gan)
  }
  
  resultados_search_2 <- rbindlist(list(
    resultados_search_2,
    data.table(
      cp = cpF,
      mb = mbF,
      ms = ms,
      md = mdF,
      gan = mean(gan_semillas))
  ))
  
  if(mean(gan_semillas)>gain){
    gain=mean(gan_semillas)
    msF=ms
  }
  print(mean(gan_semillas))
}

for (mb in c(1, as.integer(ms / 2),as.integer(ms / 3),as.integer(ms / 4))) {# otros divisores
  print(mb)
  gan_semillas <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset[,
                                                      get("clase_binaria")],
                                              p = 0.70, list = FALSE)
    dtrain  <-  dataset[in_training, ]
    dtest   <-  dataset[-in_training, ]
    
    modelo <- rpart(clase_binaria ~ .,
                    data = dtrain,
                    xval = 0,
                    cp = cpF,
                    minsplit = msF,
                    minbucket = mb,
                    maxdepth = mdF)
    
    pred_testing <- predict(modelo, dtest, type = "prob")
    gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
    
    gan_semillas <- c(gan_semillas, gan)
  }
  
  resultados_search_2 <- rbindlist(list(
    resultados_search_2,
    data.table(
      
      cp = cpF,
      mb = mb,
      ms = msF,
      md = mdF,
      gan = mean(gan_semillas))
  ))
  
  if(mean(gan_semillas)>gain){
    gain=mean(gan_semillas)
    mbF=mb
  }
  print(mean(gan_semillas))
  
  
}
write.csv(resultados_search_2,"resultados_s2.csv", row.names = FALSE)
