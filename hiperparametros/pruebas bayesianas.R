rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ROCR")
require("ggplot2")
require(caret)
require(smoof)
require("mlrMBO")

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

ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
  )
}

modelo_rpart_ganancia <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10) {
  modelo <- rpart(clase_binaria ~ ., data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  ganancia(test_prediccion[, "evento"], test$clase_binaria) / 0.3
}

experimento_rpart_completo <- function(ds, semillas, cp = -1, ms = 20, mb = 1, md = 10) {
  gan <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
                                              list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
    #train_sample <- tomar_muestra(train)
    r <- modelo_rpart_ganancia(train, test, 
                               cp = cp, ms = ms, mb = mb, md = md)
    gan <- c(gan, r)
  }
  mean(gan)
}

#### ejercicio de clase// busqueda bayesiana ####

set.seed(semillas[1])
obj_fun_md_ms <- function(x) {
  experimento_rpart_completo(dtrain, semillas
                    , md = x$maxdepth
                    , ms = x$minsplit
                    , cp = x$cp
                    , mb = floor(x$minbucket*x$minsplit))
}

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms,
  par.set = makeParamSet(
    makeIntegerParam("maxdepth",  lower = 4L, upper = 20L),
    makeIntegerParam("minsplit",  lower = 1L, upper = 500L),
    makeNumericParam("cp",lower = -1, upper = 1),
    makeNumericParam("minbucket",lower = 0L, upper = 1)
  ),
  # noisy = TRUE,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 30L)
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
  # sacar parámetro opt.focussearch.points en próximas ejecuciones
  opt.focussearch.points = 20
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)

?readRDS
saveRDS(run_md_ms, "bayesianas.rds")
