#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot
#sacar id del cliente
#activos y pasivos
# gastos e ingresos
# cortar variables m�s importantes
# rankear datos
# comparar marzo y enero
# matar primera variable
#t-sne
# ver si tiene una tarjeta
# antiguedad por ganancia
# limite de las tarjetas y consumido
# ingresos y limite de la tarjeta
# ver nas
# reordenar rankings

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
require("beepr")
require(dplyr)

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/DM-EyF")  #Establezco el Working Directory
remove(list=ls())
gc()
#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")

semillas <- c(100621,
              102149,
              202061,
              257093,
              584723)

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

# paso a factor categoricas
did_recode_columns <- function(dt, cols, type = c("as.numeric", "as.factor", "as.character", "as.interger", "as.double") ) {
  # function used to convert data.table columns
  # to factor, numeric, or character
  library(data.table)
  dt[,(cols) := lapply(.SD, type), .SDcols = cols]
  
}


variables_factor <-c("cliente_vip", "internet", "tcuentas", "cdescubierto_preacordado", "active_quarter",
                     "Master_status", "Master_delinquency",
                     "Visa_status", "Visa_delinquency",
                     "tmobile_app","thomebanking","tcallcenter", "ccaja_seguridad"
                     )
#dataset[, variables_factor] <- lapply(dataset[, variables_factor], as.factor)



dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo
remove(dataset)
gc()
#### analisis variables ####


#dtrain

#hist(log(dtrain$mcuenta_corriente))

#dapply

#imputar media
#dtrain2[is.na(M_descuentos) & !is.na(C_descuentos), .(M_descuentos,C_descuentos, mtarjeta_visa_descuentos, ctarjeta_visa_descuentos, mtarjeta_master_descuentos,ctarjeta_master_descuentos, mcajeros_propios_descuentos, ccajeros_propios_descuentos)]->vers

#base<-dapply
# hago ejercicios de a 15 y veo qu� variables funcionan

aplicarCambios<-function(base){
  base$foto_mes<-NULL
  base$nulos<-apply(X = is.na(base), MARGIN = 1, FUN = sum)
  
  #base$log_ctrx_quarter<-log(base$ctrx_quarter)
  
  base$moroso2<-paste0(base$Visa_delinquency, base$Master_delinquency)
  #base$cierre2<-paste0(base$Visa_status, base$Master_status)
  
  #base$cierres<-ifelse(base$Visa_status %in% c(6,7,9) & base$Master_status %in% c(6,7,9),1,0)
  

  variables_factor2 <-c(  "moroso2"
                        #,"cierre2"
                        #,"cierres"
  )
  
  
  
  base[ , campo1 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales <2 ) ]
  base[ , campo2 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales>=2 ) ]
  
  base[ , campo3 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro <2601.1 ) ]
  base[ , campo4 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro>=2601.1 ) ]
  
  base[ , campo5 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status>=8 | is.na(Master_status) ) ) ]
  base[ , campo6 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status <8 & !is.na(Master_status) ) ) ]
  
  base[ , campo7 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
  base[ , campo8 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]
  
  
  
  mis_variables_2 <- c("ctrx_quarter",
                       "ccaja_ahorro",
                       "cdescubierto_preacordado",
                       "ctarjeta_visa",
                       "ctarjeta_debito",
                       "nulos",
                       "mcuenta_corriente",
                       "mcuentas_saldo",
                       "mcaja_ahorro",
                       "mprestamos_personales",
                       "cprestamos_personales",
                       "ccomisiones_otras",
                       "mcomisiones_mantenimiento",
                       "ccomisiones_mantenimiento",
                       "Visa_fechaalta",
                       "Visa_fultimo_cierre"
                       ) 
  
  
  for (n in 1:(length(mis_variables_2)-1)) {
    print(n)
    
    for (m in (n+1):length(mis_variables_2)) {
        nueva <- paste(mis_variables_2[n], mis_variables_2[m], sep = "___")
        base[, (nueva) := get(mis_variables_2[n]) * get(mis_variables_2[m])]
      
    }
  }
  
  
  #did_recode_columns(base, variables_factor, type = "as.factor")
  #did_recode_columns(base, variables_factor2, type = "as.factor")
  
  
  
  
  #rankeo todas
  prefix <- "r_"
  mis_variables<-names(base)
  for (var in mis_variables) {
    if(is.numeric(base[,get(var)])){
      base[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
      
    }
  }
  
  beep(sound = 1, expr = NULL)
  return(base)
}

dtrain_m<-aplicarCambios(dtrain)







#### optimizaci�n bayesiana ####

in_training <- caret::createDataPartition(dtrain_m$clase_binaria,
                                          p = 0.70, list = FALSE)
dtrain  <-  dtrain_m[in_training, ]
dtest   <-  dtrain_m[-in_training, ]

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


set.seed(semillas[1])
obj_fun_md_ms <- function(x) {
  experimento_rpart_completo(dtrain, semillas
                             , md = x$maxdepth
                             , ms = x$minsplit
                             , cp = -1
                             , mb = floor(x$minbucket*x$minsplit))
}

require(caret)
require(smoof)
require("mlrMBO")

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms,
  par.set = makeParamSet(
    makeIntegerParam("maxdepth",  lower = 4L, upper = 20L),
    makeIntegerParam("minsplit",  lower = 1L, upper = 500L),
    #makeNumericParam("cp",lower = -1, upper = 1),
    makeNumericParam("minbucket",lower = 0L, upper = 1)
  ),
  # noisy = TRUE,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 50L)
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
  # sacar par�metro opt.focussearch.points en pr�ximas ejecuciones
  opt.focussearch.points = 20
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)

saveRDS(run_md_ms, paste0("../bayesianas","_rankYmulti",".rds"))
remove(run_md_ms,ctrl,surr_km,dtrain,dtest)
gc()
#### prueba con hiperparametros bayesianos ####

ms=349
mb=0.0973
md=7
modeloTodas  <- rpart(formula=   "clase_binaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                      data=      dtrain_m,  #los datos donde voy a entrenar
                      xval=      0,
                      cp=       -1,   #esto significa no limitar la complejidad de los splits
                      minsplit=  ms,     #minima cantidad de registros para que se haga el split
                      minbucket= mb*ms,     #tamaño minimo de una hoja
                      maxdepth=  md )    #profundidad maxima del arbol

as.data.frame(modeloTodas$variable.importance)->importancia
rownames(importancia)->importancia$variable

write.csv(importancia, paste0("../importancia","_rankYmulti",".csv"), row.names = FALSE)

#grafico el arbol
#prp(modeloTodas, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
# Medimos cuanto tarda nuestro modelo en ajustar
resultados<-c()
for (semilla in semillas) {
  set.seed(semilla)
  
  in_training <- caret::createDataPartition(dtrain_m$clase_binaria,
                                            p = 0.70, list = FALSE)
  dtrain2  <-  dtrain_m[in_training, ]
  dtest   <-  dtrain_m[-in_training, ]
  
  pred_training <- predict(modeloTodas, dtrain2, type = "prob")
  pred_testing <- predict(modeloTodas, dtest, type = "prob")
  
  
  
  
  # Armamos una funci�n que nos calcule la ganancia, usando el punto de corte de
  # 0.025
  ganancia <- function(probabilidades, clase) {
    return(sum(
      (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
    )
  }
  
  gan<-ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
  # La ganancia en testing NORMALIZADA
  print(gan)
  
  c(resultados,gan)->resultados
}

print(mean(resultados))



#### ejecuto mejores parametror ####

library(readxl)
pruebas <- read_excel("../pruebas.xlsx")
excel<-data.table()
excel$descripcion<-paste0("Se aplic� ranking y multis, sin factores")
excel$ms<-ms
excel$mb<-mb
excel$md<-md
excel$ganancia<-mean(resultados)

pruebas2<-rbind(as.data.table(pruebas),excel)

writexl::write_xlsx(pruebas2,"../pruebas.xlsx")


#### aplico el modelo a los datos nuevos ####
dapply_m<-aplicarCambios(dapply)
prediccion  <- predict( object= modeloTodas,
                        newdata= dapply_m,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "evento"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#dir.create( "./exp/KA2002" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2002/K102_011.csv",
        sep=  "," )


