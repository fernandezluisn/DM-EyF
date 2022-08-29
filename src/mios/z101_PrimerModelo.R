#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/DM-EyF")  #Establezco el Working Directory
remove(list=ls())
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

did_recode_columns(dataset, variables_factor, type = "as.factor")

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#### analisis variables ####


#dtrain

hist(log(dtrain$mcuenta_corriente))

#dapply

#imputar media
dtrain2[is.na(M_descuentos) & !is.na(C_descuentos), .(M_descuentos,C_descuentos, mtarjeta_visa_descuentos, ctarjeta_visa_descuentos, mtarjeta_master_descuentos,ctarjeta_master_descuentos, mcajeros_propios_descuentos, ccajeros_propios_descuentos)]->vers

base<-dapply
aplicarCambios<-function(base){
  base$foto_mes<-NULL
  base$nulos<-apply(X = is.na(base), MARGIN = 1, FUN = sum)
  
  base$log_ctrx_quarter<-log(base$ctrx_quarter)
  
  base$moroso2<-paste0(base$Visa_delinquency, base$Master_delinquency)
  #base$cierre2<-paste0(base$Visa_status, base$Master_status)
  
  #base$cierres<-ifelse(base$Visa_status %in% c(6,7,9) & base$Master_status %in% c(6,7,9),1,0)
  

  variables_factor2 <-c(  "moroso2"
                        #,"cierre2"
                        #,"cierres"
  )
  base[ , tipo2 :=minversion2/cinversion2]
  
  base[ , C_Seguros :=  cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]
  base[ , C_debitos :=  ccuenta_debitos_automaticos+ctarjeta_visa_debitos_automaticos+ctarjeta_master_debitos_automaticos]
  base[ , C_descuentos :=  ccajeros_propios_descuentos+ctarjeta_master_descuentos+ctarjeta_visa_descuentos]
  base[ , M_descuentos :=  mtarjeta_visa_descuentos+mtarjeta_master_descuentos+mcajeros_propios_descuentos]
  base[ , media_descuentos :=  M_descuentos/C_descuentos]
  
  base[ , media_salarios :=mpayroll/cpayroll_trx]
  base[ , media_haberes :=mpayroll2/cpayroll2_trx]
  base[ , media_ingresos :=(mpayroll2+mpayroll)/(cpayroll2_trx+cpayroll_trx)]
  
  base[ is.nan(media_ingresos), media_ingresos :=0]
  base[ is.nan(media_haberes), media_haberes :=0]
  base[ is.nan(media_salarios), media_salarios :=0]
  base[ is.nan(media_descuentos), media_descuentos :=0]
  base[ is.nan(tipo2), tipo2 :=0]
  
  did_recode_columns(base, variables_factor2, type = "as.factor")
  
  return(base)
}

dtrain_m<-aplicarCambios(dtrain)

dapply_m<-aplicarCambios(dapply)
#### generación del modelo ####

#readRDS("bayesianas.rds")->bayesianas
#print(bayesianas)
# todas las variables
modeloTodas  <- rpart(formula=   "clase_binaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain_m,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -1,   #esto significa no limitar la complejidad de los splits
                 minsplit=  13,     #minima cantidad de registros para que se haga el split
                 minbucket= 4,     #tamaÃ±o minimo de una hoja
                 maxdepth=  6 )    #profundidad maxima del arbol

as.data.frame(modeloTodas$variable.importance)->importancia
rownames(importancia)->importancia$variable
#grafico el arbol
#prp(modeloTodas, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)

#### prueba ####

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
  
  
  
  
  # Armamos una función que nos calcule la ganancia, usando el punto de corte de
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

library(readxl)
pruebas <- read_excel("pruebas.xlsx")
excel<-data.table()
excel$descripcion<-"Se hicieron cruces paste0 de tarjetas"
excel$ms<-13
excel$mb<-4
excel$md<-6
excel$ganancia<-mean(resultados)

pruebas2<-rbind(as.data.table(pruebas),excel)

writexl::write_xlsx(pruebas2,"pruebas.xlsx")


#### aplico el modelo a los datos nuevos ####
prediccion  <- predict( object= modeloTodas,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "evento"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#dir.create( "./exp/KA2002" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2002/K102_009.csv",
        sep=  "," )


#### modelo final ####
#genero el modelo,  aqui se construye el arbol
# modelo  <- rpart(formula=   "clase_ternaria ~ ctrx_quarter+mcuentas_saldo+mcomisiones+ccomisiones_otras+cdescubierto_preacordado+mactivos_margen+mcuenta_corriente+mpasivos_margen+mrentabilidad+cliente_antiguedad+active_quarter",  #quiero predecir clase_ternaria a partir de el resto de las variables
#                  data=      dtrain,  #los datos donde voy a entrenar
#                  xval=      0,
#                  cp=       -0.3,   #esto significa no limitar la complejidad de los splits
#                  minsplit=  0,     #minima cantidad de registros para que se haga el split
#                  minbucket= 1,     #tamaÃ±o minimo de una hoja
#                  maxdepth=  6 )    #profundidad maxima del arbol
# 
# modelo$variable.importance

#grafico el arbol
prp(modeloTodas, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modeloTodas,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
# dir.create( "./exp/" )
# dir.create( "./exp/KA2001" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2001/K101_004.csv",
        sep=  "," )

table(dapply$Predicted)
