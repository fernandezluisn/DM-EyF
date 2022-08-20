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


variables_factor <-c("cliente_vip", "internet", "tcuentas", "cdescubierto_preacordado")
#dataset[, variables_factor] <- lapply(dataset[, variables_factor], as.factor)

did_recode_columns(dataset, variables_factor, type = "as.factor")

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#### analisis variables ####
dtrain$foto_mes<-NULL

dapply$foto_mes<-NULL

# todas las variables
modeloTodas  <- rpart(formula=   "clase_binaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -1,   #esto significa no limitar la complejidad de los splits
                 minsplit=  13,     #minima cantidad de registros para que se haga el split
                 minbucket= 4,     #tamaño minimo de una hoja
                 maxdepth=  6 )    #profundidad maxima del arbol

modeloTodas$variable.importance

#grafico el arbol
prp(modeloTodas, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#aplico el modelo a los datos nuevos
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
        file= "./exp/KA2002/K102_006.csv",
        sep=  "," )


#### modelo final ####
#genero el modelo,  aqui se construye el arbol
# modelo  <- rpart(formula=   "clase_ternaria ~ ctrx_quarter+mcuentas_saldo+mcomisiones+ccomisiones_otras+cdescubierto_preacordado+mactivos_margen+mcuenta_corriente+mpasivos_margen+mrentabilidad+cliente_antiguedad+active_quarter",  #quiero predecir clase_ternaria a partir de el resto de las variables
#                  data=      dtrain,  #los datos donde voy a entrenar
#                  xval=      0,
#                  cp=       -0.3,   #esto significa no limitar la complejidad de los splits
#                  minsplit=  0,     #minima cantidad de registros para que se haga el split
#                  minbucket= 1,     #tamaño minimo de una hoja
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
