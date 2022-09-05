# base[ , tipo2 :=minversion2/cinversion2]
# 
# base[ , C_Seguros :=  cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]
# base[ , C_debitos :=  ccuenta_debitos_automaticos+ctarjeta_visa_debitos_automaticos+ctarjeta_master_debitos_automaticos]
# base[ , C_descuentos :=  ccajeros_propios_descuentos+ctarjeta_master_descuentos+ctarjeta_visa_descuentos]
# base[ , M_descuentos :=  mtarjeta_visa_descuentos+mtarjeta_master_descuentos+mcajeros_propios_descuentos]
# base[ , media_descuentos :=  M_descuentos/C_descuentos]
# 
# base[ , media_salarios :=mpayroll/cpayroll_trx]
# base[ , media_haberes :=mpayroll2/cpayroll2_trx]
# base[ , media_ingresos :=(mpayroll2+mpayroll)/(cpayroll2_trx+cpayroll_trx)]
# 
# base[ is.nan(media_ingresos), media_ingresos :=0]
# base[ is.nan(media_haberes), media_haberes :=0]
# base[ is.nan(media_salarios), media_salarios :=0]
# base[ is.nan(media_descuentos), media_descuentos :=0]
# base[ is.nan(tipo2), tipo2 :=0]