library(xlsx)
library(openxlsx)
library(dplyr)


###################################################################################################################################
# Note: before using this code, run getData.R since all the developed functions are there.
###################################################################################################################################


######################################################## ELECTORAL DATA - PREP 2018 ###############################################
presidencia <- openxlsx::read.xlsx ("Data/presidencia.xlsx", sheet = 1)
secciones <- xlsx::read.xlsx("Data/rango.xlsx", sheetName = 1, encoding = "UTF-8", colClasses = c("numeric", "character", "numeric", "numeric", "character", "numeric", "numeric", "numeric"))
secciones$mpio = as.character(secciones$mpio)

# Looking for missing values in "secciones" data set
secciones[!complete.cases(secciones),]
secciones <- na.omit(secciones)

# Obtaining muninicipalities from electoral sections data
presidencia_municipios = df_municipios(presidencia)

# Deleting 18 problematic polling stations in Chiapas. This stations represent less than 0.00001 percent of the total vote.
# These polling stations were registered in an electoral section that does not exist, this type of problems are common for electoral data in Oaxaca and Chiapas
presidencia_municipios = presidencia_municipios[!(presidencia_municipios$municipios == "ERROR"),]
nrow(presidencia_municipios[presidencia_municipios$municipios == "ERROR",])

# Converting  party votes into numeric class
cols = c(13:39)   
presidencia_municipios[,cols] = apply(presidencia_municipios[,cols], 2, function(x) as.numeric((x)))

# Aggregating (collapsing in Stata) votes per municipality
votos_municipio <- aggregate(cbind(PAN, PRI, PRD, PVEM, PT, MOVIMIENTO.CIUDADANO, NUEVA.ALIANZA, MORENA, ENCUENTRO.SOCIAL, PAN_PRD_MC, PAN_PRD, PAN_MC, PRD_MC, PRI_PVEM_NA, PRI_PVEM, PRI_NA, PVEM_NA, PT_MORENA_PES, PT_MORENA, PT_PES, MORENA_PES, CAND_IND_01, CAND_IND_02, CNR, VN, TOTAL_VOTOS_CALCULADOS, LISTA_NOMINAL_CASILLA) ~ ID_ESTADO + NOMBRE_ESTADO + municipios, data = presidencia_municipios, sum, na.rm=TRUE, na.action=NULL)
attach(votos_municipio)
votos_municipio <- votos_municipio[order(NOMBRE_ESTADO, municipios),]
detach(votos_municipio)

# Obtaining coalition data, vote percentages and electoral turnout from present electoral data
votos_municipio = votacion2018(votos_municipio)

# Removing past data
#rm(presidencia)
#rm(presidencia_municipios)


######################################################### CONAPO CONTROL DATA ###################################
conapo <- xlsx::read.xlsx("Data/conapo.xlsx", sheetName = 1, encoding = "UTF-8", colClasses = c("character", "character", "numeric", "character", rep("numeric", 19)))
attach(conapo)
conapo <- conapo[order(CVE_ENT, CVE_MUN),]
detach(conapo)
conapo_f = subset(conapo, select = -c(CVE_ENT, ENT, VP, OVSD, OVSDSE, IND0A100, LUGAR_EST, AÑO))

# The municipal INEGI code is assigned to electoral data
votos_clave <- df_claves_mun(votos_municipio, auditoria = 0) 
# Complete match, except for the new municipalities Puerto Madero, Quintana Roo and Simón Bolívar, Durango.
votos_clave <- na.omit(votos_clave)   # We drop Puerto Madera and Simon Bolivar

votos_clave_f = subset(votos_clave, select = -c(ANO, municipios))

votos_conapo <- merge(votos_clave_f, conapo_f, by.x = c("clave_mun"), by.y = c("CVE_MUN"), all.x = TRUE)

#rm(conapo_f)
#rm(votos_clave)
#rm(votos_clave_f)
#rm(votos_municipio)

########################################################## CORRUPTION DATA ##################################################
recuperaciones <- xlsx::read.xlsx("Data/recuperaciones.xlsx", sheetName = 1, encoding = "UTF-8", colClasses = c("numeric", rep("character", 5), "numeric", rep("character", 6), rep("numeric", 2)))
correctivas_municipal <- xlsx::read.xlsx("Data/AccionesCorrectivas1516.xlsx", sheetName = 2, encoding = "UTF-8", colClasses = c("numeric", rep("character", 5), "numeric", rep("character", 6), rep("numeric", 2)))
correctivas_estmun <- xlsx::read.xlsx("Data/AccionesCorrectivas1516.xlsx", sheetName = 3, encoding = "UTF-8", colClasses = c("numeric", rep("character", 5), "numeric", rep("character", 6), rep("numeric", 2)))

correctivas <- correctivas(correctivas_estmun, correctivas_municipal)
correctivas_clave <- df_claves_mun(correctivas, auditoria = 1)
# Total match

recuperaciones_agg = aggregate(cbind(Por.Recuperar.o.Aclarar) ~ Entidad.Federativa + Ente.Fiscalizado, data = recuperaciones, sum)
attach(recuperaciones_agg)
recuperaciones_agg <- recuperaciones_agg[order(Entidad.Federativa, Ente.Fiscalizado),]
detach(recuperaciones_agg)
recuperaciones_clave <- df_claves_mun(recuperaciones_agg, auditoria = 1)
# Total match

recuperaciones_f = subset(recuperaciones_clave, select = -c(Entidad.Federativa, Ente.Fiscalizado))
colnames(recuperaciones_f)[1] <- "recuperaciones"

correctivas_f = subset(correctivas_clave, select = -c(Entidad.Federativa, Ente.Fiscalizado))

votos_corr <- merge(votos_conapo, correctivas_f, by.x = c("clave_mun"), by.y = c("clave_mun"), all.x = T)

votos_rec <- merge(votos_corr, recuperaciones_f, by.x = c("clave_mun"), by.y = c("clave_mun"), all.x = T)

# Para dar valor de 0 cuando un municipio no tuvo una acción correctiva
for (i in 1:length(votos_rec$clave_mun)){
  if (is.na(votos_rec$correctivas_mun[i])){
    votos_rec$correctivas_mun[i] = 0
    votos_rec$corr[i] = 0
  }
}

# Para dar valor de 0 cuando un municipio no tuvo recuperaciones
for (i in 1:length(votos_rec$recuperaciones)){
  if (is.na(votos_rec$recuperaciones[i])){
    votos_rec$recuperaciones[i] = 0
  }
}

#rm(votos_corr)
#rm(votos_conapo)
#rm(recuperaciones)
#rm(recuperaciones_agg)
#rm(recuperaciones_clave)
#rm(recuperaciones_f)
#rm(correctivas)
#rm(correctivas_clave)
#rm(correctivas_estmun)
#rm(correctivas_municipal)
#rm(correctivas_f)


######################################## Anexando base de delitos ##########################################
delitos_df <- openxlsx::read.xlsx ("Data/delitos.xlsx", sheet = 1)
delitos_f = delitos(delitos_df)
delitos_f = subset(delitos_f, select = -Municipio)
votos_del <- merge(votos_rec, delitos_f, by.x = c("clave_mun"), by.y = c("Cve..Municipio"), all.x = T)

#rm(votos_rec)
#rm(delitos_df)
#rm(delitos_f)


################################### Anexando base carater´siticas municipales #############################
carac <- xlsx::read.xlsx("Data/base_V.xlsx", sheetName = 1, encoding = "UTF-8")
colnames(carac) = paste(colnames(carac), "2010", sep = "_")
carac_f = subset(carac, select = -year_2010)
carac_f = subset(carac_f, select = -auxyear_2010)
carac_f = subset(carac_f, select = -ptotal_2010)
carac_f = subset(carac_f, select = -ent_2010)
carac_f = subset(carac_f, select = -mun_2010)
carac_f = subset(carac_f, select = -nom_mun_2010)

votos_carac <- merge(votos_del, carac_f, by.x = c("clave_mun"), by.y = c("id_2010"), all.x = T)

#rm(carac_f)
#rm(votos_del)


################################################# 2012 PRESIDENTIAL DATA #######################################################
presidencia2012 <- openxlsx::read.xlsx ("Data/presidencia2012.xlsx", sheet = 1)
votos_municipio2012 <- aggregate(cbind(PAN, PRI, PRD, PVEM, PT, MC, NVA_ALIANZA, PRI_PVEM, PRD_PT_MC, PRD_PT, PRD_MC, PT_MC, Total_Votos, LISTA_NOMINAL) ~ Nombre_Estado + NOM_MPIO, data = presidencia2012, sum, na.rm=TRUE, na.action=NULL)
votos2012 = votacion2012(votos_municipio2012)
colnames(votos2012)[1] = "NOMBRE_ESTADO"
colnames(votos2012)[2] = "municipios"
attach(votos2012)
votos2012 <- votos2012[order(NOMBRE_ESTADO, municipios),]
detach(votos2012)
claves2012 = df_claves_mun(votos2012, auditoria = 2)
# Total match. Except for Simon Bolivar municipality
claves2012 <- na.omit(claves2012)

claves2012_f = claves2012[,-c(1:19)]
colnames(claves2012_f)[7]="ganador_2012"
colnames(claves2012_f)[6]="porc_ganador_2012" 

votos_pasados <- merge(votos_carac, claves2012_f, by.x = c("clave_mun"), by.y = c("clave_mun"), all.x = T)

votos_diff = diff(votos_pasados)

#rm(presidencia2012)
#rm(votos2012)
#rm(votos_pasados)
#rm(votos_municipio2012)
#rm(votos_carac)
#rm(claves2012)
#rm(claves2012_f)


######################################################### 2015 FEDERAL ELECTION DATA ########################################
diputados2015 <- openxlsx::read.xlsx ("Data/diputados2015.xlsx", sheet = 1)
cols = c(1,3:22)
diputados2015[,cols] = apply(diputados2015[,cols], 2, function(x) as.numeric((x)))
diputados2015_mun = df_municipios(diputados2015)

nrow(diputados2015_mun[diputados2015_mun$municipios == "ERROR",])
diputados2015_mun = diputados2015_mun[!(diputados2015_mun$municipios == "ERROR"),]

cols = c(4:22)
diputados2015_mun[,cols] = apply(diputados2015_mun[,cols], 2, function(x) as.numeric((x)))

votos_municipio_2015 <- aggregate(cbind(PAN, PRI, PRD, PVEM, PT, MOVIMIENTO_CIUDADANO, NUEVA_ALIANZA, MORENA, PARTIDO_HUMANISTA, ENCUENTRO_SOCIAL, PRI_PVEM, PRD_PT, CAND_IND_1, CAND_IND_2, TOTAL_CIUDADANOS_VOTARON, LISTA_NOMINAL) ~ ID_ESTADO + ESTADO + municipios, na.rm=TRUE, na.action=NULL, data = diputados2015_mun, sum)
attach(votos_municipio_2015)
votos_municipio_2015 <- votos_municipio_2015[order(ID_ESTADO, municipios),]
detach(votos_municipio_2015)
colnames(votos_municipio_2015)[2] = "NOMBRE_ESTADO"

votos_2015 <- votacion2015(votos_municipio_2015)

claves2015 = df_claves_mun(votos_2015, auditoria = 0)
# Total match. Except for Puerto Morelos and Simon Bolivar
#x = claves2015[which(is.na(claves2015$clave_mun)),]
#View(x)

claves2015_f2 = claves2015[,-c(1:22)]
claves2015_f = claves2015[,-c(1:22)]
colnames(claves2015_f) = paste(colnames(claves2015_f), "2015", sep = "_")
colnames(claves2015_f)[22]="clave_mun"

votos_dip <- merge(votos_diff, claves2015_f, by.x = c("clave_mun"), by.y = c("clave_mun"), all.x = T)

#rm(claves2015)
#rm(claves2015_f)
#rm(diputados2015)
#rm(diputados2015_mun)
#rm(votos_diff)
#rm(votos_municipio_2015)
#rm(votos_2015)


################################################## Who got audit? ##################################################
auditados <- xlsx::read.xlsx("Data/auditorias.xlsx", sheetName = 1, encoding = "UTF-8")
colnames(auditados)[1] = "Ente.Fiscalizado"
colnames(auditados)[2] = "Entidad.Federativa"

auditados_clave <- df_claves_mun(auditados, auditoria = 1)
# Match total (solo falta Puerto Morelos pero este municipio se creo en 2015)
auditados_clave_f = auditados_clave[,-c(1,2)]
auditados_clave_f[!complete.cases(auditados_clave_f),]
auditados_clave_f <- na.omit(auditados_clave_f)   # Se quita el NA (Puerto Morelos)

votos_audit <- merge(votos_dip, auditados_clave_f, by.x = c("clave_mun"), by.y = c("clave_mun"), all.x = T)

# Para dar valor de 0 cuando un municipio no fue auditado
for (i in 1:length(votos_audit$auditado)){
  if (is.na(votos_audit$auditado[i])){
    votos_audit$auditado[i] = 0
  }
}

#rm(auditados)
#rm(auditados_clave)
#rm(auditados_clave_f)
#rm(votos_dip)


################################################### CONEVAL data ##########################################
cohesion <- openxlsx::read.xlsx ("Data/cohesion_social.xlsx", sheet = 1)
cohesion_f = cohesion[,-2]
cols = 1:4
cohesion_f[,cols] = apply(cohesion_f[,cols], 2, function(x) as.numeric((x)))
votos_coneval <- merge(votos_audit, cohesion_f, by.x = c("clave_mun"), by.y = c("clave"), all.x = T)

#rm(cohesion)
#rm(cohesion_f)
#rm(votos_audit)


################################################ BASE LONG CON 2015 ###################################
votos_LONG <- merge(claves2015_f2, delitos_f, by.x = c("clave_mun"), by.y = c("Cve..Municipio"), all.x = T)
votos_LONG <- merge(votos_LONG, cohesion_f, by.x = c("clave_mun"), by.y = c("clave"), all.x = T)
votos_LONG <- merge(votos_LONG, auditados_clave_f, by.x = c("clave_mun"), by.y = c("clave_mun"), all.x = T)

# Para dar valor de 0 cuando un municipio no fue auditado
for (i in 1:length(votos_LONG$auditado)){
  if (is.na(votos_LONG$auditado[i])){
    votos_LONG$auditado[i] = 0
  }
}

votos_LONG <- merge(votos_LONG, correctivas_f, by.x = c("clave_mun"), by.y = c("clave_mun"), all.x = T)

votos_LONG <- merge(votos_LONG, recuperaciones_f, by.x = c("clave_mun"), by.y = c("clave_mun"), all.x = T)

# Para dar valor de 0 cuando un municipio no tuvo una acción correctiva
for (i in 1:length(votos_LONG$clave_mun)){
  if (is.na(votos_LONG$correctivas_mun[i])){
    votos_LONG$correctivas_mun[i] = 0
    votos_LONG$corr[i] = 0
  }
}

# Para dar valor de 0 cuando un municipio no tuvo recuperaciones
for (i in 1:length(votos_LONG$recuperaciones)){
  if (is.na(votos_LONG$recuperaciones[i])){
    votos_LONG$recuperaciones[i] = 0
  }
}

votos_LONG <- merge(votos_LONG, conapo_f, by.x = c("clave_mun"), by.y = c("CVE_MUN"), all.x = T)
votos_LONG <- merge(votos_LONG, carac_f, by.x = c("clave_mun"), by.y = c("id_2010"), all.x = T)
votos_LONG <- merge(votos_LONG, claves2015_f, by.x = c("clave_mun"), by.y = c("clave_mun"), all.x = T)

AÑO = rep(2015, nrow(votos_LONG))
t = rep(0, nrow(votos_LONG))
ID_ESTADO = rep(NA, nrow(votos_LONG))
NOMBRE_ESTADO = rep(NA, nrow(votos_LONG))
votos_LONG_f = cbind(ID_ESTADO, NOMBRE_ESTADO, votos_LONG, AÑO, t)

write.xlsx(votos_LONG_f, "baseLong.xlsx")

long2015 <- xlsx::read.xlsx("Data/baseLong2015.xlsx", sheetName = 1, encoding = "UTF-8")
long2018 <- xlsx::read.xlsx("Data/baseLong2018.xlsx", sheetName = 1, encoding = "UTF-8")

long = rbind(long2015, long2018)
write.xlsx(long, "baseLong.xlsx")

#################### Dando formato a base final WIDE ########
votos <- votos_coneval[,-c(4:25, 27:30)]

############## Imprimir datos
write.xlsx(votos_coneval, "baseDatos.xlsx")
