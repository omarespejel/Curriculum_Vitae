clasificador <- function(SECCION, edo, secciones, clave = 0){
  secciones_edo = secciones[which(secciones$cve_edo == edo), ]
  max = max(secciones_edo$fin)
  municipio = character()
  if(SECCION <= max){
    i = 1
    while (length(municipio) == 0){
      inicio = secciones_edo$inicio[i]
      fin = secciones_edo$fin[i]
      if ((SECCION >= inicio) & (SECCION <= fin)){
        municipio = as.character(secciones_edo$mpio[i])
        clave = as.character(secciones_edo$cve_mpio[i])
      }
      i = i + 1
    }
  }
  else{
    municipio = "ERROR"
    clave = "ERROR"
  }
  if(clave == 1){
    muni_clave = cbind(municipio, clave)
    return(muni_clave)
  }
  else{
    return(municipio)
  }
}


df_municipios <- function(df){
  municipios = character()
  for (i in 1:length(df$SECCION)){
    municipios[i] = clasificador(df$SECCION[i], df$ID_ESTADO[i], secciones)
  }
  df_municipios = cbind(df, municipios)
  return(df_municipios)
}


votacion2018 <- function(df, nivel = "presidencia"){
  if (nivel == "presidencia"){
    PRI_coal = df$PRI + df$PRI_PVEM_NA + df$PRI_PVEM + df$PRI_NA + df$PVEM + df$NUEVA.ALIANZA + df$PVEM_NA
    MORENA_coal = df$MORENA + df$PT_MORENA_PES + df$PT_MORENA + df$MORENA_PES + df$PT + df$ENCUENTRO.SOCIAL + df$PT_PES
    PANPRD_coal = df$PAN + df$PRD + df$MOVIMIENTO.CIUDADANO + df$PAN_PRD_MC + df$PAN_PRD + df$PAN_MC + df$PRD_MC
    
    porc_PRI_coal = (PRI_coal/df$LISTA_NOMINAL_CASILLA)*100
    porc_MORENA_coal = (MORENA_coal/df$LISTA_NOMINAL_CASILLA)*100
    porc_PANPRD_coal = (PANPRD_coal/df$LISTA_NOMINAL_CASILLA)*100
    porc_ind = (df$CAND_IND_02/df$LISTA_NOMINAL_CASILLA)*100
    concurrencia = (df$TOTAL_VOTOS_CALCULADOS/df$LISTA_NOMINAL_CASILLA)*100
    
    ANO = rep(2018, length(df$PRI))
    
    df_variables = cbind(ANO, df, PRI_coal, MORENA_coal, PANPRD_coal, porc_PRI_coal, porc_MORENA_coal, porc_PANPRD_coal, porc_ind, concurrencia)
    return(df_variables)
  } 
  
}


votacion2012 <- function(df){
  cols = 3:16
  df[,cols] = apply(df[,cols], 2, function(x) as.numeric((x)))
    
  PRI_coal = df$PRI + df$PRI_PVEM + df$PVEM
  PRD_coal = df$PRD +  df$PRD_PT_MC + df$PRD_PT + df$PRD_MC  + df$PT_MC + df$PT + df$MC
  PAN_coal = df$PAN
    
  porc_PRI_coal = (PRI_coal/df$LISTA_NOMINAL)*100
  porc_PRD_coal = (PRD_coal/df$LISTA_NOMINAL)*100
  porc_PAN_coal = (PAN_coal/df$LISTA_NOMINAL)*100
  porc_NA = (df$NVA_ALIANZA/df$LISTA_NOMINAL)*100
  concurrencia = (df$Total_Votos/df$LISTA_NOMINAL)*100
    
  df_variables = cbind(df, PRI_coal, PRD_coal, PAN_coal, porc_PRI_coal, porc_PRD_coal, porc_PAN_coal, porc_NA, concurrencia)
  colnames(df_variables) = paste(colnames(df_variables), "2012", sep = "_")
    
  df_variables$Nombre_Estado_2012 = as.character(df_variables$Nombre_Estado_2012)
    
  for (i in 1:length(df_variables$Nombre_Estado_2012)){
    if(df_variables$Nombre_Estado_2012[i] == "MICHOACAN"){
      df_variables$Nombre_Estado_2012[i] = "MICHOACÁN" 
    }
    if(df_variables$Nombre_Estado_2012[i] == "MEXICO"){
      df_variables$Nombre_Estado_2012[i] = "MÉXICO" 
    }
    if(df_variables$Nombre_Estado_2012[i] == "DISTRITO FEDERAL"){
      df_variables$Nombre_Estado_2012[i] = "CIUDAD DE MÉXICO" 
    }
    if(df_variables$Nombre_Estado_2012[i] == "NUEVO LEON"){
      df_variables$Nombre_Estado_2012[i] = "NUEVO LEÓN" 
    }
    if(df_variables$Nombre_Estado_2012[i] == "QUERETARO"){
      df_variables$Nombre_Estado_2012[i] = "QUERÉTARO"
    }
    if(df_variables$Nombre_Estado_2012[i] == "SAN LUIS POTOSI"){
      df_variables$Nombre_Estado_2012[i] = "SAN LUIS POTOSÍ" 
    }
    if(df_variables$Nombre_Estado_2012[i] == "YUCATAN"){
      df_variables$Nombre_Estado_2012[i] = "YUCATÁN" 
    }
  }
  df_variables <- na.omit(df_variables)
  ganador = character()
  gan_PRI_2012 = numeric()
  gan_PAN_2012 = numeric()
  gan_PRD_2012 = numeric()
  gan_NA_2012 = numeric()
  porc_ganador = numeric()
  
  df_variables_2 = subset(df_variables, select = porc_PRI_coal_2012:porc_NA_2012)
  j = 1
  for (i in 1:length(df_variables_2$porc_PRI_coal_2012)){
    gan = max(df_variables_2$porc_PRI_coal_2012[i], df_variables_2$porc_PRD_coal_2012[i], df_variables_2$porc_PAN_coal_2012[i], df_variables_2$porc_NA_2012[i])
    porc_ganador[i] = gan
    if(df_variables_2[i,j] == gan){ #Es ganador el PRI
      ganador[i] = 1
      gan_PRI_2012[i] = 1
      gan_PRD_2012[i] = 0
      gan_PAN_2012[i] = 0
      gan_NA_2012[i] = 0
    } else if (df_variables_2[i,j + 1] == gan){ #Es ganador PRD
      ganador[i] = 2
      gan_PRD_2012[i] = 1
      gan_PRI_2012[i] = 0
      gan_PAN_2012[i] = 0
      gan_NA_2012[i] = 0
    } else if (df_variables_2[i,j + 2] == gan){ #Es ganador PAN
      ganador[i] = 3
      gan_PAN_2012[i] = 1
      gan_PRI_2012[i] = 0
      gan_PRD_2012[i] = 0
      gan_NA_2012[i] = 0
    } else if (df_variables_2[i,j + 3] == gan){ #Es ganador NA
      ganador[i] = 4
      gan_NA_2012[i] = 1
      gan_PRI_2012[i] = 0
      gan_PRD_2012[i] = 0
      gan_PAN_2012[i] = 0
    } 
  }
  ganador = as.numeric(ganador)
  df_variables = cbind(df_variables, porc_ganador, ganador, gan_PRI_2012, gan_PAN_2012, gan_PRD_2012, gan_NA_2012)
  return(df_variables)
}


#Creating first dependent variables: differences in electoral turnout and winner party vote
diff  <- function(df, porc_ganador = "porc_ganador_2012"){
  
  diff_ganador = numeric()
  
  for(i in 1:length(df$porc_ganador)){
    if(is.na(df$ganador_2012[i])){ #no existe dicho valor
      diff_ganador[i] = NA 
    } else if (df$ganador_2012[i] == 2){ #gana PRD
      diff_ganador[i] = df$porc_ganador[i] - df$porc_PANPRD_coal[i]
    } else if (df$ganador_2012[i] == 3){ #gana PAN
      diff_ganador[i] = df$porc_ganador[i] - df$porc_PANPRD_coal[i]
    } else if (df$ganador_2012[i] == 4){ #gana Nueva Alianza
      diff_ganador[i] = df$porc_ganador[i] - df$porc_PRI_coal[i] 
    } else if (df$ganador_2012[i] == 1){ #gana PRI
      diff_ganador[i] = df$porc_ganador[i] - df$porc_PRI_coal[i]
    }  
  }
  diff_ganador_1218 = diff_ganador
  df_diff = cbind(df, diff_ganador_1218)

  return(df_diff)
}

# Por terminar
clasificador2 <- function(municipio, edo, secciones){
  #pliegos <- aggregate(cbind(Por.Recuperar.o.Aclarar, Pliego) ~ Ente.Fiscalizado + Entidad.Federativa, data = pliegos, sum)
  secciones_edo = secciones[which(secciones$cve_edo == edo), ]
  match = pmatch(tolower(municipio), tolower(secciones$mpio), dup = FALSE)
  
  return(distrito)
}

# Merger corrupción estatal
correctivas <- function(correctivas_estmun, correctivas_municipal){
  
  correctivas_est2 = aggregate(cbind(Acciones) ~ Entidad.Federativa, data = correctivas_estmun, sum)
  correctivas_mun1 = aggregate(cbind(Acciones) ~ Entidad.Federativa + Ente.Fiscalizado, data = correctivas_municipal, sum)
  colnames(correctivas_est2)[2] = "correctivas_estmun"
  colnames(correctivas_mun1)[3] = "correctivas_mun"
  corr = rep.int(1, times = length(correctivas_mun1$correctivas_mun))
  corr = as.numeric(corr)
  
  correctivas_mun1 = cbind(correctivas_mun1, corr)
  correctivas <- merge(correctivas_est2, correctivas_mun1, by = "Entidad.Federativa", all = TRUE)
  
  correctivas$Entidad.Federativa = tolower(correctivas$Entidad.Federativa)
  for (i in 1:length(correctivas$Entidad.Federativa)){
    if(correctivas$Entidad.Federativa[i] == "coahuila de zaragoza"){
      correctivas$Entidad.Federativa[i] = "coahuila"
    }
    if(correctivas$Entidad.Federativa[i] == "michoacán de ocampo"){
      correctivas$Entidad.Federativa[i] = "michoacán"
    }
    if(correctivas$Entidad.Federativa[i] == "veracruz ignacio de la llave"){
      correctivas$Entidad.Federativa[i] = "veracruz"
    }  
  }
  
  return(correctivas)
}

############# Función para igualar código de municipio por nombre de municipio #########################
# Se elige la base de datos Conapo como benchmark de los codigos municipales
codigo_municipio <- function(mun, edo, conapo, edo_char = 0){
  cve_ent = numeric()
  if (edo_char == 1){
    if (edo == "aguascalientes"){
      cve_ent = 1
    }
    if(edo == "baja california"){
      cve_ent = 2
    }
    if(edo == "baja california sur"){
      cve_ent = 3
    }
    if(edo == "campeche"){
      cve_ent = 4
    }
    if(edo == "chiapas"){
      cve_ent = 7
    }
    if(edo == "chihuahua"){
      cve_ent = 8
    }
    if(edo == "ciudad de méxico"){
      cve_ent = 9
    }
    if(edo == "coahuila" | edo == "coahuila de zaragoza"){
      cve_ent = 5
    }
    if(edo == "colima"){
      cve_ent = 6
    }
    if(edo == "durango"){
      cve_ent = 10
    }
    if(edo == "guanajuato"){
      cve_ent = 11
    }
    if(edo == "guerrero"){
      cve_ent = 12
    }
    if(edo == "hidalgo"){
      cve_ent = 13
    }
    if(edo == "jalisco"){
      cve_ent = 14
    }
    if(edo == "méxico"){
      cve_ent = 15
    }
    if(edo == "michoacán"){
      cve_ent = 16
    }
    if(edo == "morelos"){
      cve_ent = 17
    }
    if(edo == "nayarit"){
      cve_ent = 18
    }
    if(edo == "nuevo león"){
      cve_ent = 19
    }
    if(edo == "oaxaca"){
      cve_ent = 20
    }
    if(edo == "puebla"){
      cve_ent = 21
    }
    if(edo == "querétaro"){
      cve_ent = 22
    }
    if(edo == "quintana roo"){
      cve_ent = 23
    }
    if(edo == "san luis potosí"){
      cve_ent = 24
    }
    if(edo == "sinaloa"){
      cve_ent = 25
    }
    if(edo == "sonora"){
      cve_ent = 26
    }
    if(edo == "tabasco"){
      cve_ent = 27
    }
    if(edo == "tamaulipas"){
      cve_ent = 28
    }
    if(edo == "tlaxcala"){
      cve_ent = 29
    }
    if(edo == "veracruz"){
      cve_ent = 30
    }
    if(edo == "yucatán"){
      cve_ent = 31
    }
    if(edo == "zacatecas"){
      cve_ent = 32
    }
    edo = cve_ent

    mun = iconv(mun, from = "UTF-8", to = "ASCII//TRANSLIT")
  }

  conapo_edo = conapo[which(conapo$CVE_ENT == edo), ]
  conapo_edo$MUN = as.character(conapo_edo$MUN)
  
  for (i in 1:length(conapo_edo$MUN)){
    x = conapo_edo$MUN[i]
    conapo_edo$MUN[i] = iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  }
  
  match = pmatch(tolower(mun), tolower(conapo_edo$MUN), dup = FALSE)
  clave = conapo_edo$CVE_MUN[match]
  return(clave)
}

############## Función para dar codigo a municipios por base ######
df_claves_mun <- function(df, auditoria = 0){
  clave_mun = numeric()
  
  if(auditoria == 1){
    df$Entidad.Federativa = tolower(df$Entidad.Federativa)
    df$Ente.Fiscalizado = as.character(df$Ente.Fiscalizado)
    df$Entidad.Federativa = as.character(df$Entidad.Federativa)
    for (i in 1:length(df$Ente.Fiscalizado)){
      if (df$Ente.Fiscalizado[i] == "Tlaquepaque"){
        df$Ente.Fiscalizado[i] = "San Pedro Tlaquepaque"
      }
      if (df$Ente.Fiscalizado[i] == "Nayar"){
        df$Ente.Fiscalizado[i] = "Del Nayar"
      }
      if (df$Ente.Fiscalizado[i] == "la Heroica Ciudad de Huajuapan de León"){
        df$Ente.Fiscalizado[i] = "Heroica Ciudad de Huajuapan de León"
      }
      clave_mun[i] = codigo_municipio(df$Ente.Fiscalizado[i], df$Entidad.Federativa[i], conapo, edo_char = 1)
      
      if (df$Ente.Fiscalizado[i] == "Heroica Ciudad de Huajuapan de León"){
        clave_mun[i] = 20039
      }
    }
  } else if (auditoria == 0) {
    df$municipios = as.character(df$municipios)
    df$municipios = trimws(df$municipios)
    for (i in 1:length(df$municipios)){
      clave_mun[i] = codigo_municipio(df$municipios[i], df$ID_ESTADO[i], conapo)
      
      if (df$NOMBRE_ESTADO[i] == "CHIAPAS" & df$municipios[i] == "VILLACOMALTITLAN"){
        clave_mun[i] = 7071
      } else if (df$NOMBRE_ESTADO[i] == "COAHUILA" & df$municipios[i] == "ACU?A"){
        clave_mun[i] = 5002
      } else if (df$NOMBRE_ESTADO[i] == "COAHUILA" & df$municipios[i] == "CASTA?OS"){
        clave_mun[i] = 5006
      } else if (df$NOMBRE_ESTADO[i] == "COAHUILA" & df$municipios[i] == "CUATROCIENEGAS"){
        clave_mun[i] = 5006
      } else if (df$NOMBRE_ESTADO[i] == "DURANGO" & df$municipios[i] == "PE?ON BLANCO"){
        clave_mun[i] = 10021
      } else if (df$NOMBRE_ESTADO[i] == "JALISCO" & df$municipios[i] == "BOLA?OS"){
        clave_mun[i] = 14019
      } else if (df$NOMBRE_ESTADO[i] == "JALISCO" & df$municipios[i] == "CA?ADAS DE OBREGON"){
        clave_mun[i] = 14117
      } else if (df$NOMBRE_ESTADO[i] == "JALISCO" & df$municipios[i] == "SAN MARTIN DE BOLA?OS"){
        clave_mun[i] = 14076
      } else if (df$NOMBRE_ESTADO[i] == "JALISCO" & df$municipios[i] == "TLAJOMULCO DE ZU?IGA"){
        clave_mun[i] = 14097
      } else if ((df$NOMBRE_ESTADO[i] == "MICHOACÁN" | df$ID_ESTADO[i] == 16) & df$municipios[i] == "BRISE?AS"){
        clave_mun[i] = 16011
      } else if ((df$NOMBRE_ESTADO[i] == "MICHOACÁN" | df$ID_ESTADO[i] == 16) & df$municipios[i] == "TING?INDIN"){
        clave_mun[i] = 16091
      } else if ((df$NOMBRE_ESTADO[i] == "MÉXICO" | df$ID_ESTADO[i] == 15) & df$municipios[i] == "ACAMBAY DE RUIZ CASTA?EDA"){
        clave_mun[i] = 15001
      } else if (df$NOMBRE_ESTADO[i] == "NAYARIT" & df$municipios[i] == "AMATLAN DE CA?AS"){
        clave_mun[i] = 18003
      } else if ((df$NOMBRE_ESTADO[i] == "NUEVO LEÓN" | df$ID_ESTADO[i] == 19) & df$municipios[i] == "CARMEN"){
        clave_mun[i] = 19010
      } else if ((df$NOMBRE_ESTADO[i] == "NUEVO LEÓN" | df$ID_ESTADO[i] == 19) & df$municipios[i] == "DR. ARROYO"){
        clave_mun[i] = 19014
      } else if ((df$NOMBRE_ESTADO[i] == "NUEVO LEÓN" | df$ID_ESTADO[i] == 19) & df$municipios[i] == "DR. COSS"){
        clave_mun[i] = 19015
      } else if ((df$NOMBRE_ESTADO[i] == "NUEVO LEÓN" | df$ID_ESTADO[i] == 19) & df$municipios[i] == "DR. GONZALEZ"){
        clave_mun[i] = 19016
      } else if ((df$NOMBRE_ESTADO[i] == "NUEVO LEÓN" | df$ID_ESTADO[i] == 19) & df$municipios[i] == "GRAL. BRAVO"){
        clave_mun[i] = 19020
      } else if ((df$NOMBRE_ESTADO[i] == "NUEVO LEÓN" | df$ID_ESTADO[i] == 19) & df$municipios[i] == "GRAL. ESCOBEDO"){
        clave_mun[i] = 19021
      } else if ((df$NOMBRE_ESTADO[i] == "NUEVO LEÓN" | df$ID_ESTADO[i] == 19) & df$municipios[i] == "GRAL. TERAN"){
        clave_mun[i] = 19022
      } else if ((df$NOMBRE_ESTADO[i] == "NUEVO LEÓN" | df$ID_ESTADO[i] == 19) & df$municipios[i] == "GRAL. TREVI?O"){
        clave_mun[i] = 19023
      } else if ((df$NOMBRE_ESTADO[i] == "NUEVO LEÓN" | df$ID_ESTADO[i] == 19) & df$municipios[i] == "GRAL. ZARAGOZA"){
        clave_mun[i] = 19024
      } else if ((df$NOMBRE_ESTADO[i] == "NUEVO LEÓN" | df$ID_ESTADO[i] == 19) & df$municipios[i] == "GRAL. ZUAZUA"){
        clave_mun[i] = 19025
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "H VILLA TEZOATLAN SEGURA Y LUNA CUNA IND OAX"){
        clave_mun[i] = 20549
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "LA COMPA?IA"){
        clave_mun[i] = 20017
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "MAGDALENA PE?ASCO"){
        clave_mun[i] = 20050
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "MATIAS ROMERO AVENDA?O"){
        clave_mun[i] = 20057
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "SAN ANDRES NUXI?O"){
        clave_mun[i] = 20094
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "SAN BARTOLOME YUCUA?E"){
        clave_mun[i] = 20119
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "SAN FRANCISCO NUXA?O"){
        clave_mun[i] = 20147
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "SAN JOSE DEL PE?ASCO"){
        clave_mun[i] = 20167
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "SAN JUAN ?UMI"){
        clave_mun[i] = 20210
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "SAN MATEO PE?ASCO"){
        clave_mun[i] = 20252
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "SAN MATEO PI?AS"){
        clave_mun[i] = 20253
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "SAN VICENTE NU?U"){
        clave_mun[i] = 20536
      } else if (df$NOMBRE_ESTADO[i] == "OAXACA" & df$municipios[i] == "SANTA MARIA PE?OLES"){
        clave_mun[i] = 20426
      } else if (df$NOMBRE_ESTADO[i] == "PUEBLA" & df$municipios[i] == "CA?ADA MORELOS"){
        clave_mun[i] = 21099
      } else if (df$NOMBRE_ESTADO[i] == "PUEBLA" & df$municipios[i] == "SAN ANTONIO CA?ADA"){
        clave_mun[i] = 21120
      } else if ((df$NOMBRE_ESTADO[i] == "QUERÉTARO" | df$ID_ESTADO[i] == 22) & df$municipios[i] == "PE?AMILLER"){
        clave_mun[i] = 22013
      } else if (df$NOMBRE_ESTADO[i] == "SONORA" & df$municipios[i] == "PUERTO PE?ASCO"){
        clave_mun[i] = 26048
      } else if (df$NOMBRE_ESTADO[i] == "TAMAULIPAS" & df$municipios[i] == "G?EMEZ"){
        clave_mun[i] = 28013
      } else if (df$NOMBRE_ESTADO[i] == "TLAXCALA" & df$municipios[i] == "ESPA?ITA"){
        clave_mun[i] = 29012
      } else if (df$NOMBRE_ESTADO[i] == "TLAXCALA" & df$municipios[i] == "MU?OZ DE DOMINGO ARENAS"){
        clave_mun[i] = 29011
      } else if (df$NOMBRE_ESTADO[i] == "TLAXCALA" & df$municipios[i] == "ZITLALTEPEC DE TRINIDAD SANCHEZ SANTOS"){
        clave_mun[i] = 29037
      } else if (df$NOMBRE_ESTADO[i] == "ZACATECAS" & df$municipios[i] == "CA?ITAS DE FELIPE PESCADOR"){
        clave_mun[i] = 32006
      }
    }  
  } else if (auditoria == 2) {
    df$municipios = as.character(df$municipios)
    df$municipios = trimws(df$municipios)
    df$NOMBRE_ESTADO = tolower(df$NOMBRE_ESTADO)
    for (i in 1:length(df$municipios)){
      clave_mun[i] = codigo_municipio(df$municipios[i], df$NOMBRE_ESTADO[i], conapo, edo_char = 1)
      
      if (df$NOMBRE_ESTADO[i] == "chiapas" & df$municipios[i] == "VILLACOMALTITLAN"){
        clave_mun[i] = 7071
      } else if (df$NOMBRE_ESTADO[i] == "coahuila" & df$municipios[i] == "ACUÐA"){
        clave_mun[i] = 5002
      } else if (df$NOMBRE_ESTADO[i] == "coahuila" & df$municipios[i] == "CASTAÐOS"){
        clave_mun[i] = 5006
      } else if (df$NOMBRE_ESTADO[i] == "coahuila" & df$municipios[i] == "CUATROCIENEGAS"){
        clave_mun[i] = 5006
      } else if (df$NOMBRE_ESTADO[i] == "durango" & df$municipios[i] == "PEÐON BLANCO"){
        clave_mun[i] = 10021
      } else if (df$NOMBRE_ESTADO[i] == "jalisco" & df$municipios[i] == "BOLAÐOS"){
        clave_mun[i] = 14019
      } else if (df$NOMBRE_ESTADO[i] == "jalisco" & df$municipios[i] == "CAÐADAS DE OBREGON"){
        clave_mun[i] = 14117
      } else if (df$NOMBRE_ESTADO[i] == "jalisco" & df$municipios[i] == "SAN MARTIN DE BOLAÐOS"){
        clave_mun[i] = 14076
      } else if (df$NOMBRE_ESTADO[i] == "jalisco" & df$municipios[i] == "TLAJOMULCO DE ZUÐIGA"){
        clave_mun[i] = 14097
      } else if (df$NOMBRE_ESTADO[i] == "jalisco" & df$municipios[i] == "TLAQUEPAQUE"){
        clave_mun[i] = 14098
      } else if (df$NOMBRE_ESTADO[i] == "michoacán" & df$municipios[i] == "BRISEÐAS"){
        clave_mun[i] = 16011
      } else if (df$NOMBRE_ESTADO[i] == "michoacán" & df$municipios[i] == "TINGÐINDIN"){
        clave_mun[i] = 16091
      } else if (df$NOMBRE_ESTADO[i] == "méxico" & df$municipios[i] == "ACAMBAY DE RUIZ CASTAÐEDA"){
        clave_mun[i] = 15001
      } else if (df$NOMBRE_ESTADO[i] == "nayarit" & df$municipios[i] == "AMATLAN DE CAÐAS"){
        clave_mun[i] = 18003
      } else if (df$NOMBRE_ESTADO[i] == "nuevo león" & df$municipios[i] == "CARMEN"){
        clave_mun[i] = 19010
      } else if (df$NOMBRE_ESTADO[i] == "nuevo león"  & df$municipios[i] == "DR. ARROYO"){
        clave_mun[i] = 19014
      } else if (df$NOMBRE_ESTADO[i] == "nuevo león"  & df$municipios[i] == "DR. COSS"){
        clave_mun[i] = 19015
      } else if (df$NOMBRE_ESTADO[i] == "nuevo león"  & df$municipios[i] == "DR. GONZALEZ"){
        clave_mun[i] = 19016
      } else if (df$NOMBRE_ESTADO[i] == "nuevo león"  & df$municipios[i] == "GRAL. BRAVO"){
        clave_mun[i] = 19020
      } else if (df$NOMBRE_ESTADO[i] == "nuevo león"  & df$municipios[i] == "GRAL. ESCOBEDO"){
        clave_mun[i] = 19021
      } else if (df$NOMBRE_ESTADO[i] == "nuevo león" & df$municipios[i] == "GRAL. TERAN"){
        clave_mun[i] = 19022
      } else if (df$NOMBRE_ESTADO[i] == "nuevo león" & df$municipios[i] == "GRAL. TREVIÐO"){
        clave_mun[i] = 19023
      } else if (df$NOMBRE_ESTADO[i] == "nuevo león" & df$municipios[i] == "GRAL. ZARAGOZA"){
        clave_mun[i] = 19024
      } else if (df$NOMBRE_ESTADO[i] == "nuevo león" & df$municipios[i] == "GRAL. ZUAZUA"){
        clave_mun[i] = 19025
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "H VILLA TEZOATLAN SEGURA Y LUNA CUNA IND OAX"){
        clave_mun[i] = 20549
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "LA COMPAÐIA"){
        clave_mun[i] = 20017
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "MAGDALENA PEÐASCO"){
        clave_mun[i] = 20050
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "MATIAS ROMERO AVENDAÐO"){
        clave_mun[i] = 20057
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "SAN ANDRES NUXIÐO"){
        clave_mun[i] = 20094
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "SAN BARTOLOME YUCUAÐE"){
        clave_mun[i] = 20119
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "SAN FRANCISCO NUXAÐO"){
        clave_mun[i] = 20147
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "SAN JOSE DEL PEÐASCO"){
        clave_mun[i] = 20167
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "SAN JUAN ÐUMI"){
        clave_mun[i] = 20210
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "SAN MATEO PEÐASCO"){
        clave_mun[i] = 20252
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "SAN MATEO PIÐAS"){
        clave_mun[i] = 20253
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "SAN VICENTE NUÐU"){
        clave_mun[i] = 20536
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "SANTA MARIA PEÐOLES"){
        clave_mun[i] = 20426
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "JUCHITAN DE ZARAGOZA"){
        clave_mun[i] = 20043
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "TEZOATLAN DE SEGURA Y LUNA"){
        clave_mun[i] = 20549
      } else if (df$NOMBRE_ESTADO[i] == "oaxaca" & df$municipios[i] == "ZAPOTITLAN DEL RIO"){
        clave_mun[i] = 20566
      } else if (df$NOMBRE_ESTADO[i] == "puebla" & df$municipios[i] == "CAÐADA MORELOS"){
        clave_mun[i] = 21099
      } else if (df$NOMBRE_ESTADO[i] == "puebla" & df$municipios[i] == "SAN ANTONIO CAÐADA"){
        clave_mun[i] = 21120
      } else if (df$NOMBRE_ESTADO[i] == "querétaro" & df$municipios[i] == "PEÐAMILLER"){
        clave_mun[i] = 22013
      } else if (df$NOMBRE_ESTADO[i] == "sonora" & df$municipios[i] == "PUERTO PEÐASCO"){
        clave_mun[i] = 26048
      } else if (df$NOMBRE_ESTADO[i] == "tamaulipas" & df$municipios[i] == "GÐEMEZ"){
        clave_mun[i] = 28013
      } else if (df$NOMBRE_ESTADO[i] == "tlaxcala" & df$municipios[i] == "ESPAÐITA"){
        clave_mun[i] = 29012
      } else if (df$NOMBRE_ESTADO[i] == "tlaxcala" & df$municipios[i] == "MUÐOZ DE DOMINGO ARENAS"){
        clave_mun[i] = 29011
      } else if (df$NOMBRE_ESTADO[i] == "tlaxcala" & df$municipios[i] == "ZITLALTEPEC DE TRINIDAD SANCHEZ SANTOS"){
        clave_mun[i] = 29037
      } else if (df$NOMBRE_ESTADO[i] == "tlaxcala" & df$municipios[i] == "ALTZAYANCA"){
        clave_mun[i] = 29004
      } else if (df$NOMBRE_ESTADO[i] == "tlaxcala" & df$municipios[i] == "YAUHQUEMECAN"){
        clave_mun[i] = 29043
      } else if (df$NOMBRE_ESTADO[i] == "zacatecas" & df$municipios[i] == "CAÐITAS DE FELIPE PESCADOR"){
        clave_mun[i] = 32006
      }
                 
    }
  }
  
  
  df_municipios = cbind(df, clave_mun)
  return(df_municipios)
}


############# Función para ordenar base de delitos #########
delitos <- function(delitos){
  delitos_homicidios = delitos[which(delitos$Tipo.de.delito == "Homicidio"), ]
  delitos_feminicidio = delitos[which(delitos$Tipo.de.delito == "Feminicidio"), ]
  delitos_robo = delitos[which(delitos$Tipo.de.delito == "Robo"), ]
  delitos_hfr = rbind(delitos_homicidios, delitos_feminicidio, delitos_robo)
  
  homicidios = aggregate(cbind(Total) ~ Cve..Municipio + Municipio, data = delitos_homicidios, sum)
  colnames(homicidios)[3] = "homicidios"
  feminicidios = aggregate(cbind(Total) ~ Cve..Municipio, data = delitos_feminicidio, sum)
  colnames(feminicidios)[2] = "feminicidios"
  robos = aggregate(cbind(Total) ~ Cve..Municipio, data = delitos_robo, sum)
  colnames(robos)[2] = "robos"
  hfr = aggregate(cbind(Total) ~ Cve..Municipio, data = delitos_hfr, sum)
  colnames(hfr)[2] = "hfr"
  delitos_total = aggregate(cbind(Total) ~ Cve..Municipio, data = delitos, sum)
  colnames(delitos_total)[2] = "delitos"
  
  merge1 <- merge(homicidios, feminicidios, by = "Cve..Municipio", all = TRUE)
  merge2 <- merge(merge1, robos, by = "Cve..Municipio", all = TRUE)
  merge3 <- merge(merge2, hfr, by = "Cve..Municipio", all = TRUE)
  delitos_df <- merge(merge3, delitos_total, by = "Cve..Municipio", all = TRUE)
  
  return(delitos_df)
}


votacion2015 <- function(df){
  error = numeric()
  
  for (i in 1:length(df$municipios)){
    if((df$PAN[i]==0 & df$PRI[i]==0) | df$TOTAL_CIUDADANOS_VOTARON[i] == 0){
      error[i] = 1
    } else {
      error[i] = 0
    }
  }
  
  df = df[which(error == 0),]
  
  PRI_coal = df$PRI + df$PRI_PVEM + df$PVEM
  PRD_coal = df$PRD + df$PT + df$PRD_PT
  ind = df$CAND_IND_1 + df$CAND_IND_2

  porc_PRI_coal = (PRI_coal/df$LISTA_NOMINAL)*100
  porc_PRD_coal = (PRD_coal/df$LISTA_NOMINAL)*100
  porc_ind = (ind/df$LISTA_NOMINAL)*100
  porc_PAN = (df$PAN/df$TOTAL_CIUDADANOS_VOTARON)*100
  porc_MC = (df$MOVIMIENTO_CIUDADANO/df$LISTA_NOMINAL)*100
  porc_NA = (df$NUEVA_ALIANZA/df$LISTA_NOMINAL)*100
  porc_MORENA = (df$MORENA/df$LISTA_NOMINAL)*100
  porc_PH = (df$PARTIDO_HUMANISTA/df$LISTA_NOMINAL)*100
  porc_ES = (df$ENCUENTRO_SOCIAL/df$LISTA_NOMINAL)*100
  
  concurrencia = (df$TOTAL_CIUDADANOS_VOTARON/df$LISTA_NOMINAL)*100
  
  df_variables = cbind(df, PRI_coal, PRD_coal, ind, porc_PRI_coal, porc_PRD_coal, porc_ind, porc_PAN, porc_MC, porc_NA, porc_MORENA, porc_PH, porc_ES, concurrencia)
  
  ganador = numeric()
  gan_PRI_2015 = numeric()
  gan_PAN_2015 = numeric()
  gan_PRD_2015 = numeric()
  gan_IND_2015 = numeric()
  gan_MC_2015 = numeric()
  gan_NA_2015 = numeric()
  gan_MORENA_2015 = numeric()
  gan_PH_2015 = numeric()
  gan_ES_2015 = numeric()
  porc_ganador = numeric()
  df_variables_2 = subset(df_variables, select = porc_PRI_coal:porc_ES)
  j = 1
  for (i in 1:length(df_variables_2$porc_PRI_coal)){
    gan = max(df_variables_2$porc_PRI_coal[i], df_variables_2$porc_PRD_coal[i], df_variables_2$porc_ind[i], df_variables_2$porc_PAN[i], df_variables_2$porc_MC[i], df_variables_2$porc_NA[i], df_variables_2$porc_MORENA[i], df_variables_2$porc_PH[i], df_variables_2$porc_ES[i])
    porc_ganador[i] = gan
    if(df_variables_2[i,j] == gan){ #Es ganador el PRI
      ganador[i] = 1
      gan_PRI_2015[i] = 1
      gan_PAN_2015[i] = 0
      gan_PRD_2015[i] = 0
      gan_IND_2015[i] = 0
      gan_MC_2015[i] = 0
      gan_NA_2015[i] = 0
      gan_MORENA_2015[i] = 0
      gan_PH_2015[i] = 0
      gan_ES_2015[i] = 0
    } else if (df_variables_2[i,j + 1] == gan){ #Es ganador PRD
      ganador[i] = 2
      gan_PRI_2015[i] = 0
      gan_PAN_2015[i] = 0
      gan_PRD_2015[i] = 1
      gan_IND_2015[i] = 0
      gan_MC_2015[i] = 0
      gan_NA_2015[i] = 0
      gan_MORENA_2015[i] = 0
      gan_PH_2015[i] = 0
      gan_ES_2015[i] = 0
    } else if (df_variables_2[i,j + 2] == gan){ #Es ganador IND
      ganador[i] = 4
      gan_PRI_2015[i] = 0
      gan_PAN_2015[i] = 0
      gan_PRD_2015[i] = 0
      gan_IND_2015[i] = 1
      gan_MC_2015[i] = 0
      gan_NA_2015[i] = 0
      gan_MORENA_2015[i] = 0
      gan_PH_2015[i] = 0
      gan_ES_2015[i] = 0
    } else if (df_variables_2[i,j + 3] == gan){ #Es ganador PAN
      ganador[i] = 3
      gan_PRI_2015[i] = 0
      gan_PAN_2015[i] = 1
      gan_PRD_2015[i] = 0
      gan_IND_2015[i] = 0
      gan_MC_2015[i] = 0
      gan_NA_2015[i] = 0
      gan_MORENA_2015[i] = 0
      gan_PH_2015[i] = 0
      gan_ES_2015[i] = 0
    } else if (df_variables_2[i,j + 4] == gan){ #Es ganador MC
      ganador[i] = 5
      gan_PRI_2015[i] = 0
      gan_PAN_2015[i] = 0
      gan_PRD_2015[i] = 0
      gan_IND_2015[i] = 0
      gan_MC_2015[i] = 1
      gan_NA_2015[i] = 0
      gan_MORENA_2015[i] = 0
      gan_PH_2015[i] = 0
      gan_ES_2015[i] = 0
    } else if (df_variables_2[i,j + 5] == gan){ #Es ganador NA
      ganador[i] = 6
      gan_PRI_2015[i] = 0
      gan_PAN_2015[i] = 0
      gan_PRD_2015[i] = 0
      gan_IND_2015[i] = 0
      gan_MC_2015[i] = 0
      gan_NA_2015[i] = 1
      gan_MORENA_2015[i] = 0
      gan_PH_2015[i] = 0
      gan_ES_2015[i] = 0
    } else if (df_variables_2[i,j + 6] == gan){ #Es ganador MORENA
      ganador[i] = 7
      gan_PRI_2015[i] = 0
      gan_PAN_2015[i] = 0
      gan_PRD_2015[i] = 0
      gan_IND_2015[i] = 0
      gan_MC_2015[i] = 0
      gan_NA_2015[i] = 0
      gan_MORENA_2015[i] = 1
      gan_PH_2015[i] = 0
      gan_ES_2015[i] = 0
    } else if (df_variables_2[i,j + 7] == gan){ #Es ganador PH
      ganador[i] = 8
      gan_PRI_2015[i] = 0
      gan_PAN_2015[i] = 0
      gan_PRD_2015[i] = 0
      gan_IND_2015[i] = 0
      gan_MC_2015[i] = 0
      gan_NA_2015[i] = 0
      gan_MORENA_2015[i] = 0
      gan_PH_2015[i] = 1
      gan_ES_2015[i] = 0
    } else if (df_variables_2[i,j + 8] == gan){ #Es ganador ES
      ganador[i] = 9
      gan_PRI_2015[i] = 0
      gan_PAN_2015[i] = 0
      gan_PRD_2015[i] = 0
      gan_IND_2015[i] = 0
      gan_MC_2015[i] = 0
      gan_NA_2015[i] = 0
      gan_MORENA_2015[i] = 0
      gan_PH_2015[i] = 0
      gan_ES_2015[i] = 1
    } 
  }
  df_variables_3 = cbind(df_variables, porc_ganador, ganador, gan_PRI_2015, gan_PAN_2015, gan_PRD_2015, gan_IND_2015, gan_MC_2015, gan_NA_2015, gan_MORENA_2015, gan_PH_2015, gan_ES_2015)  
  return(df_variables_3)  
}

