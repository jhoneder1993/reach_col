#' Alojamiento Checks
#'
#' Sirve para identificar los datos atipicos de precios y valores atipicos de la base de datos
#'
#' @param base_datos La base de datos despues de haber realizado el cleaning
#'
#' @return Encuestas_piloto
#' @return Intencion_moverse
#' @return Tipo_alojamiento_rancho
#' @return Tipo_alojamiento_temp
#' @return Tipo_alojamiento_pagadiario
#' @return Tipo_alojamiento_hotel
#' @return Num_hogar
#' @return Facilidades
#' @return Facilidadaes_otr
#' @return Num_cuartos
#' @return precioslocos_inquilinato
#' @return precioslocos_cuarto
#' @return precioslocos_pagadiario
#' @return precioslocos_casa
#' @return precioslocos_habitacion
#' @return precioslocos_albergue
#' @return precioslocos_asentamiento
#' @return Pago_alojamiento
#' @return Agua_valor
#' @return Gas_valor
#' @return EnergiaLuz_valor
#' @return Internet_valor
#' @return Telefono_valor
#' @return Disminuye_arriendo
#' @return Aumen ta_arriendo
#' @export
#'
#' @examples base_cleaning <- cleaning_alojamiento(base_datos_reshape)
#'
#' @examples base_outliers <- checks_alojamiento(base_cleaning)
#' @examples base_outliers <- checks_alojamiento(base_datos = base_cleaning )

alojamiento_checks <- function(base_datos){
  lista <- list()

  ### Calcular cantidad_pago1
  ## Crear la columna cantidad_pago1
  base_datos["cantidad_pago1"] <- NA

  ## Organizar columna creada
  base_datos <- base_datos %>% relocate(cantidad_pago1, .after = valor_ant_alojamiento)

  ## Calcular cantidad_pago1
  for(i in 1:nrow(base_datos)){
    if(replace_na(base_datos[["tiempo_pago"]][i], "") == "Diariamente"){
      base_datos[["cantidad_pago1"]][i] <- as.numeric(base_datos[["cantidad_pago"]][i]) * 30
    }else if(replace_na(base_datos[["tiempo_pago"]][i], "") == "Semanalmente"){
      base_datos[["cantidad_pago1"]][i] <- as.numeric(base_datos[["cantidad_pago"]][i]) * 4
    }else if(replace_na(base_datos[["tiempo_pago"]][i], "") == "Cada 15 días"){
      base_datos[["cantidad_pago1"]][i] <- as.numeric(base_datos[["cantidad_pago"]][i]) * 2
    }else if(replace_na(base_datos[["tiempo_pago"]][i], "") == "Cada mes"){
      base_datos[["cantidad_pago1"]][i] <- as.numeric(base_datos[["cantidad_pago"]][i])
    }
  }


  ### Calcular valor_ant_alojamiento1
  ## Crear la valor_ant_alojamiento1
  base_datos["valor_ant_alojamiento1"] <- NA

  ## Organizar columna creada
  base_datos <- base_datos %>% relocate(valor_ant_alojamiento1, .after = valor_ant_alojamiento)

  ## Calcular cantidad_pago1
  for(i in 1:nrow(base_datos)){
    if(replace_na(base_datos[["pago_alojamiento_ant"]][i], "") == "Diariamente"){
      base_datos[["valor_ant_alojamiento1"]][i] <- as.numeric(base_datos[["valor_ant_alojamiento"]][i]) * 30
    }else if(replace_na(base_datos[["pago_alojamiento_ant"]][i], "") == "Semanalmente"){
      base_datos[["valor_ant_alojamiento1"]][i] <- as.numeric(base_datos[["valor_ant_alojamiento"]][i]) * 4
    }else if(replace_na(base_datos[["pago_alojamiento_ant"]][i], "") == "Cada 15 días"){
      base_datos[["valor_ant_alojamiento1"]][i] <- as.numeric(base_datos[["valor_ant_alojamiento"]][i]) * 2
    }else if(replace_na(base_datos[["pago_alojamiento_ant"]][i], "") == "Cada mes"){
      base_datos[["valor_ant_alojamiento1"]][i] <- as.numeric(base_datos[["valor_ant_alojamiento"]][i])
    }
  }

  base_datos <- base_datos %>% filter(!replace_na(base_datos[["consentimiento"]], "") == "No")


  ### Verificar encuesta piloto
  base_datos2 <- base_datos

  ##fechas de recoleccion
  fechas <- base_datos %>% select("fecha_encuesta") %>% unique()
  fechas <- data.frame(fecha_encuesta = sort(fechas$fecha_encuesta))
  fechas$encuesta_piloto <- NA

  #agregar las variables a la base de datos
  base_datos2$encuesta_piloto <- NA

  #agregar los datos dados por el usuario a las fechas
  print("Para la verificacion de las encuestas piloto")
  cat("\n")
  for(i in 1:nrow(fechas)){
    valor <- readline(paste(prompt = "Hace parte de las encuestas piloto", paste(fechas[["fecha_encuesta"]][i], "?(si/no):", sep = "")))
    while (tolower(valor) != "si" & tolower(valor) != "no") {
      valor <- readline(paste(prompt = "Solo 'si' o 'no'. Hace parte de las encuestas piloto", paste(fechas[["fecha_encuesta"]][i], "?(si/no):", sep = "")))
    }
    fechas[["encuesta_piloto"]][i] <- valor
  }

  #Pasar los valores dados por el usuario de las fechas a la base de datos
  for(i in 1:nrow(fechas)){
    for(j in 1:nrow(base_datos)){
      if (fechas[["fecha_encuesta"]][i]  == base_datos2[["fecha_encuesta"]][j]){
        base_datos2[["encuesta_piloto"]][j] <- fechas[["encuesta_piloto"]][i]
      }
    }
  }

  # Hacer filtro para los que son encuesta piloto
  base_datos2 <- base_datos2 %>% filter(encuesta_piloto == "si")
  # Eliminar la columna creada para el filtro
  base_datos2 <- base_datos2 %>% select(-encuesta_piloto)

  #print avance
  print("La base de datos 'Encuestas_piloto' se ha ejecutado de manera correcta.")
  cat("\n")


  ### Check intencion de moverse
  base_datos3 <- base_datos
  dejar <- c("uuid", "fecha_encuesta", "organizacion", "encuestador", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "movimiento_otro")

  base_datos3 <- base_datos3[,(names(base_datos3)) %in% dejar]

  ## Filtro
  base_datos3 <- base_datos3 %>% filter(movimiento_otro != "")

  #print avance
  print("La base de datos 'Intencion_moverse' se ha ejecutado de manera correcta.")
  cat("\n")


  ### Check tipo de alojamiento
  ##Definicion rancho
  base_datos4 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento","municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "tipo_alojamiento_otr")
  base_datos4 <- base_datos4[,(names(base_datos4)) %in% dejar]

  base_datos4 <- base_datos4 %>% filter(tipo_alojamiento_otr != "")

  #print avance
  print("La base de datos 'Tipo_alojamiento_rancho' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Casa o apartamento- alojamiento temporal
  base_datos5 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento","municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "prop_alojamiento", "tipo_alojamiento")

  base_datos5 <- base_datos5[,(names(base_datos5)) %in% dejar]

  base_datos5 <- base_datos5 %>% filter(prop_alojamiento == "Hospedaje temporal" & tipo_alojamiento == "Casa o apartamento")

  #print avance
  print("La base de datos 'Tipo_alojamiento_temp' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Pagadiario diferente a alojamiento temporal
  base_datos6 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento","municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "prop_alojamiento", "tipo_alojamiento")
  base_datos6 <- base_datos6[,(names(base_datos6)) %in% dejar]

  base_datos6 <- base_datos6 %>% filter(prop_alojamiento == "Hospedaje temporal" & tipo_alojamiento == "Pagadiario o pasa día")

  #print avance
  print("La base de datos 'Tipo_alojamiento_pagadiario' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Habitacion hotel diferente a alojamiento temporal
  base_datos7 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento","municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "prop_alojamiento", "tipo_alojamiento")
  base_datos7 <- base_datos7[,(names(base_datos7)) %in% dejar]

  base_datos7 <- base_datos7 %>% filter(prop_alojamiento == "Hospedaje temporal" & tipo_alojamiento == "Habitación hotel")

  #print avance
  print("La base de datos 'Tipo_alojamiento_hotel' se ha ejecutado de manera correcta.")
  cat("\n")


  ### Confirmar outlier
  ## Numero integrantes hogar
  base_datos8 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento","municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "num_hogar", "num_nohogar")
  base_datos8 <- base_datos8[,(names(base_datos8)) %in% dejar]

  base_datos8 <- base_datos8 %>% filter(as.numeric(num_hogar) > 10 | as.numeric(num_nohogar) > 10)

  #print avance
  print("La base de datos 'Num_hogar' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Numero de facilidades
  base_datos9 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "inodoro_compt",
             "inodoro_indiv", "cuarto_prva", "cuarto_compt", "ducha_compt", "ducha_indiv", "lavamanos_compt",
             "lavamanos_indiv", "cocina_compt", "cocina_indiv", "otr_facilidad", "lavan_compt", "lavan_indiv")
  base_datos9 <- base_datos9[,(names(base_datos9)) %in% dejar]

  base_datos9 <- base_datos9 %>% filter(as.numeric(inodoro_compt) < 0 | as.numeric(inodoro_indiv < 0) | as.numeric(cuarto_prva < 0) |
                                          as.numeric(cuarto_compt) < 0 | as.numeric(ducha_compt < 0) | as.numeric(ducha_indiv < 0) |
                                          as.numeric(lavamanos_compt) < 0 | as.numeric(lavamanos_indiv) < 0 | as.numeric(cocina_compt < 0) |
                                          as.numeric(cocina_indiv) < 0 | as.numeric(otr_facilidad) < 0 | as.numeric(lavan_compt) < 0 |
                                          as.numeric(lavan_indiv) < 0 | as.numeric(inodoro_compt) > 3 | as.numeric(inodoro_indiv) > 3 |
                                          as.numeric(cuarto_prva) > 3 | as.numeric(cuarto_compt) > 3 | as.numeric(ducha_compt) > 3 |
                                          as.numeric(ducha_indiv) > 3 | as.numeric(lavamanos_compt) > 3 | as.numeric(lavamanos_indiv) > 3 |
                                          as.numeric(cocina_compt) > 3 | as.numeric(cocina_indiv) > 3 | as.numeric(otr_facilidad) > 3 |
                                          as.numeric(lavan_compt > 3) | as.numeric(lavan_indiv) > 3)

  #print avance
  print("La base de datos 'Facilidades' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Otra facilidad
  base_datos10 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "otr_facilidad_esp")
  base_datos10 <- base_datos10[,(names(base_datos10)) %in% dejar]

  base_datos10 <- base_datos10 %>% filter(otr_facilidad_esp != "")

  #print avance
  print("La base de datos 'Facilidades_otr' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Numero cuatros
  base_datos11 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "cuartos")
  base_datos11 <- base_datos11[,(names(base_datos11)) %in% dejar]

  base_datos11 <- base_datos11 %>% filter(as.numeric(cuartos) == 0 | as.numeric(cuartos) < 0 | as.numeric(cuartos) > 2)

  #print avance
  print("La base de datos 'Num_cuartos' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Cantidad pago inquilinato
  base_datos12 <- base_datos
  base_datos12 <- base_datos12 %>% filter(tipo_alojamiento == "Cuarto en Inquilinato")
  variables <- c("cantidad_pago1", "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal")


  for (i in variables){
    # Nombre de la nueva columna
    name <- paste("Z", i, sep = "_")
    #valor de la columna
    valor <- as.numeric(base_datos12[[i]]);
    #promedio de la columna de la que toma el valor
    mean_i <- mean(as.numeric(base_datos12[[i]]), na.rm = TRUE)
    #des. estandar de la columna de la que toma el valor
    sd_i <- sd(as.numeric(base_datos12[[i]]), na.rm = TRUE)

    # Nuevo valor de la columna calculado
    base_datos12[[name]] <- (abs(valor - mean_i) / sd_i)

    # Datos > 2 van a quedar con 1, o sea atipicos
    for (j in 1:nrow(base_datos12)) {
      if(replace_na(base_datos12[[name]][j], 0) > 2){
        base_datos12[[name]][j] <- 1
      }else{
        base_datos12[[name]][j] <- 0
      }
    }
  }

  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "tiempo_pago",
             "cantidad_pago", "cantidad_pago1",  "pago_alojamiento_ant", "valor_ant_alojamiento",
             "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal", "Z_valor_ant_alojamiento1",
             "Z_cantidad_pago1", "Z_deposito_cash", "Z_pago_adelantado", "Z_pago_formal")

  base_datos12 <- base_datos12[,(names(base_datos12)) %in% dejar]

  base_datos12 <- base_datos12 %>% filter(Z_cantidad_pago1 == 1 | Z_valor_ant_alojamiento1 == 1 |
                                            deposito_cash == 1 | Z_pago_adelantado == 1 | Z_pago_formal == 1)

  #print avance
  print("La base de datos 'precioslocos_inquilinato' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Cantidad pago cuarto
  base_datos13 <- base_datos
  base_datos13 <- base_datos13 %>% filter(tipo_alojamiento == "Cuarto en otro tipo de estructura")
  variables <- c("cantidad_pago1", "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal")

  if (nrow(base_datos13) > 0){
    for (i in variables){
      # Nombre de la nueva columna
      name <- paste("Z", i, sep = "_")
      #valor de la columna
      valor <- as.numeric(base_datos13[[i]]);
      #promedio de la columna de la que toma el valor
      mean_i <- mean(as.numeric(base_datos13[[i]]), na.rm = TRUE)
      #des. estandar de la columna de la que toma el valor
      sd_i <- sd(as.numeric(base_datos13[[i]]), na.rm = TRUE)

      # Nuevo valor de la columna calculado
      base_datos13[[name]] <- (abs(valor - mean_i) / sd_i)

      # Datos > 2 van a quedar con 1, o sea atipicos
      for (j in 1:nrow(base_datos13)) {
        if(replace_na(base_datos13[[name]][j], 0) > 2){
          base_datos13[[name]][j] <- 1
        }else{
          base_datos13[[name]][j] <- 0
        }
      }
    }


    dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
               "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "tiempo_pago",
               "cantidad_pago", "cantidad_pago1",  "pago_alojamiento_ant", "valor_ant_alojamiento",
               "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal", "Z_valor_ant_alojamiento1",
               "Z_cantidad_pago1", "Z_deposito_cash", "Z_pago_adelantado", "Z_pago_formal")

    base_datos13 <- base_datos13[,(names(base_datos13)) %in% dejar]

    base_datos13 <- base_datos13 %>% filter(Z_cantidad_pago1 == 1 | Z_valor_ant_alojamiento1 == 1 |
                                              deposito_cash == 1 | Z_pago_adelantado == 1 | Z_pago_formal == 1)

    #print avance
    print("La base de datos 'precioslocos_cuarto' se ha ejecutado de manera correcta.")
    cat("\n")
  } else {
    #print avance
    print("No hay datos para 'precioslocos_cuarto'. se ha ejecutado de manera correcta.")
    cat("\n")
  }




  ## Cantidad pago pagadiario
  base_datos14 <- base_datos
  base_datos14 <- base_datos14 %>% filter(tipo_alojamiento == "Pagadiario o pasa día")
  variables <- c("cantidad_pago1", "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal")

  if (nrow(base_datos14) > 0){
    for (i in variables){
      # Nombre de la nueva columna
      name <- paste("Z", i, sep = "_")
      #valor de la columna
      valor <- as.numeric(base_datos14[[i]]);
      #promedio de la columna de la que toma el valor
      mean_i <- mean(as.numeric(base_datos14[[i]]), na.rm = TRUE)
      #des. estandar de la columna de la que toma el valor
      sd_i <- sd(as.numeric(base_datos14[[i]]), na.rm = TRUE)

      # Nuevo valor de la columna calculado
      base_datos14[[name]] <- (abs(valor - mean_i) / sd_i)

      # Datos > 2 van a quedar con 1, o sea atipicos
      for (j in 1:nrow(base_datos14)) {
        if(replace_na(base_datos14[[name]][j], 0) > 2){
          base_datos14[[name]][j] <- 1
        }else{
          base_datos14[[name]][j] <- 0
        }
      }
    }

    dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
               "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "tiempo_pago",
               "cantidad_pago", "cantidad_pago1",  "pago_alojamiento_ant", "valor_ant_alojamiento",
               "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal", "Z_valor_ant_alojamiento1",
               "Z_cantidad_pago1", "Z_deposito_cash", "Z_pago_adelantado", "Z_pago_formal")

    base_datos14 <- base_datos14[,(names(base_datos14)) %in% dejar]

    base_datos14 <- base_datos14 %>% filter(Z_cantidad_pago1 == 1 | Z_valor_ant_alojamiento1 == 1 |
                                              deposito_cash == 1 | Z_pago_adelantado == 1 | Z_pago_formal == 1)

    #print avance
    print("La base de datos 'precioslocos_pagadiario' se ha ejecutado de manera correcta.")
    cat("\n")

  } else {
    #print avance
    print("No hay datos para 'precioslocos_pagadiario'. se ha ejecutado de manera correcta.")
    cat("\n")
  }



  ## Cantidad casa o apartamento
  base_datos15 <- base_datos
  base_datos15 <- base_datos15 %>% filter(tipo_alojamiento == "Pagadiario o pasa día")
  variables <- c("cantidad_pago1", "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal")

  if (nrow(base_datos15) > 0){
    for (i in variables){
      # Nombre de la nueva columna
      name <- paste("Z", i, sep = "_")
      #valor de la columna
      valor <- as.numeric(base_datos15[[i]]);
      #promedio de la columna de la que toma el valor
      mean_i <- mean(as.numeric(base_datos15[[i]]), na.rm = TRUE)
      #des. estandar de la columna de la que toma el valor
      sd_i <- sd(as.numeric(base_datos15[[i]]), na.rm = TRUE)

      # Nuevo valor de la columna calculado
      base_datos15[[name]] <- (abs(valor - mean_i) / sd_i)

      # Datos > 2 van a quedar con 1, o sea atipicos
      for (j in 1:nrow(base_datos15)) {
        if(replace_na(base_datos15[[name]][j], 0) > 2){
          base_datos15[[name]][j] <- 1
        }else{
          base_datos15[[name]][j] <- 0
        }
      }
    }

    dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
               "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "tiempo_pago",
               "cantidad_pago", "cantidad_pago1",  "pago_alojamiento_ant", "valor_ant_alojamiento",
               "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal", "Z_valor_ant_alojamiento1",
               "Z_cantidad_pago1", "Z_deposito_cash", "Z_pago_adelantado", "Z_pago_formal")

    base_datos15 <- base_datos15[,(names(base_datos15)) %in% dejar]

    base_datos15 <- base_datos15 %>% filter(Z_cantidad_pago1 == 1 | Z_valor_ant_alojamiento1 == 1 |
                                              deposito_cash == 1 | Z_pago_adelantado == 1 | Z_pago_formal == 1)

    #print avance
    print("La base de datos 'precioslocos_casa' se ha ejecutado de manera correcta.")
    cat("\n")
  } else {
    #print avance
    print("No hay datos para 'precioslocos_casa'. se ha ejecutado de manera correcta.")
    cat("\n")
  }



  ## Cantidad Habitacion hotel
  base_datos16 <- base_datos
  base_datos16 <- base_datos16 %>% filter(tipo_alojamiento == "Habitación hotel")
  variables <- c("cantidad_pago1", "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal")

  if (nrow(base_datos16) > 0){
    for (i in variables){
      # Nombre de la nueva columna
      name <- paste("Z", i, sep = "_")
      #valor de la columna
      valor <- as.numeric(base_datos16[[i]]);
      #promedio de la columna de la que toma el valor
      mean_i <- mean(as.numeric(base_datos16[[i]]), na.rm = TRUE)
      #des. estandar de la columna de la que toma el valor
      sd_i <- sd(as.numeric(base_datos16[[i]]), na.rm = TRUE)

      # Nuevo valor de la columna calculado
      base_datos16[[name]] <- (abs(valor - mean_i) / sd_i)

      # Datos > 2 van a quedar con 1, o sea atipicos
      for (j in 1:nrow(base_datos16)) {
        if(replace_na(base_datos16[[name]][j], 0) > 2){
          base_datos16[[name]][j] <- 1
        }else{
          base_datos16[[name]][j] <- 0
        }
      }
    }

    dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
               "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "tiempo_pago",
               "cantidad_pago", "cantidad_pago1",  "pago_alojamiento_ant", "valor_ant_alojamiento",
               "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal", "Z_valor_ant_alojamiento1",
               "Z_cantidad_pago1", "Z_deposito_cash", "Z_pago_adelantado", "Z_pago_formal")

    base_datos16 <- base_datos16[,(names(base_datos16)) %in% dejar]

    base_datos16 <- base_datos16 %>% filter(Z_cantidad_pago1 == 1 | Z_valor_ant_alojamiento1 == 1 |
                                              deposito_cash == 1 | Z_pago_adelantado == 1 | Z_pago_formal == 1)

    #print avance
    print("La base de datos 'precioslocos_habitacion' se ha ejecutado de manera correcta.")
    cat("\n")
  } else {
    #print avance
    print("No hay datos para 'precioslocos_habitacion'. se ha ejecutado de manera correcta.")
    cat("\n")
  }



  ## Cantidad Albergue
  base_datos17 <- base_datos
  base_datos17 <- base_datos17 %>% filter(tipo_alojamiento == "Albergue")
  variables <- c("cantidad_pago1", "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal")


  if (nrow(base_datos17) > 0){
    for (i in variables){
      # Nombre de la nueva columna
      name <- paste("Z", i, sep = "_")
      #valor de la columna
      valor <- as.numeric(base_datos17[[i]]);
      #promedio de la columna de la que toma el valor
      mean_i <- mean(as.numeric(base_datos17[[i]]), na.rm = TRUE)
      #des. estandar de la columna de la que toma el valor
      sd_i <- sd(as.numeric(base_datos17[[i]]), na.rm = TRUE)

      # Nuevo valor de la columna calculado
      base_datos17[[name]] <- (abs(valor - mean_i) / sd_i)

      # Datos > 2 van a quedar con 1, o sea atipicos
      for (j in 1:nrow(base_datos17)) {
        if(replace_na(base_datos17[[name]][j], 0) > 2){
          base_datos17[[name]][j] <- 1
        }else{
          base_datos17[[name]][j] <- 0
        }
      }
    }

    dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
               "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "tiempo_pago",
               "cantidad_pago", "cantidad_pago1",  "pago_alojamiento_ant", "valor_ant_alojamiento",
               "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal", "Z_valor_ant_alojamiento1",
               "Z_cantidad_pago1", "Z_deposito_cash", "Z_pago_adelantado", "Z_pago_formal")

    base_datos17 <- base_datos17[,(names(base_datos17)) %in% dejar]

    base_datos17 <- base_datos17 %>% filter(Z_cantidad_pago1 == 1 | Z_valor_ant_alojamiento1 == 1 |
                                              deposito_cash == 1 | Z_pago_adelantado == 1 | Z_pago_formal == 1)

    #print avance
    print("La base de datos 'precioslocos_albergue' se ha ejecutado de manera correcta.")
    cat("\n")
  } else {
    #print avance
    print("No hay datos para 'precioslocos_albergue'. se ha ejecutado de manera correcta.")
    cat("\n")
  }



  ## Cantidad Ocupación en terrenos privados mediante asentamientos informales.
  base_datos18 <- base_datos
  base_datos18 <- base_datos18 %>% filter(tipo_alojamiento == "Ocupación en terrenos privados mediante asentamientos informales.")
  variables <- c("cantidad_pago1", "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal")


  if (nrow(base_datos18) > 0){
    for (i in variables){
      # Nombre de la nueva columna
      name <- paste("Z", i, sep = "_")
      #valor de la columna
      valor <- as.numeric(base_datos18[[i]]);
      #promedio de la columna de la que toma el valor
      mean_i <- mean(as.numeric(base_datos18[[i]]), na.rm = TRUE)
      #des. estandar de la columna de la que toma el valor
      sd_i <- sd(as.numeric(base_datos18[[i]]), na.rm = TRUE)

      # Nuevo valor de la columna calculado
      base_datos18[[name]] <- (abs(valor - mean_i) / sd_i)

      # Datos > 2 van a quedar con 1, o sea atipicos
      for (j in 1:nrow(base_datos18)) {
        if(replace_na(base_datos18[[name]][j], 0) > 2){
          base_datos18[[name]][j] <- 1
        }else{
          base_datos18[[name]][j] <- 0
        }
      }
    }

    dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
               "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "tiempo_pago",
               "cantidad_pago", "cantidad_pago1",  "pago_alojamiento_ant", "valor_ant_alojamiento",
               "valor_ant_alojamiento1", "deposito_cash", "pago_adelantado", "pago_formal", "Z_valor_ant_alojamiento1",
               "Z_cantidad_pago1", "Z_deposito_cash", "Z_pago_adelantado", "Z_pago_formal")

    base_datos18 <- base_datos18[,(names(base_datos18)) %in% dejar]

    base_datos18 <- base_datos18 %>% filter(Z_cantidad_pago1 == 1 | Z_valor_ant_alojamiento1 == 1 |
                                              deposito_cash == 1 | Z_pago_adelantado == 1 | Z_pago_formal == 1)

    #print avance
    print("La base de datos 'precioslocos_asentamiento' se ha ejecutado de manera correcta.")
    cat("\n")
  } else {
    #print avance
    print("No hay datos para 'precioslocos_asentamiento'. se ha ejecutado de manera correcta.")
    cat("\n")
  }



  ## Valor servicios
  base_datos19 <- base_datos

  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "valor_servicio_1",
             "valor_servicio_2", "valor_servicio_3", "valor_servicio_4", "valor_servicio_5",
             "valor_servicio_6", "valor_facilidades_1", "valor_facilidades_2",
             "valor_facilidades_3", "valor_facilidades_4", "valor_facilidades_999")
  base_datos19 <- base_datos19[,(names(base_datos19)) %in% dejar]

  base_datos19 <- base_datos19 %>%
    filter(if("valor_servicio_1" %in% names(base_datos19)) as.numeric(valor_servicio_1) < 1000 else TRUE |
             if("valor_servicio_2" %in% names(base_datos19)) as.numeric(valor_servicio_2) < 1000 else TRUE |
             if("valor_servicio_3" %in% names(base_datos19)) as.numeric(valor_servicio_3) < 1000 else TRUE |
             if("valor_servicio_4" %in% names(base_datos19)) as.numeric(valor_servicio_4) < 1000 else TRUE |
             if("valor_servicio_5" %in% names(base_datos19)) as.numeric(valor_servicio_5) < 1000 else TRUE |
             if("valor_facilidades_1" %in% names(base_datos19)) as.numeric(valor_facilidades_1) < 1000 else TRUE |
             if("valor_facilidades_2" %in% names(base_datos19)) as.numeric(valor_facilidades_2) < 1000 else TRUE |
             if("valor_facilidades_3" %in% names(base_datos19)) as.numeric(valor_facilidades_3) < 1000 else TRUE |
             if("valor_facilidades_4" %in% names(base_datos19)) as.numeric(valor_facilidades_4) < 1000 else TRUE |
             as.numeric(valor_servicio_1) > 500000 | as.numeric(valor_servicio_2) > 300000 | as.numeric(valor_servicio_3) > 200000 |
             as.numeric(valor_servicio_4) > 200000 | as.numeric(valor_servicio_5) > 200000 |
             if("valor_facilidades_1" %in% names(base_datos19)) as.numeric(valor_facilidades_1) > 20000 else TRUE |
             if("valor_facilidades_2" %in% names(base_datos19)) as.numeric(valor_facilidades_2) > 20000 else TRUE |
             if("valor_facilidades_3" %in% names(base_datos19)) as.numeric(valor_facilidades_3) > 20000 else TRUE |
             if("valor_facilidades_4" %in% names(base_datos19)) as.numeric(valor_facilidades_4) > 20000 else TRUE)
  #print avance
  print("La base de datos 'Pago_alojamiento' se ha ejecutado de manera correcta.")
  cat("\n")


  ### Checks Logicos
  ## Agua
  base_datos20 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "servicios 1",
             "modo_pago_servicio_1", "valor_servicio_1")
  base_datos20 <- base_datos20[,(names(base_datos20)) %in% dejar]
  base_datos20 <- base_datos20 %>% filter(modo_pago_servicio_1 == "Se paga por aparte" & as.numeric(valor_servicio_1) == 0 )

  #print avance
  print("La base de datos 'Agua_valor' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Gas
  base_datos21 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "servicios 1",
             "modo_pago_servicio_2", "valor_servicio_2")
  base_datos21 <- base_datos21[,(names(base_datos21)) %in% dejar]
  base_datos21 <- base_datos21 %>% filter(modo_pago_servicio_2 == "Se paga por aparte" & as.numeric(valor_servicio_2) == 0 )

  #print avance
  print("La base de datos 'Gas_valor' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Energia y luz
  base_datos22 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "servicios 1",
             "modo_pago_servicio_3", "valor_servicio_3")
  base_datos22 <- base_datos22[,(names(base_datos22)) %in% dejar]
  base_datos22 <- base_datos22 %>% filter(modo_pago_servicio_3 == "Se paga por aparte" & as.numeric(valor_servicio_3) == 0 )

  #print avance
  print("La base de datos 'EnergiaLuz_valor' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Conectividad internet
  base_datos23 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "servicios 1",
             "modo_pago_servicio_4", "valor_servicio_4")
  base_datos23 <- base_datos23[,(names(base_datos23)) %in% dejar]
  base_datos23 <- base_datos23 %>% filter(modo_pago_servicio_4 == "Se paga por aparte" & as.numeric(valor_servicio_4) == 0 )

  #print avance
  print("La base de datos 'Internet_valor' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Conectividad telefono
  base_datos24 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento", "servicios 1",
             "modo_pago_servicio_5", "valor_servicio_5")
  base_datos24 <- base_datos24[,(names(base_datos24)) %in% dejar]
  base_datos24 <- base_datos24 %>% filter(if("valor_servicio_5" %in% names(base_datos24)) modo_pago_servicio_5 == "Se paga por aparte" & as.numeric(valor_servicio_5) == 0 else FALSE)

  #print avance
  print("La base de datos 'telefono_valor' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Cantidad pago (si disminuye): se verifica que si se reporta una disminución
  ## en el pago del arriendo efectivamente se cumpla
  base_datos25 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento",
             "cantidad_pago", "cambio_precio", "valor_ant_alojamiento")
  base_datos25 <- base_datos25[,(names(base_datos25)) %in% dejar]
  base_datos25 <- base_datos25 %>% filter(cantidad_pago == valor_ant_alojamiento | as.numeric(cantidad_pago) > as.numeric(valor_ant_alojamiento))

  #print avance
  print("La base de datos 'Disminuye_arriendo' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Cantidad pago (si aumenta):  se verifica que si se reporta un aumento
  ## en el pago del arriendo efectivamente se cumpla
  base_datos26 <- base_datos
  dejar <- c("uuid", "encuestador", "fecha_encuesta", "organizacion", "departamento", "municipio",
             "zona_municipio", "barrio", "vereda", "sexo", "tipo_alojamiento",
             "cantidad_pago", "cambio_precio", "valor_ant_alojamiento")
  base_datos26 <- base_datos26[,(names(base_datos26)) %in% dejar]
  base_datos26 <- base_datos26 %>% filter(cantidad_pago == valor_ant_alojamiento | as.numeric(cantidad_pago) < as.numeric(valor_ant_alojamiento))

  #print avance
  print("La base de datos 'Aumenta_arriendo' se ha ejecutado de manera correcta.")
  cat("\n")


  ##dia escrito, mes escrito, mes numero, dia, hora, minuto, segundo, año
  ##format(Sys.time(), "%a %b %m %d %H:%M:%S %Y")
  dia <- format(Sys.time(), "%d-%m-%Y")

  ## Guardar las base de datos
  dir_base_datos <- writexl::write_xlsx(list("Encuestas_piloto" = base_datos2,
                                             "Intencion_moverse" = base_datos3,
                                             "Tipo_alojamiento_rancho" = base_datos4,
                                             "Tipo_alojamiento_temp" = base_datos5,
                                             "Tipo_alojamiento_pagadiario" = base_datos6,
                                             "Tipo_alojamiento_hotel" = base_datos7,
                                             "Num_hogar" = base_datos8,
                                             "Facilidades" = base_datos9,
                                             "Facilidadaes_otr" = base_datos10,
                                             "Num_cuartos" = base_datos11,
                                             "precioslocos_inquilinato" = base_datos12,
                                             "precioslocos_cuarto" = base_datos13,
                                             "precioslocos_pagadiario" = base_datos14,
                                             "precioslocos_casa" = base_datos15,
                                             "precioslocos_habitacion" = base_datos16,
                                             "precioslocos_albergue" = base_datos17,
                                             "precioslocos_asentamiento" = base_datos18,
                                             "Pago_alojamiento" = base_datos19,
                                             "Agua_valor" = base_datos20,
                                             "Gas_valor" = base_datos21,
                                             "EnergiaLuz_valor" = base_datos22,
                                             "Internet_valor" = base_datos23,
                                             "Telefono_valor" = base_datos24,
                                             "Disminuye_arriendo" = base_datos25,
                                             "Aumenta_arriendo" = base_datos26),
                                        paste("Result/Clean_data/Check/REACH_Checks_JMMICOL_Sheltter_Total_", dia ,".xlsx", sep = ""))

  #print avance
  print("La bases de datos se han guardado en el archivo 'Result/Clean_data/Check/REACH_Checks_JMMICOL_Sheltter_Total_...' de manera correcta.")
  cat("\n")


  ### Guardar bases de datos segun ORG
  org <- base_datos %>% select(organizacion) %>% unique()

  for (i in 1:nrow(org)) {
    filtro <- base_datos %>% filter(organizacion %in% org[[1]][i])
    dir_filtro <- writexl::write_xlsx(list("Base de datos" = filtro),
                                      paste("Result/Clean_data/Feedback/Formato_Limpieza_Vivienda_", org[[1]][i] ,".xlsx", sep = ""))
  }

  #print avance
  print("La bases de datos segun ORG 'Result/Clean_data/Feedback/Formato_Limpieza_Vivienda_..' se han guardado de manera correcta.")
  cat("\n")


  ##salida de la lista
  print("Funcion ejecutada con exito...")

  ##Return la lista con toda la informacion
  return(base_datos)
}
