#' Alojamiento Cleaning
#'
#' @param base_datos lista de las hojas que genero el full_reshape
#'
#' @return - Un dataframe con la hoja principal, servicios y facilidades unificado por el _uuid
#' @return - Ajustes de los nombres de las variables _uuid, _submission_time y _index
#' @return - Ajustes los 1 y 0 por Si y No
#' @export
#'
#' @examples base_datos_reshape <- full_reshape_alojamiento(base_datos, etiquetas, choices)
#'
#' @examples base_cleaning <- cleaning_alojamiento(base_datos_reshape)
#' @examples base_cleaning <- cleaning_alojamiento(base_datos_reshape = base_datos_reshape)


alojamiento_cleaning <- function(base_datos){
  ### Cambiar nombre a la based de datos de la Lista
  jmmi <- names(base_datos[1])
  jmmi_labels <- names(base_datos[2])
  names(base_datos)[names(base_datos) == jmmi] <- 'JMMI_COLOMBIA'
  names(base_datos)[names(base_datos) == jmmi_labels] <- 'JMMI_COLOMBIA_LABELS'


  ### Borrar duplicados
  ## ver si hay uuid repetidos en la pagina principal
  duplicados <- base_datos$JMMI_COLOMBIA %>% filter (duplicated("_uuid")) %>% select("_uuid")
  print(paste("Se encuentran los siguientes uuid  (" , length(duplicados[["_uuid"]]), ") duplicados en JMMI_COLOMBIA, los cuales seran eliminados:"))
  print(duplicados[["_uuid"]])
  cat("\n")

  ## se deja los restantes sin duplicados
  jmmi_colombia <- base_datos$JMMI_COLOMBIA %>% filter (!duplicated("_uuid"))


  ### Realizar Merge
  ## Cargar los archivos restantes
  servicios <- base_datos$servicios_reshape
  facilidades <- base_datos$facilidades_reshape

  ## se realiza el merge de las tres tablas
  jmmi_colombia <- merge(x = jmmi_colombia, y = servicios, by = "_uuid" , all.x = TRUE)
  jmmi_colombia <- merge(x = jmmi_colombia, y = facilidades, by = "_uuid" , all.x = TRUE)

  ## Eliminar duplicados
  duplicados <- jmmi_colombia %>% filter(duplicated("_uuid")) %>% select("_uuid")
  print(paste("Se encuentran los siguientes uuid  (" , length(duplicados[["_uuid"]]), ") duplicados en JMMI_COLOMBIA, los cuales seran eliminados:"))
  print(duplicados[["_uuid"]])
  cat("\n")

  # se deja los restantes sin duplicados
  jmmi_colombia <- jmmi_colombia %>% filter(!duplicated("_uuid"))
  jmmi_colombia

  # Organizar las columnas con los datos existentes
  jmmi_colombia <- jmmi_colombia %>% relocate(`_index`, .First)
  jmmi_colombia <- jmmi_colombia %>% relocate(`_uuid`, .after = `_index`)


  ### Renombrar opciones de respuesta en español
  variables <- c("barreras_aloj 1", "barreras_aloj 2", "barreras_aloj 3", "barreras_aloj 4", "barreras_aloj 5",
                 "barreras_aloj 6", "barreras_aloj 7", "barreras_aloj 8", "barreras_aloj 9", "barreras_aloj 10",
                 "barreras_aloj 11", "barreras_aloj 12", "barreras_aloj 13", "barreras_aloj -999", "barreras_aloj -888",
                 "nacionalidad 1", "nacionalidad 2", "nacionalidad 3", "nacionalidad -999", "nacionalidad -888",
                 "docu_migratorio 1", "docu_migratorio 2", "docu_migratorio 3", "docu_migratorio 4", "docu_migratorio 5",
                 "docu_migratorio 6", "docu_migratorio 7", "docu_migratorio 8", "docu_migratorio 9", "docu_migratorio 10",
                 "docu_migratorio 11", "docu_migratorio 12", "docu_migratorio -999", "docu_migratorio -888", "servicios 1",
                 "servicios 2", "servicios 3", "servicios 4", "servicios 5", "servicios 6", "servicios -999", "servicios -888",
                 "modalidad_basuras_antes 1", "modalidad_basuras_antes 2", "modalidad_basuras_antes 3",
                 "modalidad_basuras_antes 4", "modalidad_basuras_antes 5", "modalidad_basuras_antes 6",
                 "modalidad_basuras_antes 7", "modalidad_basuras_antes 8", "modalidad_basuras_antes 9",
                 "modalidad_basuras_antes -999", "modalidad_basuras_antes -888", "reciclaje 1", "reciclaje 2",
                 "reciclaje 3", "reciclaje 4", "reciclaje 5", "reciclaje 6", "modalidad_agua_antes 1", "modalidad_agua_antes 2",
                 "modalidad_agua_antes 3", "modalidad_agua_antes 4", "modalidad_agua_antes 5", "modalidad_agua_antes 6",
                 "modalidad_agua_antes 7", "modalidad_agua_antes 8", "modalidad_agua_antes 9", "modalidad_agua_antes 10",
                 "modalidad_agua_antes 11", "modalidad_agua_antes 12", "modalidad_agua_antes 13", "modalidad_agua_antes 14",
                 "modalidad_agua_antes -999", "modalidad_agua_antes -888", "facilidades_pago 1", "facilidades_pago 2",
                 "facilidades_pago 3", "facilidades_pago 4", "facilidades_pago -999", "facilidades_pago -888", "razones_inseguridad 1",
                 "razones_inseguridad 2", "razones_inseguridad 3", "razones_inseguridad 4", "razones_inseguridad 5",
                 "razones_inseguridad 6", "razones_inseguridad 7", "razones_inseguridad 8", "razones_inseguridad 9",
                 "razones_inseguridad 10", "razones_inseguridad -999", "razones_inseguridad -888", "implicaciones_contrato 1",
                 "implicaciones_contrato 2", "implicaciones_contrato 3", "implicaciones_contrato 4", "implicaciones_contrato 5",
                 "implicaciones_contrato 6", "implicaciones_contrato 7", "implicaciones_contrato 8", "implicaciones_contrato 9",
                 "implicaciones_contrato 10", "implicaciones_contrato -999", "implicaciones_contrato -888", "incumplimiento 1",
                 "incumplimiento 2", "incumplimiento 3", "incumplimiento 4", "incumplimiento 5", "incumplimiento 6", "incumplimiento 8",
                 "incumplimiento -999", "incumplimiento -888", "razones_desalojo 1", "razones_desalojo 2", "razones_desalojo 3",
                 "razones_desalojo 4", "razones_desalojo 5", "razones_desalojo 6", "razones_desalojo 7", "razones_desalojo 8",
                 "razones_desalojo -999", "razones_desalojo -888", "razones_aloj 1", "razones_aloj 2", "razones_aloj 3",
                 "razones_aloj 4", "razones_aloj 5", "razones_aloj 6", "razones_aloj 7", "razones_aloj 8",
                 "razones_aloj -999", "razones_aloj -888")

  ## datos que si existen
  existen <- data.frame(Nombre = character())
  no_estan <- data.frame(Nombre = character())
  for (i in variables) {
    if (i %in% names(jmmi_colombia)) {
      existen <- existen %>% add_row(Nombre = i)
    }else{
      no_estan <- no_estan %>% add_row(Nombre = i)
    }
  }

  ## mostrar datos que no existen
  print("Las siguientes variables no se encuentra en la BD, por favor verificar si su nombre esta bien o modificar en el paquete funciones 'reemplazar puntos': ")
  print(no_estan)
  cat("\n")

  ## Renombrando variables
  for(i in existen$Nombre){
    for (j in 1:nrow(jmmi_colombia)){
      if(replace_na(jmmi_colombia[[i]][j], "") == "0"){
        jmmi_colombia[[i]][j] <- "No"
      }else if(replace_na(jmmi_colombia[[i]][j], "") == "1"){
        jmmi_colombia[[i]][j] <- "Si"
      }
    }
  }

  ### Renombrar variables
  jmmi_colombia <- dplyr::rename(jmmi_colombia, uuid = "_uuid")
  jmmi_colombia <- dplyr::rename(jmmi_colombia, tiempo_sub_encuesta = "_submission_time")
  jmmi_colombia <- dplyr::rename(jmmi_colombia, index_encuesta = "_index")

  ## Organizar las columnas
  # revisar que los datos esten
  variables <- c("servicios_pago_1", "modo_pago_servicio_1", "valor_servicio_1", "servicios_pago_2",
                 "modo_pago_servicio_2", "valor_servicio_2", "servicios_pago_3", "modo_pago_servicio_3",
                 "valor_servicio_3", "servicios_pago_4", "modo_pago_servicio_4", "valor_servicio_4", "servicios_pago_5",
                 "modo_pago_servicio_5", "valor_servicio_5", "servicios_pago_6", "modo_pago_servicio_6",
                 "valor_servicio_6", "valor_facilidades_1", "valor_facilidades_2",
                 "valor_facilidades_3", "valor_facilidades_4", "valor_facilidades_-999")

  # datos que si existen
  organizar <- data.frame(Nombre = character())
  no_estan <- data.frame(Nombre = character())
  for (i in variables) {
    if (i %in% names(jmmi_colombia)) {
      organizar <- organizar %>% add_row(Nombre = i)
    }else{
      no_estan <- no_estan %>% add_row(Nombre = i)
    }
  }

  # mostrar datos que no existen
  print("Las siguientes variables no se encuentra en la BD, por favor verificar si su nombre esta bien o modificar en el paquete funciones 'organizar': ")
  print(no_estan)
  cat("\n")

  # Organizar las columnas con los datos existentes
  jmmi_colombia <- jmmi_colombia %>% relocate(organizar$Nombre,
                                              .before = seguridad)


  ### Guardar la base de datos en excel
  base_de_datos <- writexl::write_xlsx(list("Base de datos" = jmmi_colombia), "Result/Clean_data/Feedback/Feedback_alojamiento.xlsx")
  print("La base de datos 'Feedback_alojamiento.xlsx' se ha guardado de manera correcta")
  cat("\n")


  ### Guardar sin PII para HQ
  jmmi_colombia2 <- jmmi_colombia
  jmmi_colombia2["encuestador"] <- NA
  jmmi_colombia2["barrio"] <- NA
  jmmi_colombia2["vereda"] <- NA
  jmmi_colombia2["comentarios_finales"] <- NA
  jmmi_colombia2["organizacion"] <- NA
  jmmi_colombia2["organizacion_otr"] <- NA

  ## Guardar la base de datos en excel
  base_de_datos2 <- writexl::write_xlsx(list("Raw data" = jmmi_colombia2), "Result/Clean_data/HQ/JMMICOL_Dataset_alojamiento.xlsx")
  print("La base de datos 'JMMICOL_Dataset_alojamiento.xlsx' se ha guardado de manera correcta")
  cat("\n")


  ### Guardar para GIS
  jmmi_colombia3 <- jmmi_colombia %>% select(uuid, departamento, municipio, zona_municipio, barrio, vereda,
                                             tipo_alojamiento, seguridad)

  ## Guardar la base de datos en excel
  base_de_datos3 <- writexl::write_xlsx(list("Con nombres" = jmmi_colombia3), "Result/Clean_data/GIS/JMMICOL_Shelter_Dataset_GIS.xlsx")
  print("La base de datos 'JMMICOL_Shelter_Dataset_GIS.xlsx' se ha guardado de manera correcta")
  cat("\n")

  ##salida de la lista
  print("Funcion ejecutada con exito...")

  ##Generar la salida de la tabla
  return(jmmi_colombia)
}

