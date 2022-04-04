#' Comercio Merge
#'
#' @param base_datos lista de las hojas que genero el full_reshape
#'
#' @return - Un dataframe con la hoja principal, abastecimiento, food y non_food unificado por el _uuid
#' @return - Ajustes los 1 y 0 por Si y No
#' @export
#'
#' @examples base_datos_reshape <- full_reshape_alojamiento(base_datos, etiquetas, choices)
#'
#' @examples base_merge <- comercio_merge(base_datos)
#' @examples base_merge <- comercio_merge(base_datos = base_datos_reshape)


comercio_merge <- function(base_datos){
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
  abastecimiento <- base_datos$abastecimiento_reshape
  food <- base_datos$food_reshape
  nfi <- base_datos$non_food_reshape

  ## se realiza el merge de las cuatro tablas
  jmmi_colombia <- merge(x = jmmi_colombia, y = abastecimiento, by = "_uuid" , all.x = TRUE)
  jmmi_colombia <- merge(x = jmmi_colombia, y = food, by = "_uuid" , all.x = TRUE)
  jmmi_colombia <- merge(x = jmmi_colombia, y = nfi, by = "_uuid" , all.x = TRUE)

  ## Eliminar duplicados
  duplicados <- jmmi_colombia %>% filter(duplicated("_uuid")) %>% select("_uuid")
  print(paste("Se encuentran los siguientes uuid  (" , length(duplicados[["_uuid"]]), ") duplicados en JMMI_COLOMBIA, los cuales seran eliminados:"))
  print(duplicados[["_uuid"]])
  cat("\n")

  # se deja los restantes sin duplicados
  jmmi_colombia <- jmmi_colombia %>% filter(!duplicated("_uuid"))

  # Organizar las columnas con los datos existentes
  jmmi_colombia <- jmmi_colombia %>% relocate(`_index`, .First)
  jmmi_colombia <- jmmi_colombia %>% relocate(`_uuid`, .after = `_index`)

  ## Organizar las columnas
  # revisar que los datos esten
  variables <- c("capa_abste_nuevo_1", "proveedor_nuevo_1", "depart_nuevo_1", "pais_nuevo_1", "otr_pais_nuevo_1", "tipo_proveedor_nuevo_1", "otr_tipo_proveedor_nuevo_1",
                 "capa_abste_nuevo_2", "proveedor_nuevo_2", "depart_nuevo_2", "pais_nuevo_2", "otr_pais_nuevo_2", "tipo_proveedor_nuevo_2", "otr_tipo_proveedor_nuevo_2",
                 "capa_abste_nuevo_3", "proveedor_nuevo_3", "depart_nuevo_3", "pais_nuevo_3", "otr_pais_nuevo_3", "tipo_proveedor_nuevo_3", "otr_tipo_proveedor_nuevo_3",
                 "capa_abste_nuevo_4", "proveedor_nuevo_4", "depart_nuevo_4", "pais_nuevo_4", "otr_pais_nuevo_4", "tipo_proveedor_nuevo_4", "otr_tipo_proveedor_nuevo_4",
                 "capa_abste_nuevo_5", "proveedor_nuevo_5", "depart_nuevo_5", "pais_nuevo_5", "otr_pais_nuevo_5", "tipo_proveedor_nuevo_5", "otr_tipo_proveedor_nuevo_5",
                 "capa_abste_nuevo_6", "proveedor_nuevo_6", "depart_nuevo_6", "pais_nuevo_6", "otr_pais_nuevo_6", "tipo_proveedor_nuevo_6", "otr_tipo_proveedor_nuevo_6",
                 "capa_abste_nuevo_7", "proveedor_nuevo_7", "depart_nuevo_7", "pais_nuevo_7", "otr_pais_nuevo_7", "tipo_proveedor_nuevo_7", "otr_tipo_proveedor_nuevo_7",
                 "capa_abste_nuevo_8", "proveedor_nuevo_8", "depart_nuevo_8", "pais_nuevo_8", "otr_pais_nuevo_8", "tipo_proveedor_nuevo_8", "otr_tipo_proveedor_nuevo_8",
                 "capa_abste_nuevo_9", "proveedor_nuevo_9", "depart_nuevo_9", "pais_nuevo_9", "otr_pais_nuevo_9", "tipo_proveedor_nuevo_9", "otr_tipo_proveedor_nuevo_9",
                 "precio_no_alimento_ant_1", "dias_exisnc_no_alimento_ant_1",
                 "precio_no_alimento_ant_2", "dias_exisnc_no_alimento_ant_2",
                 "precio_no_alimento_ant_3", "dias_exisnc_no_alimento_ant_3",
                 "precio_no_alimento_ant_4", "dias_exisnc_no_alimento_ant_4",
                 "precio_no_alimento_ant_5", "dias_exisnc_no_alimento_ant_5",
                 "precio_no_alimento_ant_6", "dias_exisnc_no_alimento_ant_6",
                 "precio_no_alimento_ant_7", "dias_exisnc_no_alimento_ant_7",
                 "precio_no_alimento_ant_8", "dias_exisnc_no_alimento_ant_8",
                 "precio_no_alimento_ant_9", "dias_exisnc_no_alimento_ant_9",
                 "precio_alimento_ant_1", "dias_exisnc_alimento_ant_1",
                 "precio_alimento_ant_2", "dias_exisnc_alimento_ant_2",
                 "precio_alimento_ant_3", "dias_exisnc_alimento_ant_3",
                 "precio_alimento_ant_4", "dias_exisnc_alimento_ant_4",
                 "precio_alimento_ant_5", "dias_exisnc_alimento_ant_5",
                 "precio_alimento_ant_6", "dias_exisnc_alimento_ant_6",
                 "precio_alimento_ant_7", "dias_exisnc_alimento_ant_7",
                 "precio_alimento_ant_8", "dias_exisnc_alimento_ant_8",
                 "precio_alimento_ant_9", "dias_exisnc_alimento_ant_9",
                 "precio_alimento_ant_10", "dias_exisnc_alimento_ant_10",
                 "precio_alimento_ant_11", "dias_exisnc_alimento_ant_11",
                 "precio_alimento_ant_12", "dias_exisnc_alimento_ant_12",
                 "precio_alimento_ant_13", "dias_exisnc_alimento_ant_13",
                 "precio_alimento_ant_14", "dias_exisnc_alimento_ant_14",
                 "precio_alimento_ant_15", "dias_exisnc_alimento_ant_15")



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
                                              .before = otravez_encuesta)


  ### Renombrar opciones de respuesta en español
  variables <- c("medios_pago 1", "medios_pago 2", "medios_pago 3", "medios_pago 4", "medios_pago 5",
                 "medios_pago 6", "medios_pago 7", "medios_pago 8", "medios_pago 9", "medios_pago 10",
                 "medios_pago -999", "medios_pago -888", "tipo_producto 1", "tipo_producto 2", "tipo_producto 3",
                 "tipo_producto 4", "tipo_producto 5", "tipo_producto 6", "tipo_producto -999", "tipo_producto -888",
                 "problemas_acceso 1", "problemas_acceso 2", "problemas_acceso 3", "problemas_acceso 4",
                 "problemas_acceso 5", "problemas_acceso 6", "problemas_acceso 7", "problemas_acceso 8",
                 "problemas_acceso 9", "problemas_acceso 10", "problemas_acceso 11", "problemas_acceso -999",
                 "problemas_acceso -888", "unprov_nuevo 1", "unprov_nuevo 2", "unprov_nuevo 3", "unprov_nuevo 4",
                 "unprov_nuevo 5", "unprov_nuevo 6", "unprov_nuevo 7", "unprov_nuevo -999", "unprov_nuevo -888",
                 "alimentos_venta 1", "alimentos_venta 2", "alimentos_venta 3", "alimentos_venta 4", "alimentos_venta 5",
                 "alimentos_venta 6", "alimentos_venta 7", "alimentos_venta 8", "alimentos_venta 9", "alimentos_venta 10",
                 "alimentos_venta 11", "alimentos_venta 12", "alimentos_venta 13", "alimentos_venta 14", "alimentos_venta 15",
                 "alimentos_venta -999", "alimentos_venta -888", "no_alimentos_venta 1", "no_alimentos_venta 2",
                 "no_alimentos_venta 3", "no_alimentos_venta 4", "no_alimentos_venta 5", "no_alimentos_venta 6",
                 "no_alimentos_venta 7", "no_alimentos_venta 8", "no_alimentos_venta 9", "no_alimentos_venta -999",
                 "no_alimentos_venta -888", "aumento_precio 1", "aumento_precio 2", "aumento_precio 3", "aumento_precio 4",
                 "aumento_precio 5", "aumento_precio 6", "aumento_precio 7", "aumento_precio 8", "aumento_precio 9",
                 "aumento_precio 10", "aumento_precio 11", "aumento_precio 12", "aumento_precio 13", "aumento_precio 14",
                 "aumento_precio 15", "aumento_precio -999", "aumento_precio -888", "baja_precio 1", "baja_precio 2",
                 "baja_precio 3", "baja_precio 4", "baja_precio 5", "baja_precio 6", "baja_precio 7", "baja_precio 8",
                 "baja_precio 9", "baja_precio 10", "baja_precio 11", "baja_precio 12", "baja_precio 13", "baja_precio 14",
                 "baja_precio -999", "baja_precio -888", "desafios 1", "desafios 2", "desafios 3", "desafios 4", "desafios 5",
                 "desafios 6", "desafios 7", "desafios 8", "desafios 9", "desafios 10", "desafios 11", "desafios 12",
                 "desafios 13", "desafios 14", "desafios 15", "desafios 16", "desafios 17", "desafios 18", "desafios -999",
                 "desafios -888", "desafs_reabast_esp 1", "desafs_reabast_esp 2", "desafs_reabast_esp 3", "desafs_reabast_esp 4",
                 "desafs_reabast_esp 5", "desafs_reabast_esp 6", "desafs_reabast_esp 7", "desafs_reabast_esp 8",
                 "desafs_reabast_esp 9", "desafs_reabast_esp 10", "desafs_reabast_esp 11", "desafs_reabast_esp 12",
                 "desafs_reabast_esp 13", "desafs_reabast_esp 14", "desafs_reabast_esp 15", "desafs_reabast_esp 16",
                 "desafs_reabast_esp 17", "desafs_reabast_esp 18", "desafs_reabast_esp -999", "desafs_reabast_esp -888",
                 "transporte 1", "transporte 2", "transporte 3", "transporte 4", "transporte 5", "transporte 6", "transporte 7",
                 "transporte 8", "transporte 9", "transporte 10", "transporte 11", "transporte -999", "transporte -888")

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
  print("Las siguientes variables no se encuentra en la BD, por favor verificar si su nombre esta bien o modificar en el paquete funciones 'reemplazar 0 y 1 por Si y No': ")
  print(no_estan)
  cat("\n")

  # Otra manera de cambiar los 1y 0
  #jmmi_colombia2 <- jmmi_colombia2 %>% mutate(!!sym(prueba) := case_when(
  #  !!sym(prueba) == "1" ~ "Si",
  #  !!sym(prueba) == "0" ~ "No",
  #  TRUE ~ !!sym(prueba)))

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

  ## Guardar la base de datos en excel
  base_datos1 <- writexl::write_xlsx(list("Base de datos" = jmmi_colombia), "Result/Clean_data/Feedback/Feedback_Comercio.xlsx")
  print("La base de datos 'Result/Clean_data/Feedback/Feedback_Comercio.xlsx' se ha guardado de manera correcta")
  cat("\n")


  ### Guardar sin PII para HQ
  jmmi_colombia2 <- jmmi_colombia
  jmmi_colombia2["encuestador"] <- NA
  jmmi_colombia2["ubicacion_gps"] <- NA
  jmmi_colombia2["nombre_negocio"] <- NA
  jmmi_colombia2["comer_urbano"] <- NA
  jmmi_colombia2["comer_rural"] <- NA
  jmmi_colombia2["nombre_mercado"] <- NA
  jmmi_colombia2["organizacion"] <- NA
  jmmi_colombia2["otr_organ"] <- NA
  jmmi_colombia2["_ubicacion_gps_latitude"] <- NA
  jmmi_colombia2["_ubicacion_gps_longitude"] <- NA
  jmmi_colombia2["_ubicacion_gps_altitude"] <- NA
  jmmi_colombia2["_ubicacion_gps_precision"] <- NA


  ## Guardar la base de datos en excel
  base_de_datos2 <- writexl::write_xlsx(list("Raw data" = jmmi_colombia2),
                                        "Result/Clean_data/HQ/JMMICOL_Dataset_Comerciantes.xlsx")
  print("La base de datos 'Result/Clean_data/HQ/JMMICOL_Dataset_Comerciantes.xlsx' se ha guardado de manera correcta")
  cat("\n")


  ##salida de la lista
  print("Funcion ejecutada con exito...")

  ##Generar la salida de la tabla
  return(jmmi_colombia)
}
