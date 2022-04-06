#' Comercio Final Cleaning
#'
#' Genera dos excel con la informacion correspondiente para HQ y GIS, tambien regresa una lista con estos dos dataframe
#'
#' @param df Dataframe final para realizar las correspondientes salidas en excel para HQ y GIS
#'
#' @return 2 archivos de excel, uno para HQ y GIS
#' @return 1 lista con los dos dataframe de HQ y GIS
#' @export
#'
#' @examples lista_final <- comercio_final_cleaning(base_datos)
#' @examples lista_final <- comercio_final_cleaning(df = base_datos)


comercio_final_cleaning <- function(df){
  ## Crear lista
  lista <- list()
  df2 <- df


  ## Datos para HQ
  # borrar variables
  borrar <- c("_notes", "_status", "_submitted_by", "_tags", "_submission__notes",
              "_submission__status", "_submission__submitted_by", "_submission__tags")
  df_HQ <- df2[ , !(names(df2) %in% borrar)]

  # Eliminar informacion de las varariables
  blanco <- c("encuestador", "nombre_negocio", "ubicacion_gps", "comer_urbano",
              "nombre_capacitaciones", "datos_contacto", "comer_rural", "nombre_mercado",
              "organizacion", "_ubicacion_gps_latitude", "_ubicacion_gps_longitude",
              "_ubicacion_gps_altitude", "_ubicacion_gps_precision", "otr_organ")
  for (i in blanco){
    df_HQ[[i]] <- ""
  }

  # Guardar excel
  dir_df_ <- writexl::write_xlsx(list("Clean data" = df_HQ),
                                 "Result/Clean_data/HQ/JMMICOL_Dataset_comerciantes.xlsx")

  lista[["base_datos_HQ"]] <- data.table(df_HQ)

  # mostrar ejecucion
  cat("\n")
  print("La bases de datos para HQ 'Result/Clean_data/HQ/JMMICOL_Dataset_comerciantes.xlsx' se han guardado de manera correcta.")
  cat("\n")


  ##Datos para GIS
  #Variables que se dejan
  dejar <- c("_index", "_uuid", "departamento", "municipio", "zona_comercio", "depart_nuevo_1",
             "pais_nuevo_1", "depart_nuevo_2", "pais_nuevo_2", "depart_nuevo_3", "pais_nuevo_3",
             "depart_nuevo_4", "pais_nuevo_4", "depart_nuevo_5", "pais_nuevo_5", "depart_nuevo_6",
             "pais_nuevo_6", "desafios_30ds", "desafs_reabas", "desafios 2", "desafs_reabast_esp 2",
             "capa_abste_nuevo_1", "capa_abste_nuevo_2", "capa_abste_nuevo_3", "capa_abste_nuevo_4",
             "capa_abste_nuevo_5", "capa_abste_nuevo_6")
  df_GIS <- df[ , (names(df) %in% dejar)]



  #datos que si existen
  new_name <- data.frame(Nombre = character())
  no_estan <- data.frame(Nombre = character())
  for (i in dejar) {
    if (i %in% names(df)) {
      new_name <- new_name %>% add_row(Nombre = i)
    }else{
      no_estan <- no_estan %>% add_row(Nombre = i)
    }
  }

  # mostrar datos que no existen
  cat("\n")
  print("Las siguientes variables no se encuentra en la BD, por favor verificar si su nombre esta bien o modificar en el paquete funciones 'variables': ")
  print(no_estan)
  cat("\n")

  # Guardar excel
  dir_base_datos_filter2 <- writexl::write_xlsx(list("Nombres" = df_GIS),
                                                "Result/Clean_data/GIS/JMMICOL_Dataset_GIS_comerciantes.xlsx")

  lista[["base_datos_GIS"]] <- data.table(df_GIS)

  # mostrar ejecucion
  cat("\n")
  print("La bases de datos para HQ 'Result/Clean_data/GIS/JMMICOL_Dataset_GIS_comerciantes.xlsx' se han guardado de manera correcta.")
  cat("\n")

  ## salida de la lista
  print("Funcion ejecutada con exito...")

  ## Return la lista con toda la informacion
  return(lista)
}
