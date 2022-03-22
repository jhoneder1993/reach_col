#' Alojamiento Final Cleaning
#'
#' Genera dos excel con la informacion correspondiente para HQ y GIS, tambien regresa una lista con estos dos dataframe
#'
#' @param df Dataframe final para realizar las correspondientes salidas en excel para HQ y GIS
#'
#' @return 2 archivos de excel, uno para HQ y GIS
#' @return 1 lista con los dos dataframe de HQ y GIS
#' @export
#'
#' @examples lista_final <- alojamiento_final_cleaning(base_datos)
#' @examples lista_final <- alojamiento_final_cleaning(df = base_datos)

alojamiento_final_cleaning <- function(df){
  ## Crear lista
  lista <- list()
  df2 <- df

  ## Organizar las columnas
  # revisar que los datos esten
  variables <- c("servicios_pago_5", "modo_pago_servicio_5", "valor_servicio_5", "servicios_pago_6",
                 "modo_pago_servicio_6", "valor_servicio_6", "valor_facilidades_1",
                 "valor_facilidades_2", "valor_facilidades_3", "valor_facilidades_4", "valor_facilidades_-999")

  # datos que si existen
  organizar <- data.frame(Nombre = character())
  no_estan <- data.frame(Nombre = character())
  for (i in variables) {
    if (i %in% names(df2)) {
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
  df2 <- df2 %>% relocate(organizar$Nombre, .after = valor_servicio_4)

  # Cambiar -888 y -999 por vacios
  df2[["cantidad_pago1"]] <- case_when(df2$cantidad_pago1 ==  "-888" ~ "",
                                       df2$cantidad_pago1 == "-999" ~ "",
                                       TRUE ~ df2$cantidad_pago1)

  ## Datos para HQ
  # borrar variables
  borrar <- c("_validation_status", "_notes", "_status", "_submitted_by", "_tags",
              "_submission__notes", "_submission__status", "_submission__submitted_by",
              "_submission__tags", "dup nota_final")
  df_HQ <- df2[ , !(names(df2) %in% borrar)]

  # Eliminar informacion de las varariables
  blanco <- c("encuestador", "barrio", "vereda", "comentarios_finales", "organizacion")
  for (i in blanco){
    df_HQ[[i]] <- ""
  }

  # Guardar excel
  dir_df_ <- writexl::write_xlsx(list("Clean data" = df_HQ),
                                 "Result/Clean_data/HQ/MMICOL_Dataset_alojamiento.xlsx")

  lista[["base_datos_HQ"]] <- data.table(df_HQ)

  # mostrar ejecucion
  cat("\n")
  print("La bases de datos para HQ 'Result/Clean_data/HQ/MMICOL_Dataset_alojamiento.xlsx' se han guardado de manera correcta.")
  cat("\n")

  ##Datos para GIS
  #Variables que se dejan
  dejar <- c("uuid", "departamento", "municipio", "zona_municipio", "barrio", "vereda",
             "tipo_alojamiento", "seguridad", "desalojo", "cambio_precio", "razones_desalojo 1", "razones_aloj 1")
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
  dir_base_datos_filter2 <- writexl::write_xlsx(list("Con nombres" = df_GIS),
                                                "Result/Clean_data/GIS/JMMICOL_Shelter_Dataset_GIS.xlsx")

  lista[["base_datos_GIS"]] <- data.table(df_GIS)

  # mostrar ejecucion
  cat("\n")
  print("La bases de datos para HQ 'Result/Clean_data/GIS/JMMICOL_Shelter_Dataset_GIS.xlsx' se han guardado de manera correcta.")
  cat("\n")

  ## salida de la lista
  print("Funcion ejecutada con exito...")

  ## Return la lista con toda la informacion
  return(lista)
}
