#' Comercio Replacements
#'
#' Variables con 999 o 888 las cambia a no sabe o se rehusa a responder
#'
#' @param df dataframe
#'
#' @return Dataframe con sin los numeros 888, 999 y sin las encuenesta donde no dieron el consentimiento
#' @export
#'
#' @examples output <- comercio_replacements(base_datos)
#' @examples output <- comercio_replacements(df = base_datos)


comercio_replacements <- function(df){
  # Eliminar las encuestas que no tenian el consentimiento
  eliminar <- df %>% filter(consentimiento == "No") %>% select(`_uuid`)

  print("Los siguientes uuid se van a eliminar debido a que no dieron el cosentimiento para realizar la encuesta: ")
  print(eliminar)
  cat("\n")

  # Dejar la base de datos sin los datos de consentimiento
  df <- df %>% filter(consentimiento != "No")


  ##Cambiar los valores 999 y 888 por No sabe y Se rehúsa a responder
  pos <- c("dias_exisnc_no_alimento_ant_1", "dias_exisnc_no_alimento_ant_2", "dias_exisnc_no_alimento_ant_3",
           "dias_exisnc_no_alimento_ant_4", "dias_exisnc_no_alimento_ant_5", "dias_exisnc_no_alimento_ant_6",
           "dias_exisnc_no_alimento_ant_7", "dias_exisnc_no_alimento_ant_8", "dias_exisnc_alimento_ant_1",
           "dias_exisnc_alimento_ant_2", "dias_exisnc_alimento_ant_3", "dias_exisnc_alimento_ant_4",
           "dias_exisnc_alimento_ant_5", "dias_exisnc_alimento_ant_6", "dias_exisnc_alimento_ant_7",
           "dias_exisnc_alimento_ant_8", "dias_exisnc_alimento_ant_9", "dias_exisnc_alimento_ant_10",
           "dias_exisnc_alimento_ant_11", "dias_exisnc_alimento_ant_12", "dias_exisnc_alimento_ant_13",
           "dias_exisnc_alimento_ant_14", "dias_exisnc_alimento_ant_15", "precio_no_alimento_ant_1",
           "precio_no_alimento_ant_2", "precio_no_alimento_ant_3", "precio_no_alimento_ant_4",
           "precio_no_alimento_ant_5", "precio_no_alimento_ant_6", "precio_no_alimento_ant_7",
           "precio_no_alimento_ant_8", "precio_alimento_ant_1", "precio_alimento_ant_2",
           "precio_alimento_ant_3", "precio_alimento_ant_4", "precio_alimento_ant_5",
           "precio_alimento_ant_6", "precio_alimento_ant_7", "precio_alimento_ant_8",
           "precio_alimento_ant_9", "precio_alimento_ant_10", "precio_alimento_ant_11",
           "precio_alimento_ant_12", "precio_alimento_ant_13", "precio_alimento_ant_14",
           "precio_alimento_ant_15", "tiempo_reabastecimiento", "desafios", "desafs_reabast_esp",
           "aumento_alimentos", "aumento_precio")

  #datos que si existen
  new_name <- data.frame(Nombre = character())
  no_estan <- data.frame(Nombre = character())
  for (i in pos) {
    if (i %in% names(df)) {
      new_name <- new_name %>% add_row(Nombre = i)
    }else{
      no_estan <- no_estan %>% add_row(Nombre = i)
    }
  }

  #mostrar datos que no existen
  cat("\n")
  print("Las siguientes variables no se encuentra en la BD, por favor verificar si su nombre esta bien o modificar en el paquete funciones 'variables': ")
  print(no_estan)
  cat("\n")

  #cambiar los datos segun algunos valores
  for (i in new_name$Nombre) {
    df[[i]][df[[i]] == "-999" | df[[i]] == "999" | df[[i]] == ".999" | df[[i]] == "9999"] <- "No sabe"
    df[[i]][df[[i]] == "-888" | df[[i]] == "888" | df[[i]] == ".888" | df[[i]] == "8888"] <- "Se rehusa a responder"
  }

  # Cambiar datos para baja_precio
  df[["baja_precio"]][df[["baja_precio"]] == "-999" | df[["baja_precio"]] == "999" | df[["baja_precio"]] == ".999" | df[["baja_precio"]] == "9999"] <- ""
  df[["baja_precio"]][df[["baja_precio"]] == "-888" | df[["baja_precio"]] == "888" | df[["baja_precio"]] == ".888" | df[["baja_precio"]] == "8888"] <- ""


  ##salida de la lista
  cat("\n")
  print("Funcion ejecutada con exito...")

  ##Generar la salida de la tabla
  return(df)
}
