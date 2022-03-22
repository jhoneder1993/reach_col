#' Alojamiento Replacements
#'
#' Variables con 999 o 888 las cambia a no sabe o se rehusa a responder
#'
#' @param df dataframe
#'
#' @return Dataframe con sin los numeros 888 y 999
#' @export
#'
#' @examples output <- replacements_alojamiento(base_datos)
#' @examples output <- replacements_alojamiento(df = base_datos)


alojamiento_replacements <- function(df){
  ##Cambiar los valores 999 y 888 por No sabe y Se rehúsa a responder
  pos <- c("num_hogar", "num_nohogar", "inodoro_compt", "inodoro_indiv", "cuarto_prva", "cuarto_compt",
           "ducha_compt", "ducha_indiv", "lavamanos_compt", "lavamanos_indiv", "cocina_compt",
           "cocina_indiv", "lavan_compt", "lavan_indiv", "otr_facilidad", "cuartos", "area_mcuadrados",
           "deposito_cash", "pago_adelantado", "pago_formal", "cantidad_pago", "valor_ant_alojamiento",
           "valor_servicio_1", "valor_servicio_2", "valor_servicio_3", "valor_servicio_4", "valor_servicio_5",
           "valor_servicio_6", "valor_facilidades_1", "valor_facilidades_2", "valor_facilidades_3",
           "valor_facilidades_4", "valor_facilidades_999", "valor_facilidades_-999", "valor_facilidades -999", "cantidad_pago1")

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


  ##salida de la lista
  cat("\n")
  print("Funcion ejecutada con exito...")

  ##Generar la salida de la tabla
  return(df)
}
