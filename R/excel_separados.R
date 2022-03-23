#' Excel Separados
#'
#' Genera salidas individuales segun la variable ingresada, ej. si la variable utilizada es
#' organizacion genera salida segun las organizaciones que se tengan
#'
#' @param df
#' @param variable
#'
#' @return
#' @export
#'
#' @examples


excel_separados <- function(df, variable="organizacion"){

  ### Guardar bases de datos segun ORG
  nombres <- df %>% select(variable[1]) %>% unique()

  for (i in 1:nrow(nombres)) {
    filtro <- df %>% filter(df[[variable]] %in% nombres[[1]][i])
    dir_filtro <- writexl::write_xlsx(list("Base de datos" = filtro),
                                      paste("Result/Clean_data/individuals/BD_", nombres[[1]][i] ,".xlsx", sep = ""))
  }

  # print avance
  print("La bases de datos individuales 'Result/Clean_data/individuals/BD_..' se han guardado de manera correcta.")
  cat("\n")

  # Generar una salida
  return("Ok")
}

