#' Separate Excel
#'
#' Genera salidas individuales segun la variable ingresada, ej. si la variable utilizada es
#' organizacion genera salida segun las organizaciones que se tengan.
#' los archivos se guardan en la ruta Result/Clean_data/individuals/BD_..
#'
#' @param df data frame a utilizar
#' @param variable variable con la cual separar los archivos de excel
#' @param eliminar conjunto de nombres de las variables a eliminar si hay datos personales
#'
#' @return excel separados por la variable suministrada en la ruta Result/Clean_data/individuals/BD_..
#' @export
#'
#' @examples eliminar= c("_notes", "_status", "_submitted_by", "_tags", "_submission__notes", "_submission__tags")
#' @examples separate_excel(base_merge, "organizacion", eliminar)
#' @examples separate_excel(df = base_merge, variable = "organizacion", borrar = eliminar)


separate_excel <- function(df, variable="organizacion", borrar=c()){

  # borrar variables
  df <- df[ , !(names(df) %in% borrar)]

  ### Guardar bases de datos segun ORG
  nombres <- df %>% select(variable[1]) %>% unique()

  for (i in 1:nrow(nombres)) {
    filtro <- df %>% filter(df[[variable]] %in% nombres[[1]][i])
    dir_filtro <- writexl::write_xlsx(list("Base de datos" = filtro),
                                      paste("Result/Clean_data/individuals/BD_", nombres[[1]][i] ,".xlsx", sep = ""))
  }

  # Generar una salida
  return("La bases de datos individuales 'Result/Clean_data/individuals/BD_..' se han guardado de manera correcta.")
}

