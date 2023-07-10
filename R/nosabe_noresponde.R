#' No sabe / No responde
#'
#' Manera de encontrar los no sabe y no responde cuando las preguntas son de tipo númerico y se han utilizado los codígos 888 y 999
#'
#' @param df Dataframe a verificar
#' @param uuid como aparece el uuui en el DF
#'
#' @return Un dataframe con la informacion variable, uuid y valor del no sabe no responde
#' @export
#'
#' @examples nainteger <- nosabe_noresponde(df, uuid = "uuid")
#'
#'


nosabe_noresponde <- function(df, uuid = "uuid") {

  encontradosraros <- data.frame(variable = character(),
                                 valor = character(),
                                 uuid = character())

  nainteger <- c("-888", "-8888", "-88888", "-888888",
                 "-999", "-9999", "-99999", "-999999",
                 -888, -8888, -88888, -888888,
                 -999, -9999, -99999, -999999,
                 "888", "8888", "88888", "888888",
                 "999", "9999", "99999", "999999",
                 888, 8888, 88888, 888888,
                 999, 9999, 99999, 999999)

  for (i in names(df)) {
    bus <- df |> filter(!!sym(i) %in% nainteger)
    if (nrow(bus) > 0) {
      bus <- bus |> mutate(valor := !!sym(i)) |> select(!!sym(uuid), valor) |> mutate(variable = i)
      encontradosraros <- rbind(encontradosraros, bus)
    }
  }
  return(encontradosraros)
}
