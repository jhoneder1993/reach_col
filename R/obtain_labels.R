#' Obtain Labels
#'
#' @param sv el archivo survey de la herramienta de Kobo
#' @param cc el archivo choices de la herramienta de Kobo
#'
#' @return un dataframe con los names y sus debidas etiquetas
#' @export
#'
#' @examples etiquetas <- obtain_labels(survey, choices)

obtain_labels <- function(sv, cc, label = "label") {
  ## Select just the data to work
  sv <- sv |> select(type, name, !!sym(label))

  sv[c("type.1", "type.2")] <- str_split_fixed(sv$type, " ", 2)

  tabla <- data.frame(type = as.character(),
                      type.1 = as.character(),
                      type.2 = as.character(),
                      name = as.character(),
                      label = as.character())

  for (i in 1:nrow(sv)) {
    # Iniciar por las que no son select_multiple
    if (sv[["type.1"]][i] != "select_multiple") {
      # Agregar la informacion a la tabla vacia
      tabla <- tabla |> add_row(type = sv[["type.1"]][i],
                                type.1 = sv[["type.1"]][i],
                                type.2 = sv[["type.2"]][i],
                                name = sv[["name"]][i],
                                label = sv[[label]][i])
      # Si es select multiple
    } else {
      # realizar un filtro, un mutate, seleccionar la info importante, rename,
      # otro mutate para agregar la informacion
      tabla <- tabla |> add_row(type = sv[["type.1"]][i],
                                type.1 = sv[["type.1"]][i],
                                type.2 = sv[["type.2"]][i],
                                name = sv[["name"]][i],
                                label = sv[[label]][i])

      filtro <- cc |> filter(list_name == sv[["type.2"]][i]) |>
        mutate(conca = paste(sv[["name"]][i], name, sep="/")) |>
        select(!!sym(label), conca) |> rename(name = conca) |>
        mutate(type = sv[["type"]][i],
               type.1 = sv[["type.1"]][i],
               type.2 = sv[["type.2"]][i])

      # unir las tablas
      tabla <- rbind(tabla, filtro)
    }
  }
  # Regresar la tabla
  return(tabla)
}
