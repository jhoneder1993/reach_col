#' KOBO to DAP
#'
#' Se alimenta de la información de la herramienta KOBO para generar un archivo DAP.
#'
#' @param sv Datos de la hoja "survey" de la herramienta kobo
#' @param ch Datos de la hoja "choices" de la herramienta kobo
#' @param label Como aparece la variable label
#'
#' @return un dataframen con los datos para terminar de ajustar el DAP
#' @export
#'
#' @examples datos <- Kobo_to_DAP(sv, ch, label = "label")


kobo_to_DAP <- function(sv, ch, label = "label") {
  ## Primer filtro de type
  data <- sv |> filter(!(type %in% c("calculate", "note", "start", "end", "audit", "today")))

  data[c("type.1", "type.2")] <- str_split_fixed(data$type, " ", 2)

  ## Almacenar datos en un Dataframe
  tabla <- data.frame("Research questions" = as.character(),
                      "Research sub-questions" = as.character(),
                      "IN #" = as.character(),
                      "Data collection method" = as.character(),
                      "Indicator group / sector" = as.character(),
                      "Indicator / Variable" = as.character(),
                      "Questionnaire Question" = as.character(),
                      "Instructions" = as.character(),
                      "Relevant" = as.character(),
                      "Constraint" = as.character(),
                      "Questionnaire Responses" = as.character(),
                      "Data collection level" = as.character(),
                      "Sampling" = as.character(),
                      "Comments" = as.character(),
                      check.names = FALSE)


  for (i in 1:nrow(data)) {

    print(i)
    # filtrar las opciones
    opc <- ch |> filter(list_name == data[["type.2"]][i])
    #opc <- opc |> select(label) |> mutate(prueba = paste0(label, collapse = " /n "))
    opc <- opc |> select(!!sym(label))
    print(data[["name"]][i])

    tabla <- tabla |> add_row("IN #" = as.character(i),
                              `Indicator / Variable` = data[["name"]][i],
                              `Questionnaire Question` = data[[label]][i],
                              `Instructions` = data[["type.1"]][i],
                              `Relevant` = data[["relevant"]][i],
                              `Constraint` = data[["constraint"]][i],
                              `Questionnaire Responses` = paste0(opc[[label]], collapse = "\n"))
  }

  # Homogenizar los type
  tabla <- tabla |> mutate(Instructions = case_when(Instructions == "date" ~ "Fecha",
                                                    Instructions == "select_one" ~ "Selección (única)",
                                                    Instructions == "geopoint" ~ "GPS",
                                                    Instructions == "text" ~ "Texto",
                                                    Instructions == "integer" ~ "Numérica",
                                                    Instructions == "select_multiple" ~ "Selección (múltiple)",
                                                    Instructions == "begin_group" ~ "Inicia grupo de preguntas",
                                                    Instructions == "end_group" ~ "Finaliza grupo de preguntas"))
  return(tabla)
}
