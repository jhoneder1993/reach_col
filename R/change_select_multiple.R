#' Change Select Multiple
#'
#' La funcion toma un dataframe de kobo y cambia los datos del select multiple 1 y 0
#'
#' @param encuesta El parametro encuesta sera el dataframe de kobo
#' @param choices El parametro choices es la hoja Choices de la herramienta Kobo
#' @param survey El parametro survey es la hoja Survey de la herramienta Kobo
#' @param sep el separador de los select multiple "/", ".", "_"
#'
#' @return Un dataframe de la encuesta y con los valores de los select multiple convertidos
#' @export
#'
#' @examples survey <- read_excel("Input/KOBO_ABA_BOA VISTA - ANDRÉ_revGN_v6_GN.xlsx", sheet = "survey")
#' @examples choices <- read_excel("Input/KOBO_ABA_BOA VISTA - ANDRÉ_revGN_v6_GN.xlsx", sheet = "choices")
#' @examples df <-  read_excel("Input/REACH_BRA_ABAConsolidadoCleaning_20220906.xlsm", sheet = "Raw Data")
#'
#' @examples base <- change_select_multiple(df, choices, survey, "/")
#' @examples base <- change_select_multiple(encuesta = df, choices = choices, survey = survey, sep = "/")
#'


change_select_multiple <- function(encuesta, choices, survey, sep = "/"){
  # se almacena la encuesta como xls1
  xls1 = encuesta  # Cambiar a i


  survey[c("type.1", "type.2")] <- str_split_fixed(survey$type, " ", 2)

  survey <- survey |> filter(type.1 == "select_multiple")

  survey <- survey |> mutate(columname = name) |> select(columname, type.2)

  choices <- choices |> filter(list_name %in% survey$type.2)

  choices <- choices |> left_join(survey, by = c("list_name" = "type.2"))


  # Se agrega una columna en choices
  choices = choices %>% mutate(Nombre = paste(columname, name, sep = sep))



  dataset <- choices |> select(Nombre)

  new.si <- readline(prompt = "Cual sera el nuevo valor para 1:  ")
  new.no <- readline(prompt = "Cual sera el nuevo valor para 0:  ")

  for (i in 1:nrow(dataset)) {
    var <- dataset[["Nombre"]][i]
    if (var %in% colnames(xls1)) {
      xls1[[var]] <- as.character(xls1[[var]])

      xls1 <- xls1 %>% mutate(!!sym(var) := case_when(!!sym(var) == "1" | !!sym(var) == 1 | !!sym(var) == TRUE ~ new.si,
                                                      !!sym(var) == "0" | !!sym(var) == 0 | !!sym(var) == FALSE ~ new.no,
                                                      TRUE ~ NA_character_))
    } else{
      print(paste("No se encuentra la columna:", var))
    }
  }

  ##Regrese la lista con los datos
  cat("\n")
  print("Funcion ejecutada con exito...")
  return(xls1)
}
