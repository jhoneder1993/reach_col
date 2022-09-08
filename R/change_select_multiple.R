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
  # Se agrega una columna en choices
  choices = choices %>% mutate(concated_column = paste(list_name, name, sep = ''))

  # se almacena la encuesta como xls1
  xls1 = encuesta  # Cambiar a i

  # Se crea dataset para tener el nombre y tipo de dato
  dataset <- data.frame(Type = character(),
                        Nombre = character())

  ##Colocar los nombres de la encuesta en fila
  for (j in 1:length(names(xls1))) {
    dataset <- dataset %>% add_row(Nombre = names(xls1[j]))
  }

  #Colocar el tipo de dato que es para despues obterner los labels
  for (a in 1:length(dataset$Nombre)) {
    for (b in 1:length(survey$name)){
      new_name <- str_split(dataset$Nombre[a], sep)
      new_name <- new_name[[1]][1]
      if (new_name == (replace_na(survey$name[b], ""))) {
        dataset$Type[a] <- survey$type[b]
      }
    }
  }


  ##Tipo select_one o select_multiple
  for (i in 1:length(dataset$Nombre)) {
    if (str_detect(replace_na(dataset$Type[i], ""), "select_multiple")){
      dataset$Type[i] <- "select_multiple"
    }else if(str_detect(replace_na(dataset$Type[i], ""), "select_one")){
      dataset$Type[i] <- "select_one"
    }else {
      dataset$Type[i] <- ""
    }
  }

  dataset <- dataset %>% filter(Type == "select_multiple" & str_detect(Nombre, sep))

  new.si <- readline(prompt = "Cual sera el nuevo valor para 1:  ")
  new.no <- readline(prompt = "Cual sera el nuevo valor para 0:  ")

  for (i in 1:nrow(dataset)) {
    var <- dataset[["Nombre"]][i]
    xls1[[var]] <- as.character(xls1[[var]])

    xls1 <- xls1 %>% mutate(!!sym(var) := case_when(!!sym(var) == "1" ~ new.si,
                                                    !!sym(var) == "0" ~ new.no,
                                                    TRUE ~ NA_character_))
  }

  ##Regrese la lista con los datos
  cat("\n")
  print("Funcion ejecutada con exito...")
  return(xls1)
}
