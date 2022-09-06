#' Kobo dataframe to R
#'
#' La funcion toma un dataframe de kobo y lo cambia de name a label
#'
#' @param encuesta El parametro encuesta sera el dataframe de kobo
#' @param choices El parametro choices es la hoja Choices de la herramienta Kobo
#' @param choices El parametro survey es la hoja Survey de la herramienta Kobo
#' @param label_name El parametro label_name es el nombre de la etiqueta a la que se quiere transformar los datos
#'
#' @return - Un dataframe de la encuesta y la convierte de numeros a los labels
#' @export
#'
#' @examples survey <- read_excel("Input/KOBO_ABA_BOA VISTA - ANDRÉ_revGN_v6_GN.xlsx", sheet = "survey")
#' @examples choices <- read_excel("Input/KOBO_ABA_BOA VISTA - ANDRÉ_revGN_v6_GN.xlsx", sheet = "choices")
#' @examples df <-  read_excel("Input/REACH_BRA_ABAConsolidadoCleaning_20220906.xlsm", sheet = "Raw Data")

#'
#' @examples base <- kobo.df_to_r(df, choices, survey, "label")
#' @examples base <- kobo.df_to_r(encuesta = df, choices = choices, survey = survey, label_name = "label")

kobo.df_to_r <- function(encuesta, choices, survey, label_name = "label"){
  choices = choices %>% mutate(concated_column = paste(list_name, name, sep = ''))

  xls1 = encuesta  # Cambiar a i
  #pasar todo a character
  xls1 <- setNames(data.frame(lapply(xls1, as.character)),
                   colnames(xls1))

  #pasar los NA a *****
  xls1 <- xls1 %>% mutate(across(everything(), ~replace_na(.x, "-----")))
  #reemplazar los nombres de las variables
  xls1 <- xls1 %>% rename_all(funs(str_replace_all(., "/", " ")))

  dataset <- data.frame(Type = character(),
                        Choice = character(),
                        Nombre = character(),
                        Etiqueta = character())

  ##Colocar los nombres de la encuesta en fila
  for (j in 1:length(names(xls1))) {
    dataset <- dataset %>% add_row(Nombre = names(xls1[j]))
  }

  #Colocar el tipo de dato que es para despues obterner los labels
  for (a in 1:length(dataset$Nombre)) {
    for (b in 1:length(survey$name)){
      new_name <- str_split(dataset$Nombre[a], " ")
      new_name <- new_name[[1]][1]
      if (new_name == (replace_na(survey$name[b], ""))) {
        dataset$Type[a] <- survey$type[b]
      }
    }
  }

  ##Colocar choice
  for (i in 1:length(dataset$Nombre)) {
    new_name <- str_split(dataset$Type[i], " ")
    new_name <- new_name[[1]][2]
    dataset$Choice[i] <- new_name
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

  ##Colocar los dato del select_one para despues pasar de numero a el valor
  for (i in 1:length(names(xls1))){
    for (j in 1:length(dataset$Nombre)){
      if (names(xls1[i]) == dataset$Nombre[j] & dataset$Type[j] == "select_one"){
        xls1[[i]] <- paste(dataset$Choice[j], xls1[[i]], sep = "")
      }
    }
  }

  ## Colocar los dato del select_multiple para despues pasar de numero a el valor
  for (i in 1:length(names(xls1))){
    for (j in 1:length(dataset$Nombre)){
      if (names(xls1[i]) == dataset$Nombre[j] & dataset$Type[j] == "select_multiple" & !(names(xls1[i]) %ilike% " ")){
        xls1[[i]] <- paste(dataset$Choice[j], xls1[[i]], sep = "")
      }
    }
  }

  # Completar los select_multiple, cambiar los espacios por el titulo
  for (i in 1:length(names(xls1))){
    for (j in 1:length(dataset$Nombre)){
      if (names(xls1[i]) == dataset$Nombre[j] & dataset$Type[j] == "select_multiple" & !(names(xls1[i]) %ilike% " ")){
        # Se separa con // para hacerlo mas legible
        xls1[[i]] <- gsub(" ", paste("  // ", dataset$Choice[j], sep = ""), xls1[[i]])
      }
    }
  }


  ##Quitar los datos que tienen el nombre con NA
  for (i in 1:length(names(xls1))) {
    for (j in 1:length(xls1[[3]])){
      x <- str_split(class(xls1[[i]]), " ")
      x <- x[1]
      if (x != "POSIXct") {
        if (str_detect(replace_na(xls1[[i]][j], ""), "-----")) {
          xls1[[i]][j] <- NA
        }
      }
    }
  }

  # Pasar los select multiples de numeros a los valores del cuestionario
  for (i in names(xls1)){
    x <- dataset %>% filter(Nombre == i)
    if (x$Type == "select_multiple" & !(i %ilike% " ")){
      filtro <- choices %>% filter(list_name == x$Choice)
      xls1[[i]] <- paste(xls1[[i]], " ", sep = "")
      for (z in 1:nrow(filtro)){
        xls1[[i]] <- gsub(paste(filtro[["concated_column"]][z], " ", sep = ""), paste(filtro[[label_name]][z], "", sep = "") , xls1[[i]])
        # Ajustar los "NA " creados a NA
        xls1[[i]] <- gsub("NA ", NA, xls1[[i]])
      }
    }
  }

  ## Pasar los datos restantes de numeros a los valores del cuestionario
  for (i in names(xls1)) {
    if(sum(is.na(choices[[label_name]][match(xls1[[i]],choices$concated_column)])) < length(xls1[[3]])){
      xls1[i] <- choices[[label_name]][match(xls1[[i]],choices$concated_column)]
    }
  }

  #regresar los datos de espacio a /
  xls1 <- xls1 %>% rename_all(funs(str_replace_all(., " ", "/")))


  ##Regrese la lista con los datos
  cat("\n")
  print("Funcion ejecutada con exito...")
  return(lista)
}
