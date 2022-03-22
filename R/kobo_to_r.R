#' Kobo to R
#'
#' La funcion toma la encuesta descargada de kobo y la pasa a un dataframe
#'
#' @param encuesta El parametro encuesta sera el directorio con el archivo descargado de kobo
#' @param choices El parametro choices es la hoja Choices de la herramienta Kobo
#'
#' @return - Una lista con los dataframe de todas las hojas de la encuesta y la convierte de numeros a los labels
#' @export
#'
#' @examples name <- "REACH_JMMI_Survey.xlsx"
#' @examples directorio <- paste("Input/", name, sep="")
#' @examples choices <- read_excel(directorio, 'choices')
#'
#' @examples name2 <- "COL2001_JMMI_FECHA.xlsx"
#' @examples encuesta <- paste("Input/", name2, sep="")
#'
#' @examples base <- kobo_to_r(encuesta, choices)
#' @examples base <- kobo_to_r(encuesta = directorio2, choices = choices)


kobo_to_r <- function(encuesta, choices){
  choices = choices %>% mutate(concated_column = paste(list_name, name, sep = ''))
  lista <- list()
  for(i in 1:length(excel_sheets(encuesta))){
    hojas = excel_sheets(encuesta)
    namesheet <- hojas[i]
    print(paste('INICIAMOS EL SHEET', i, ":", namesheet))
    xls1 = read_excel(encuesta, i)
    #pasar todo a character
    xls1 <- setNames(data.frame(lapply(xls1, as.character)),
                     colnames(xls1))
    #pasar los NA a *****
    xls1 <- xls1 %>% mutate(across(everything(), ~replace_na(.x, "-----")))
    #reemplazar los nombres de las variables
    xls1 <- xls1 %>% rename_all(funs(str_replace_all(., "/", " ")))
    #print(names(xls1[i]))

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

    ##Pasar de numeros a los valores del cuestionario
    for (i in names(xls1)) {
      if(sum(is.na(choices$label[match(xls1[[i]],choices$concated_column)])) < length(xls1[[3]])){
        xls1[i] <- choices$label[match(xls1[[i]],choices$concated_column)]
      }
    }

    ##Generar los dataframe con el mismo nombre de la hoja de excel
    ##(paste(namesheet, " ", sep = ""), xls1)
    assign(namesheet, xls1)

    ##lista <- list(data.table(assign(namesheet, xls1)))
    lista[[namesheet]] <- data.table(xls1)
  }

  ##Regrese la lista con los datos
  cat("\n")
  print("Funcion ejecutada con exito...")
  return(lista)
}
