#' Import Rename
#'
#' La funcion toma la salida generada con el kobo_to_r, de la herramienta kobo toma choices y survey para generar una lista con los nombre y etiquetas de la base de datos
#'
#' @param encuesta lista de las hojas que genero el kobo_to_r
#' @param choices El parametro choices es la hoja Choices de la herramienta Kobo
#' @param survey El parametro choices es la hoja Survey de la herramienta Kobo
#'
#' @return - Una lista con los nombres de las variables y su descripcion
#' @export
#'
#' @examples choices <- read_excel("Input/REACH_JMMI_Survey.xlsx", 'choices')
#' @examples survey <- read_excel("Input/REACH_JMMI_Survey.xlsx", 'survey')
#'
#' @examples base_datos <- kobo_to_r(directorio2, choices)
#'
#' @examples etiquetas <- import_raname(base_datos, choices, survey)
#' @examples etiquetas <- import_raname(base_datos = base_datos, choices = choices, survey = survey)


import_rename <- function(encuesta, choices, survey){
  choices = choices %>% mutate(concated_column = paste(list_name, name, sep = ' '))
  lista <- list()
  ## 0. Import_rename
  for(i in 1:length(encuesta)){
    namesheet <- names(base_datos[i])
    print(paste('INICIAMOS EL dataframe', i, ":", namesheet))
    xls1 = as.data.frame(encuesta[[i]])
    #xls1 = xls1 %>% rename_all(funs(str_replace_all(., " ", "")))

    #print(names(xls1[i]))

    dataset <- data.frame(Type = character(),
                          Nombre1 = character(),
                          Nombre2 = character(),
                          Etiqueta = character())

    ##Colocar los nombres de la encuesta en fila
    for (j in 1:length(names(xls1))) {
      dataset <- dataset %>% add_row(Nombre1 = names(xls1[j]))
    }

    #Colocar el tipo de dato que es para despues obterner los labels
    for (a in 1:length(dataset$Nombre1)) {
      for (b in 1:length(survey$name)){
        new_name <- str_split(dataset$Nombre1[a], " ")
        new_name <- new_name[[1]][1]
        if (new_name == (replace_na(survey$name[b], ""))) {
          dataset$Type[a] <- survey$type[b]
        }
      }
    }

    ##Colocar los nombres (multiple) para ingresar a la busqueda de los labesl
    for(i in 1:length(dataset$Type)){
      if(str_detect(replace_na(dataset$Type[i], ""), "multiple")){
        numero_multiple <- str_split(dataset$Nombre1[i], " ")
        numero_multiple <- replace_na(numero_multiple[[1]][2], "")
        nombre_multiple <- str_split(dataset$Type[i], " ")
        nombre_multiple <- nombre_multiple[[1]][2]
        new_name <- paste(nombre_multiple, numero_multiple, sep=" ")
        dataset$Nombre2[i] <- new_name
      }else{
        dataset$Nombre2[i] <- dataset$Nombre1[i]
      }
    }

    #Colocar las etiquetas
    for (i in 1:length(dataset$Nombre2)) {
      if (dataset$Nombre2[i] %in% survey$name){
        for(b in 1:length(survey$name)){
          if(replace_na(dataset$Nombre2[i], "") == replace_na(survey$name[b], "")){
            dataset$Etiqueta[i] <- survey$label[b]
          }
        }
      }else if (replace_na(dataset$Nombre2[i], "") %in% choices$concated_column){
        for(c in 1:length(choices$concated_column)){
          if(dataset$Nombre2[i] == choices$concated_column[c]){
            dataset$Etiqueta[i] <- choices$label[c]
          }
        }
      }else {
        for(b in 1:length(survey$name)){
          if(replace_na(dataset$Nombre1[i], "") == replace_na(survey$name[b], "")){
            dataset$Etiqueta[i] <- survey$label[b]
          }
        }
      }
    }

    ##Dejar el dataset solamente con Nombre y Etiqueta
    dataset <- dataset %>% select(Nombre = Nombre1, label_name =Nombre2, Etiqueta) %>%
      mutate(Nombre = gsub(" ", "", Nombre))

    #Generar los dataframe con el mismo nombre de la hoja de excel
    #(paste(x, " ", sep = ""), dataset)
    assign(namesheet, dataset)

    ##lista <- list(data.table(assign(namesheet, dataset)))
    lista[[namesheet]] <- data.table(dataset)
  }

  ##Regrese la lista con los datos
  cat("\n")
  print("Funcion ejecutada con exito...")
  return(lista)
}
