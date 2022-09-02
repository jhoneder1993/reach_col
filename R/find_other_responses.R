#' Find Other Responses
#'
#' Encontrar todas las respuestas en todas las columnas que podrían ser "especificar otras" respuestas a una pregunta de opción múltiple
#'
#' @param data a dataframe
#' @param uuid como aparece el uuid en la base de datos
#'
#' @return Un dataframe con una fila por problema potencial. Tiene columnas para el índice de la fila correspondiente en los datos originales; el valor sospechoso; el nombre de la variable en el conjunto de datos original en el que se produjo el valor sospechoso; una descripción del tipo de problema.
#' @export
#'
#' @examples otros <- find_other_responses(base_datos)


find_other_responses <- function(data, uuid="uuid"){

  if (uuid %in% names(data)){
    # Cambiar nombre al uuid
    data <- dplyr::rename(data, uuid = uuid)
    # Otros. Palabras que se deben tener en cuenta
    select_other_columns <- function(data) {
      othernames <- grep("other$|Other$|otro$|Otro$|otr$|Otr$|^otro|^Otro|^otr_|^Otr_|uuid$", names(data),
                         value = T
      );othernames
      data[othernames]
    }

    # Calcular el numero de indice
    empty_issues_table <- function() {
      data.frame(
        index = numeric(), value = numeric(), variable = character(),
        has_issue = logical(), issue_type = character()
      )

    }


    # Revisar los otros
    counts<-data %>% select_other_columns
    if(ncol(counts) == 0){return(empty_issues_table())}
    counts <- counts %>% tidyr::gather("key", "value", -uuid)


    if(ncol(counts) == 0){return(empty_issues_table())}else{
      #%>% extract(.,colSums(!is.na(.))<nrow(.))
      counts %<>% filter(!is.na(value)) %>% filter(!value %in% c("", TRUE, FALSE, 1, 0, "TRUE", "FALSE", "<NA>", "NA", "n/a", "N/A"))

      counts %<>% group_by(key,value) %>% mutate(uuid = paste0(uuid, collapse = "; "))
      counts %<>% group_by(uuid, key,value) %>% summarise(count=length(value)) %>% filter(!is.na(value))

      #summarise_all(funs(sum, na.rm = T))

      others <- counts %>% as.data.frame

      if (nrow(others) == 0) {
        return(empty_issues_table())
      }

      others <- others %>% mutate(value = paste0(value," /// casos: ",count)) %>% select(uuid, variable = key, value)

      others <- data.frame(others[, c("uuid" ,"value", "variable")],
                           has_issue = T, issue_type = "'otra' respuesta. pueden necesitar recodificar.", stringsAsFactors = F)

      return(others)
    }
  }else{
    print("Revisar nuevamente no se encuentra el uuid suministrado")
  }
}

