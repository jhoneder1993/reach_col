#' Alojamiento Full Reshape
#'
#' @param encuesta lista de las hojas que genero el kobo_to_r
#' @param etiquetas lista de las hojas que genero el import_rename
#' @param choices El parametro choices es la hoja Choices de la herramienta Kobo
#'
#' @return - Una lista con los dataframe de la hoja principal, servicios y facilidades en reshape;
#' @return - una lista con los nombres de las variables y su descripcion
#' @export
#'
#' @examples choices <- read_excel("Input/REACH_JMMI_Survey.xlsx", 'choices')
#'
#' @examples base_datos <- kobo_to_r(directorio2, choices)
#'
#' @examples etiquetas <- import_raname(base_datos, choices, survey)
#'
#' @examples base_datos_reshape <- full_reshape(base_datos, etiquetas, choices)
#' @examples base_datos_reshape <- full_reshape(base_datos = base_datos, etiquetas = etiquetas, choices = choices)


alojamiento_full_reshape <- function(encuesta, etiquetas, choices){
  lista <- list()
  JMMI <- names(encuesta[1])
  servicios <- as.data.frame(encuesta$pago_servicios_grp)
  repeat_servicios <- as.data.frame(etiquetas$pago_servicios_grp)

  choices = choices %>% mutate(concated_column = paste(list_name, name, sep = ' '))
  ###Reshape para Pago_servicios
  ##Se eliminan variables para la hoja Pago_servicios
  borrar <- c("index2", "nombre_servicio", "_index", "_parent_table_name", "_parent_index", "_submission__id",
              "_submission__submission_time", "_submission__validation_status", "_submission__notes",
              "_submission__status", "_submission__submitted_by", "_submission__tags")
  repeat_servicios <- repeat_servicios[!(repeat_servicios$Nombre %in% borrar),]
  servicios <- servicios[, !(names(servicios)) %in% borrar]

  for(i in 1:nrow(servicios)){
    if(replace_na(servicios$id_servicios[i]) == "-999" | replace_na(servicios$id_servicios[i], "") == "-888"){
      servicios$id_servicios[i] <- NA
    }
  }

  #se deja los restantes sin los vacios
  servicios <- servicios[!(is.na(servicios$id_servicios)),]

  #uuid repetidos
  duplicados1 <- servicios %>% filter((id_servicios == "1" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados2 <- servicios %>% filter((id_servicios == "2" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados3 <- servicios %>% filter((id_servicios == "3" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados4 <- servicios %>% filter((id_servicios == "4" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)

  duplicados <- bind_rows(duplicados1, duplicados2, duplicados3, duplicados4)

  ##Elininar duplicados
  servicios <- servicios %>% filter(!(id_servicios == "1" & duplicated("_submission__uuid")))
  servicios <- servicios %>% filter(!(id_servicios == "2" & duplicated("_submission__uuid")))
  servicios <- servicios %>% filter(!(id_servicios == "3" & duplicated("_submission__uuid")))
  servicios <- servicios %>% filter(!(id_servicios == "4" & duplicated("_submission__uuid")))

  print("Datos duplicados que fueron eliminados: ")
  print(duplicados["_submission__uuid"])
  cat("\n")

  ##Hacer el reshape o pivot wider
  servicios_reshape <- servicios %>%
    pivot_wider(id_cols = `_submission__uuid`,#i
                names_from = id_servicios,#j
                values_from = c(servicios_pago, modo_pago_servicio, valor_servicio))

  ##renombrar la columna _submission__uuid
  servicios_reshape <- dplyr::rename(servicios_reshape, "_uuid" = `_submission__uuid`)



  ###Reshape para Pago facilidades
  facilidades <- as.data.frame(encuesta$pago_facilidades)
  repeat_facilidades <- as.data.frame(etiquetas$pago_facilidades)

  ##Se eliminan variables para la capa de facilidades
  borrar <- c("_submission__submission_time", "_submission__validation_status", "_parent_index",
              "_parent_table_name", "_index", "_submission__id", "index3")
  repeat_facilidades <- repeat_facilidades[!(repeat_facilidades$Nombre %in% borrar), ]
  facilidades <- facilidades[,!(names(facilidades) %in% borrar)]

  for(i in 1:nrow(facilidades)){
    if(replace_na(facilidades$id_facilidades[i], "") == "-999" | replace_na(facilidades$id_facilidades[i], "") == "-888"){
      facilidades$id_facilidades[i] <- NA
    }
  }

  #se deja los restantes sin los vacios
  facilidades <- facilidades[!(is.na(facilidades$id_facilidades)),]

  ##Elininar duplicados
  facilidades <- facilidades %>% filter(!(id_facilidades == "1" & duplicated("_submission__uuid")))
  facilidades <- facilidades %>% filter(!(id_facilidades == "2" & duplicated("_submission__uuid")))
  facilidades <- facilidades %>% filter(!(id_facilidades == "3" & duplicated("_submission__uuid")))
  facilidades <- facilidades %>% filter(!(id_facilidades == "4" & duplicated("_submission__uuid")))

  #uuid repetidos
  duplicados1 <- facilidades %>% filter((id_facilidades == "1" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados2 <- facilidades %>% filter((id_facilidades == "2" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados3 <- facilidades %>% filter((id_facilidades == "3" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados4 <- facilidades %>% filter((id_facilidades == "4" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)

  duplicados <- bind_rows(duplicados1, duplicados2, duplicados3, duplicados4)

  print("Datos duplicados que fueron eliminados: ")
  print(duplicados["_submission__uuid"])
  cat("\n")

  #Agregar al id_facilidades el valor_facilidades
  facilidades["id_facilidades"] <- paste("valor_facilidades", facilidades[["id_facilidades"]], sep = "_")

  ###Hacer el reshape o pivot wider
  facilidades_reshape <- facilidades %>%
    pivot_wider(id_cols = `_submission__uuid`,#i
                names_from = id_facilidades,#j
                values_from = c(valor_facilidades))

  ##renombrar la columna _submission__uuid
  facilidades_reshape <- dplyr::rename(facilidades_reshape, "_uuid" = `_submission__uuid`)



  ###Hacer una salida de la lista con los 6 dataframe
  lista[[JMMI]] <- as.data.frame(encuesta[[1]])
  JMMI <- paste(JMMI, "labels", sep = "_")
  lista[[JMMI]] <- as.data.frame(etiquetas[[1]])

  lista["servicios_reshape"] <- list(servicios_reshape)
  lista["servicios_labels"] <- list(repeat_servicios)

  lista["facilidades_reshape"] <- list(facilidades_reshape)
  lista["facilidades_labels"] <- list(repeat_facilidades)

  ##salida de la lista
  print("Funcion ejecutada con exito...")

  return(lista)
}
