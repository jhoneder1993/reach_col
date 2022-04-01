#' Comercio Full Reshape
#'
#' @param encuesta lista de las hojas que genero el kobo_to_r
#' @param etiquetas lista de las hojas que genero el import_rename
#' @param choices El parametro choices es la hoja Choices de la herramienta Kobo
#'
#' @return - Una lista con los dataframe de la hoja principal, abastecimiento, food y non_food en reshape;
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


comercio_full_reshape <- function(encuesta, etiquetas, choices){
  lista <- list()
  JMMI <- names(encuesta[1])
  abas <- as.data.frame(encuesta$abastecimientonuevo_repeat)
  repeat_abas <- as.data.frame(etiquetas$abastecimientonuevo_repeat)


  choices = choices %>% mutate(concated_column = paste(list_name, name, sep = ' '))
  ### Reshape para Pago_abas
  ## Se eliminan variables para la hoja abastecimiento
  borrar <- c("nombre_alimento_grupo", "_submission__submission_time", "_submission__validation_status", "_parent_index",
              "_parent_table_name", "_index", "_submission__id", "index1")
  repeat_abas <- repeat_abas[!(repeat_abas$Nombre %in% borrar),]
  abas <- abas[, !(names(abas)) %in% borrar]

  for(i in 1:nrow(abas)){
    if(replace_na(abas$id_alimento[i]) == "-999" | replace_na(abas$id_alimento[i], "") == "-888" | replace_na(abas$id_alimento[i], "") == "."){
      abas$id_alimento[i] <- NA
    }
  }

  #se deja los restantes sin los vacios
  abas <- abas[!(is.na(abas$id_alimento)),]

  #uuid repetidos
  duplicados1 <- abas %>% filter((id_alimento == "1" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados2 <- abas %>% filter((id_alimento == "2" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados3 <- abas %>% filter((id_alimento == "3" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados4 <- abas %>% filter((id_alimento == "4" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)

  duplicados <- bind_rows(duplicados1, duplicados2, duplicados3, duplicados4)

  print("Datos duplicados que fueron eliminados: ")
  print(duplicados["_submission__uuid"])
  cat("\n")

  ##Eliminar duplicados
  abas <- abas %>% filter(!(id_alimento == "1" & duplicated("_submission__uuid")))
  abas <- abas %>% filter(!(id_alimento == "2" & duplicated("_submission__uuid")))
  abas <- abas %>% filter(!(id_alimento == "3" & duplicated("_submission__uuid")))
  abas <- abas %>% filter(!(id_alimento == "4" & duplicated("_submission__uuid")))

  ##Hacer el reshape o pivot wider
  abas_reshape <- abas %>%
    pivot_wider(id_cols = `_submission__uuid`,#i
                names_from = id_alimento,#j
                values_from = c(capa_abste_nuevo, proveedor_nuevo, depart_nuevo,
                                pais_nuevo, otr_pais_nuevo, tipo_proveedor_nuevo,
                                otr_tipo_proveedor_nuevo))

  ##renombrar la columna _submission__uuid
  abas_reshape <- dplyr::rename(abas_reshape, "_uuid" = `_submission__uuid`)



  ###Reshape para Pago food
  food <- as.data.frame(encuesta$repeat_food_ant)
  repeat_food <- as.data.frame(etiquetas$repeat_food_ant)

  ##Se eliminan variables para la capa de food
  borrar <- c("nombre_alimento_ant", "_submission__submission_time", "_submission__validation_status", "_parent_index",
              "_parent_table_name", "_index", "_submission__id", "index1_my")
  repeat_food <- repeat_food[!(repeat_food$Nombre %in% borrar), ]
  food <- food[,!(names(food) %in% borrar)]

  for(i in 1:nrow(food)){
    if(replace_na(food$id_alimento_my[i], "") == "-999" | replace_na(food$id_alimento_my[i], "") == "-888" | replace_na(food$id_alimento_my[i], "") == "."){
      food$id_alimento_my[i] <- NA
    }
  }

  #se deja los restantes sin los vacios
  food <- food[!(is.na(food$id_alimento_my)),]

  #uuid repetidos
  duplicados1 <- food %>% filter((id_alimento_my == "1" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados2 <- food %>% filter((id_alimento_my == "2" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados3 <- food %>% filter((id_alimento_my == "3" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados4 <- food %>% filter((id_alimento_my == "4" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)

  duplicados <- bind_rows(duplicados1, duplicados2, duplicados3, duplicados4)

  print("Datos duplicados que fueron eliminados: ")
  print(duplicados["_submission__uuid"])
  cat("\n")


  ##Elininar duplicados
  food <- food %>% filter(!(id_alimento_my == "1" & duplicated("_submission__uuid")))
  food <- food %>% filter(!(id_alimento_my == "2" & duplicated("_submission__uuid")))
  food <- food %>% filter(!(id_alimento_my == "3" & duplicated("_submission__uuid")))
  food <- food %>% filter(!(id_alimento_my == "4" & duplicated("_submission__uuid")))


  ###Hacer el reshape o pivot wider
  food_reshape <- food %>%
    pivot_wider(id_cols = `_submission__uuid`,#i
                names_from = id_alimento_my,#j
                values_from = c(precio_alimento_ant, dias_exisnc_alimento_ant))

  ##renombrar la columna _submission__uuid
  food_reshape <- dplyr::rename(food_reshape, "_uuid" = `_submission__uuid`)




  ###Reshape para Pago non food
  non_food <- as.data.frame(encuesta$repeat_no_food_ant)
  repeat_non_food <- as.data.frame(etiquetas$repeat_no_food_ant)


  ##Se eliminan variables para la capa de food
  borrar <- c("nombre_no_alimento_ant", "_submission__submission_time", "_submission__validation_status", "_parent_index",
              "_parent_table_name", "_index", "_submission__id", "index2_my")
  repeat_non_food <- repeat_non_food[!(repeat_non_food$Nombre %in% borrar), ]
  non_food <- non_food[,!(names(non_food) %in% borrar)]


  for(i in 1:nrow(non_food)){
    if(replace_na(non_food$id_no_alimento_my[i], "") == "-999" | replace_na(non_food$id_no_alimento_my[i], "") == "-888" | replace_na(non_food$id_no_alimento_my[i], "") == "."){
      non_food$id_no_alimento_my[i] <- NA
    }
  }

  #se deja los restantes sin los vacios
  non_food <- non_food[!(is.na(non_food$id_no_alimento_my)),]

  #uuid repetidos
  duplicados1 <- non_food %>% filter((id_no_alimento_my == "1" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados2 <- non_food %>% filter((id_no_alimento_my == "2" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados3 <- non_food %>% filter((id_no_alimento_my == "3" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)
  duplicados4 <- non_food %>% filter((id_no_alimento_my == "4" & duplicated("_submission__uuid"))) %>% select(`_submission__uuid`)

  duplicados <- bind_rows(duplicados1, duplicados2, duplicados3, duplicados4)

  print("Datos duplicados que fueron eliminados: ")
  print(duplicados["_submission__uuid"])
  cat("\n")


  ##Elininar duplicados
  non_food <- non_food %>% filter(!(id_no_alimento_my == "1" & duplicated("_submission__uuid")))
  non_food <- non_food %>% filter(!(id_no_alimento_my == "2" & duplicated("_submission__uuid")))
  non_food <- non_food %>% filter(!(id_no_alimento_my == "3" & duplicated("_submission__uuid")))
  non_food <- non_food %>% filter(!(id_no_alimento_my == "4" & duplicated("_submission__uuid")))


  ###Hacer el reshape o pivot wider
  non_food_reshape <- non_food %>%
    pivot_wider(id_cols = `_submission__uuid`,#i
                names_from = id_no_alimento_my,#j
                values_from = c(precio_no_alimento_ant, dias_exisnc_no_alimento_ant))

  ##renombrar la columna _submission__uuid
  non_food_reshape <- dplyr::rename(non_food_reshape, "_uuid" = `_submission__uuid`)


  ###Hacer una salida de la lista con los 6 dataframe
  lista[[JMMI]] <- as.data.frame(encuesta[[1]])
  JMMI <- paste(JMMI, "labels", sep = "_")
  lista[[JMMI]] <- as.data.frame(etiquetas[[1]])

  lista["abastecimiento_reshape"] <- list(abas_reshape)
  lista["abastecimiento_labels"] <- list(repeat_abas)

  lista["food_reshape"] <- list(food_reshape)
  lista["food_labels"] <- list(repeat_food)

  lista["non_food_reshape"] <- list(non_food_reshape)
  lista["non_food_labels"] <- list(repeat_non_food)

  ##salida de la lista
  print("Funcion ejecutada con exito...")

  return(lista)
}
