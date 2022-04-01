#' Comercio Checks
#'
#' Sirve para identificar los datos atipicos de precios y valores atipicos de la base de datos
#'
#' @param base_datos La base de datos despues de haber realizado el cleaning
#'
#' @return InventariosLocos
#' @return PreciosLocos
#' @return OTROS
#' @return GISNombres
#' @return Feekback en excel por organizaciones
#' @export
#'
#' @examples base_cleaning <- cleaning_alojamiento(base_datos_reshape)
#'
#' @examples base_outliers <- checks_alojamiento(base_cleaning)
#' @examples base_outliers <- checks_alojamiento(base_datos = base_cleaning)


comercio_checks <- function(base_datos){
  lista <- list()

  ## Eliminar datos donde no dieron concentimiento
  base_datos <- base_datos %>% filter(!replace_na(base_datos[["consentimiento"]], "") == "No")

  ## PRODUCTOS PERECEDEROS MAS DE UN MES DE INVENTARIO
  base_datos2 <- base_datos

  # Se dejan las variables a analizar
  dejar <- c("_index", "_uuid", "inicio", "final", "municipio", "nombre_mercado", "nombre_negocio",
             "encuestador", "fecha_encuesta", "organizacion", "dias_exisnc_alimento_ant_1",
             "dias_exisnc_alimento_ant_2", "dias_exisnc_alimento_ant_3", "dias_exisnc_alimento_ant_4",
             "dias_exisnc_alimento_ant_5", "dias_exisnc_alimento_ant_6", "dias_exisnc_alimento_ant_7",
             "dias_exisnc_alimento_ant_8", "dias_exisnc_alimento_ant_9", "dias_exisnc_alimento_ant_10",
             "dias_exisnc_alimento_ant_11", "dias_exisnc_alimento_ant_12", "dias_exisnc_alimento_ant_13",
             "dias_exisnc_alimento_ant_14", "dias_exisnc_alimento_ant_15")

  base_datos2 <- base_datos2[,(names(base_datos2)) %in% dejar]

  # Filtrar si tienen mas de 20 dias
  base_datos2 <- base_datos2 %>%
    filter(as.numeric(dias_exisnc_alimento_ant_1) > 20 | as.numeric(dias_exisnc_alimento_ant_2) > 20 |
             as.numeric(dias_exisnc_alimento_ant_3) > 20 | as.numeric(dias_exisnc_alimento_ant_4) > 20 |
             as.numeric(dias_exisnc_alimento_ant_5) > 20 | as.numeric(dias_exisnc_alimento_ant_6) > 20 |
             as.numeric(dias_exisnc_alimento_ant_7) > 20 | as.numeric(dias_exisnc_alimento_ant_8) > 20 |
             as.numeric(dias_exisnc_alimento_ant_9) > 20 | as.numeric(dias_exisnc_alimento_ant_10) > 20 |
             as.numeric(dias_exisnc_alimento_ant_11) > 20 | as.numeric(dias_exisnc_alimento_ant_12) > 20 |
             as.numeric(dias_exisnc_alimento_ant_13) > 20 | as.numeric(dias_exisnc_alimento_ant_14) > 20 |
             as.numeric(dias_exisnc_alimento_ant_15) > 20)

  lista[["InventariosLocos"]] <- base_datos2


  # Precios locos
  base_datos3 <- base_datos

  variables <- c("precio_alimento_ant_1", "precio_alimento_ant_2", "precio_alimento_ant_3",
                 "precio_alimento_ant_4", "precio_alimento_ant_5", "precio_alimento_ant_6",
                 "precio_alimento_ant_7", "precio_alimento_ant_8", "precio_alimento_ant_9",
                 "precio_alimento_ant_10", "precio_alimento_ant_11", "precio_alimento_ant_12",
                 "precio_alimento_ant_13", "precio_alimento_ant_14", "precio_alimento_ant_15",
                 "precio_no_alimento_ant_1", "precio_no_alimento_ant_2", "precio_no_alimento_ant_3",
                 "precio_no_alimento_ant_4", "precio_no_alimento_ant_5", "precio_no_alimento_ant_6",
                 "precio_no_alimento_ant_7", "precio_no_alimento_ant_8")

  if (nrow(base_datos3) > 0){
    for (i in variables){
      # Nombre de la nueva columna
      name <- paste("Z", i, sep = "_")
      #valor de la columna
      valor <- as.numeric(base_datos3[[i]]);
      #promedio de la columna de la que toma el valor
      mean_i <- mean(as.numeric(base_datos3[[i]]), na.rm = TRUE)
      #des. estandar de la columna de la que toma el valor
      sd_i <- sd(as.numeric(base_datos3[[i]]), na.rm = TRUE)

      # Nuevo valor de la columna calculado
      base_datos3[[name]] <- (abs(valor - mean_i) / sd_i)

      # Datos > 2 van a quedar con 1, o sea atipicos
      for (j in 1:nrow(base_datos3)) {
        if(replace_na(base_datos3[[name]][j], 0) > 2){
          base_datos3[[name]][j] <- 1
        }else{
          base_datos3[[name]][j] <- 0
        }
      }
    }

    dejar <- c("_index", "_uuid", "inicio", "final", "municipio", "nombre_mercado", "nombre_negocio",
               "encuestador", "fecha_encuesta", "organizacion",
               "precio_alimento_ant_1", "precio_alimento_ant_2", "precio_alimento_ant_3",
               "precio_alimento_ant_4", "precio_alimento_ant_5", "precio_alimento_ant_6",
               "precio_alimento_ant_7", "precio_alimento_ant_8", "precio_alimento_ant_9",
               "precio_alimento_ant_10", "precio_alimento_ant_11", "precio_alimento_ant_12",
               "precio_alimento_ant_13", "precio_alimento_ant_14", "precio_alimento_ant_15",
               "precio_no_alimento_ant_1", "precio_no_alimento_ant_2", "precio_no_alimento_ant_3",
               "precio_no_alimento_ant_4", "precio_no_alimento_ant_5", "precio_no_alimento_ant_6",
               "precio_no_alimento_ant_7", "precio_no_alimento_ant_8", "Z_precio_alimento_ant_1", "Z_precio_alimento_ant_2",
               "Z_precio_alimento_ant_3", "Z_precio_alimento_ant_4", "Z_precio_alimento_ant_5", "precio_alimento_ant_6",
               "Z_precio_alimento_ant_7", "Z_precio_alimento_ant_8", "Z_precio_alimento_ant_9",
               "Z_precio_alimento_ant_10", "Z_precio_alimento_ant_11", "Z_precio_alimento_ant_12",
               "Z_precio_alimento_ant_13", "Z_precio_alimento_ant_14", "Z_precio_alimento_ant_15",
               "Z_precio_no_alimento_ant_1", "Z_precio_no_alimento_ant_2", "Z_precio_no_alimento_ant_3",
               "Z_precio_no_alimento_ant_4", "Z_precio_no_alimento_ant_5", "Z_precio_no_alimento_ant_6",
               "Z_precio_no_alimento_ant_7", "Z_precio_no_alimento_ant_8")

    base_datos3 <- base_datos3[,(names(base_datos3)) %in% dejar]

    base_datos3 <- base_datos3 %>% filter(Z_precio_alimento_ant_1 == 1 | Z_precio_alimento_ant_2 == 1 |
                                            Z_precio_alimento_ant_3 == 1 | Z_precio_alimento_ant_4 == 1 | Z_precio_alimento_ant_5 == 1 |
                                            precio_alimento_ant_6 == 1 | Z_precio_alimento_ant_7 == 1 | Z_precio_alimento_ant_8 == 1 |
                                            Z_precio_alimento_ant_9 == 1 | Z_precio_alimento_ant_10 == 1 | Z_precio_alimento_ant_11 == 1 |
                                            Z_precio_alimento_ant_12 == 1 | Z_precio_alimento_ant_13 == 1 | Z_precio_alimento_ant_14 == 1 |
                                            Z_precio_alimento_ant_15 == 1 | Z_precio_no_alimento_ant_1 == 1 | Z_precio_no_alimento_ant_2 == 1 |
                                            Z_precio_no_alimento_ant_3 == 1 | Z_precio_no_alimento_ant_4 == 1 | Z_precio_no_alimento_ant_5 == 1 |
                                            Z_precio_no_alimento_ant_6 == 1 | Z_precio_no_alimento_ant_7 == 1 | Z_precio_no_alimento_ant_8 == 1)

    lista[["PreciosLocos"]] <- base_datos3

    #print avance
    print("La base de datos 'PreciosLocos' se ha ejecutado de manera correcta.")
    cat("\n")

  } else {
    #print avance
    print("No hay datos para 'PreciosLocos'. se ha ejecutado de manera correcta.")
    cat("\n")
  }

  # Revisar otros
  base_datos4 <- base_datos

  dejar <- c("_index", "_uuid", "inicio", "final", "municipio", "nombre_mercado", "nombre_negocio",
             "encuestador", "fecha_encuesta")

  base_datos4 <- base_datos4[,(names(base_datos4)) %in% dejar | (names(base_datos4)) %ilike% "otr"]
  base_datos4 <- base_datos4 %>% select(-otravez_encuesta)

  base_datos4 <- base_datos4 %>% filter(!is.na(otr_clase_mercado) | !is.na(otr_medios_pago) | !is.na(problemas_acceso_otro ) |
                                          !is.na(otr_aumento_precio) | !is.na(otr_baja_precio) | !is.na(otr_desafios) | !is.na(otr_desafs_reabast) |
                                          !is.na(otr_transporte) | !is.na(otr_pais_nuevo_1) | !is.na(otr_tipo_proveedor_nuevo_1) |
                                          !is.na(otr_pais_nuevo_2) | !is.na(otr_tipo_proveedor_nuevo_2) | !is.na(otr_pais_nuevo_3) |
                                          !is.na(otr_tipo_proveedor_nuevo_3) | !is.na(otr_pais_nuevo_4) | !is.na(otr_tipo_proveedor_nuevo_4) |
                                          !is.na(otr_pais_nuevo_5) | !is.na(otr_tipo_proveedor_nuevo_5) | !is.na(otr_pais_nuevo_6) |
                                          !is.na(otr_tipo_proveedor_nuevo_6))

  lista[["OTROS"]] <- base_datos4

  ### Guardar bases de datos segun ORG
  org <- base_datos %>% select(organizacion) %>% unique()

  for (i in 1:nrow(org)) {
    filtro <- base_datos %>% filter(organizacion %in% org[[1]][i])
    dir_filtro <- writexl::write_xlsx(list("Base de datos" = filtro),
                                      paste("Result/Clean_data/Feedback/Formato limpieza_Comerciantes_", org[[1]][i] ,".xlsx", sep = ""))
  }

  #print avance
  print("La bases de datos segun ORG 'Result/Clean_data/Feedback/Formato limpieza_Comerciantes_..' se han guardado de manera correcta.")
  cat("\n")


  ## GIS TEMPORAL
  base_datos5 <- base_datos

  # Se dejan las variables a analizar
  dejar <- c("_index", "_uuid", "departamento", "municipio", "zona_comercio", "depart_nuevo_1", "pais_nuevo_1",
             "depart_nuevo_2", "pais_nuevo_2", "depart_nuevo_3", "pais_nuevo_3", "depart_nuevo_4", "pais_nuevo_4",
             "depart_nuevo_5", "pais_nuevo_5", "depart_nuevo_6", "pais_nuevo_6")

  base_datos5 <- base_datos5[,(names(base_datos5)) %in% dejar]

  lista[["GISNombres"]] <- base_datos5

  #print avance
  print("La base de datos 'GISNombres' se ha ejecutado de manera correcta.")
  cat("\n")


  ## Guardar las base de datos

  dir_base_datos <- writexl::write_xlsx(list("InventariosLocos" = base_datos2,
                                             "PreciosLocos" = base_datos3,
                                             "OTROS" = base_datos4,
                                             "GIS_Nombres" = base_datos5),
                                        paste("Result/Clean_data/Check/REACH_Checks_JMMICOL_R4_Total.xlsx", sep = ""))

  #print avance
  print("La bases de datos se han guardado en el archivo 'Result/Clean_data/Check/REACH_Checks_JMMICOL_R4_Total_...' de manera correcta.")
  cat("\n")


  ##salida de la lista
  print("Funcion ejecutada con exito...")

  ##Return la lista con toda la informacion
  return(lista)
}
