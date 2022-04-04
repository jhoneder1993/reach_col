#' Find Outliers
#'
#' Busca valores que estén fuera de más de tres desviaciones estándar de la media.
#' Si se encuentran menos valores atípicos cuando los datos se transforman en logaritmos antes de la comprobación, sólo se devuelven los valores atípicos en los datos transformados en logaritmos.
#'
#' @param data Un dataframe
#' @param sd_max_p Desviacion estandar, por default es 3, pero se puede ajustar
#' @param uuid Nombre de la variable uuid como se encuentra en la base de datos
#'
#' @return Un dataframe con una fila por problema potencial. Tiene columnas para el índice de fila correspondiente en los datos originales; el valor sospechoso; el nombre de la variable en el conjunto de datos original en el que se produjo el valor sospechoso; una descripción del tipo de problema.
#' @export
#'
#' @examples


find_outliers <- function(data, sd_max_p=3, uuid="uuid") {

  if (uuid %in% names(data)){
    # Funcion para validacion normal
    data_validation_outliers_normal <- function(data, sd_max=sd_max_p) {
      outliers_normal <- data %>% lapply(outliers.numerical, sd_max = sd_max)
      return(outliers_normal)
    }

    # Funcion para validacion con log_normal
    data_validation_outliers_log_normal <- function(data, sd_max=sd_max_p) {
      outliers_log_normal <- data %>% lapply(log.outliers.numerical, sd_max = sd_max)
      return(outliers_log_normal)
    }

    # detecting outliers:
    outliers.numerical <- function(x, sd_max=sd_max_p) {
      # IN:
      # x: numerical vector
      # sd_max: integer
      # out: vector of indicies, of values in x that deviate more than sd_max from the mean.

      x <- gsub(" ","",x)
      x<- suppressWarnings(as.numeric(as.character(x))) # as.character to prevent factors from being treated is factor level ID integer
      x_data_only<-hasdata(x)
      x_data_only_indices<-hasdata(x,return.index = T)
      outliers_indices_in_data_only<-which(abs(x_data_only-mean(x_data_only))>sd_max*sd(x_data_only)& length(unique(x_data_only))>10)
      outliers_indices_in_original_vector<-x_data_only_indices[outliers_indices_in_data_only]
      return(
        cbind(
          index=outliers_indices_in_original_vector,
          value=x[outliers_indices_in_original_vector]) %>% as.data.frame(stringsAsFactors=F))

    }

    log.outliers.numerical <- function(x, sd_max=sd_max_p) {
      # IN:
      # x: numerical vector
      # sd_max: integer
      # out: vector of indicies, of values in x that deviate more than sd_max from the mean.
      x<- gsub(" ","",x)
      x<- suppressWarnings(as.numeric(as.character(x))) # as.character to prevent factors from being treated is factor level ID integer
      x_not_logged<-x
      x <- suppressWarnings(log(x))
      x_data_only <- hasdata(x)
      x_data_only_indices <- hasdata(x, return.index = T)
      outliers_indices_in_data_only <- which(abs(x_data_only - mean(x_data_only)) > sd_max * sd(x_data_only) & length(unique(x_data_only)) > 10)
      outliers_indices_in_original_vector <- x_data_only_indices[outliers_indices_in_data_only]
      return(
        cbind(

          index=outliers_indices_in_original_vector,
          value=x_not_logged[outliers_indices_in_original_vector]) %>% as.data.frame(stringsAsFactors=F))
    }

    # removes NA, empty strings and non-finite values from a vector
    hasdata <- function(x, return.index = F) {
      # in: vector of any class
      # out: the in vector without NULL,NA, or ""
      index <- which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))
      value <- x[which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))]
      if (return.index) {
        return(index)
      }
      return(value)
    }

    # Calcular el indice
    empty_issues_table <- function() {
      data.frame(
        index = numeric(), value = numeric(), variable = character(),
        has_issue = logical(), issue_type = character()
      )

    }



    ## calculate both normal and log normal outliers for the whole dataframe
    outliers_normal <- data %>% data_validation_outliers_normal()
    outliers_log_normal <- data %>% data_validation_outliers_log_normal()
    outliers <- lapply(names(data), function(x) {
      ## return an empty issues dataframe of issues if no outliers are found
      if ((nrow(outliers_log_normal[[x]]) == 0) & (nrow(outliers_normal[[x]]) ==
                                                   0)) {
        return(empty_issues_table())
      }
      else if (nrow(outliers_log_normal[[x]]) < nrow(outliers_normal[[x]])) { ## for each variable, select the method with fewer outliers

        data.frame(outliers_log_normal[[x]],
                   variable = rep(x, nrow(outliers_log_normal[[x]])), # rep(...,nrow()) makes this work for no rows etc.
                   has_issue = rep(T, nrow(outliers_log_normal[[x]])),
                   issue_type = rep("log normal distribution outlier", nrow(outliers_log_normal[[x]])), stringsAsFactors = F
        )
      }
      else {
        data.frame(outliers_normal[[x]],
                   variable = rep(x, nrow(outliers_normal[[x]])),
                   has_issue = rep(T, nrow(outliers_normal[[x]])),
                   issue_type = rep("normal distribution outlier", nrow(outliers_normal[[x]])),
                   stringsAsFactors = F
        )

      }
    }) %>% do.call(rbind, .)
    if (nrow(outliers) == 0) {
      return(empty_issues_table())
    }
    outliers$variable <- as.character(outliers$variable)
    if (uuid %in% names(data)){
      for (i in 1:nrow(outliers)){
        outliers[["uuid"]][i] <- data[[uuid]][outliers[["index"]][i]]
      }
    }else{
      print("El uuid mencionado no se encuntra en la base de datos, verifique que lo haya escrito correctamente")
    }
    return(outliers)
  }else{
    print("Revisar nuevamente no se encuentra el uuid suministrado")
  }
}
