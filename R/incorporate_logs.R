#' Incorporate Logs
#'
#' Sirve para realizar los ajustes a la base de datos segun el uuid, variable, valor anterior y valor nuevo. Tambien se debe tener en cuenta en el archivo de cleaning_log que los ajustes se realizaran a los que tengan 'si' en la variable changed
#'
#' @param raw_df dataframe
#' @param cleaning_log dataframe de los cambios a realizar
#' @param df_group_seprator Si en los multiples esta separado por "/", " ", "."
#' @param uuid_col Como se encuentra escrito el uuid en la base de datos
#'
#' @return Lista con el archivo cleaned_df que es la base de datos a la cual se le realizo la limpieza
#' @return Lista con el archivo master_cleaning_log de los registros que se aplicaron o no se aplicaron a los datos
#' @return Lista con el archivo logs_not_in_rawDF de los registros cuyo nombre de la pregunta o uuid no esta disponible en los base de datos
#' @return Lista con el archivo cleaning_log.appli de los registros de limpieza que se aplico a los datos brutos
#' @return Lista con el archivo duplicate_logs de los registros duplicados
#' @export
#'
#' @examples incorprated_logs <- incorporate_logs(my_df, cleaning_log, uuid_col = "uuid")


### declaring the function
incorporate_logs = function(raw_df, cleaning_log, df_group_seprator = "/", uuid_col = "_uuid"){
  error <- "Error!
La ejecución se detuvo debido a uno de los siguientes problemas::
  - El registro de limpieza está vacío
  - No hay cambios en los datos (en el registro de limpieza la propiedad 'changed' para todos los registros está establecida en 'NO')
  - Falta una/más de una de las columnas (uuid, question.name, old.value, new.value y changed) o los nombres de las columnas están mal escritos
"
  if (sum(grepl("uuid|question.name|old.value|new.value|changed", names(cleaning_log)))==5) {
    `%nin%` = Negate(`%in%`)
    # changing the group seprator (operator) from "/" to "."
    names(raw_df) <- gsub(df_group_seprator,".",names(raw_df))
    cleaning_log$question.name <- gsub(df_group_seprator,".", cleaning_log$question.name)

    # subsetting logs that their question is not (available) in dataset
    logs_not_in_rawdf <- cleaning_log[cleaning_log$question.name %nin% names(raw_df) | cleaning_log$uuid %nin% raw_df[[uuid_col]], ]
    logs_not_in_rawdf <- logs_not_in_rawdf[logs_not_in_rawdf$changed %in% c("yes","Yes", "si", "Si"),]

    # subsetting logs that their question exist in raw data frame and its new value is changed
    cleaning_log.changed <- cleaning_log[cleaning_log$question.name %in% names(raw_df) & cleaning_log$uuid %in% raw_df[[uuid_col]], ]
    cleaning_log.changed <- cleaning_log.changed[cleaning_log.changed$changed %in% c("yes","Yes", "si", "Si"),]

    # capturing duplicate logs
    cleaning_log$unique_key <- paste(cleaning_log$uuid, cleaning_log$question.name, sep = "_")
    duplicate_logs <- cleaning_log[(duplicated(cleaning_log$unique_key) | duplicated(cleaning_log$unique_key, fromLast = T)),]

    # cleaning master cleaning log
    cleaning_log <- cleaning_log[cleaning_log$uuid %nin% logs_not_in_rawdf$uuid | cleaning_log$question.name %nin% logs_not_in_rawdf$question.name,]
    cleaning_log <- cleaning_log[!is.na(cleaning_log$question.name), ]
    cleaning_log <- cleaning_log[!is.na(cleaning_log$uuid), ]

    raw_df_valid <- raw_df
    if (nrow(cleaning_log.changed)>0) {
      # Apply cleaning log on raw data
      for (rowi in 1:nrow(cleaning_log.changed)){
        uuid_i <- cleaning_log.changed$uuid[rowi]
        var_i <- cleaning_log.changed$question.name[rowi]
        old_i <- cleaning_log.changed$old.value[rowi]
        new_i <- cleaning_log.changed$new.value[rowi]
        if(class(raw_df_valid[[var_i]]) == "character"){
          new_i <- as.character(new_i)
        }else if(class(raw_df_valid[[var_i]]) == "numeric"){
          new_i <- as.numeric(new_i)
        }else if(class(raw_df_valid[[var_i]]) == "logical"){
          new_i <- as.integer(new_i)
        }else if(class(raw_df_valid[[var_i]]) == "integer"){
          new_i <- as.integer(new_i)
        }
        # Find the variable according to the row of the cleaning log
        raw_df_valid[raw_df_valid[[uuid_col]] == uuid_i, var_i] <- new_i
        print(paste(rowi,"uuid:", uuid_i, "valor anterior:", old_i, "cambio a", new_i, "para", var_i))
      }
      return(list(cleaned_df = raw_df_valid, cleaning_log.applied = cleaning_log.changed, logs_not_in_rawDF = logs_not_in_rawdf, duplicate_logs = duplicate_logs, master_cleaning_log = cleaning_log))
    }else{
      cat(error)
      return(list(cleaned_df = raw_df_valid, cleaning_log.applied = cleaning_log.changed,logs_not_in_rawdf = logs_not_in_rawdf))
    }
  }else{
    cat(error)
  }
}
