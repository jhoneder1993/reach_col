#' Find Similar Surveys
#'
#' compara cada encuesta con cada una de las otras encuestas del conjunto de datos y encuentra la más similar, es decir, la que tiene el menor número de respuestas diferentes.
#'
#'
#' @param raw.data Un dataframe
#' @param tool.survey el survey de la herramienta kobo
#' @param uuid como aparece el uuid
#'
#' @return La función devuelve un marco de datos con el mismo número de filas (todas las encuestas) y algunas columnas adicionales que indican el ID de la encuesta más similar y cuántas columnas son diferentes.
#' @export
#'
#' @examples survey <- read_excel("Input/Kobo_ABA_GN2_JM_V22.xlsx", sheet = "survey")
#' @examples data <- read_excel("Input/encuesta.xlsx")
#' @examples similares <- find_similar_surveys(data, survey, "uuid")


find_similar_surveys <- function(raw.data, tool.survey, uuid="_uuid"){
  # 1) store UUIDs
  uuids <- raw.data[[uuid]]

  # 2) convert all columns to character and tolower
  raw.data <- mutate_all(raw.data, as.character)
  raw.data <- mutate_all(raw.data, tolower)

  # 3) remove columns that are naturally different in each survey:
  # - columns of type = "start", "end", "text" (for the other responses), etc.
  # - columns starting with "_"
  # - option columns for the select multiple -> keeping only the concatenation column
  types_to_remove <- c("start", "end", "today", "deviceid", "date", "geopoint", "audit",
                       "note", "calculate", "text")
  cols_to_keep <- data.frame(column=colnames(raw.data)) %>%
    left_join(select(tool.survey, name, type), by=c("column"="name")) %>%
    filter(!(type %in% types_to_remove) & !str_starts(column, "_") & !str_detect(column, "/"))
  raw.data <- raw.data[, all_of(cols_to_keep$column)]

  # 4) remove columns with all NA; convert remaining NA to "NA"; convert all columns to factor
  raw.data <- raw.data[, colSums(is.na(raw.data))<nrow(raw.data)]
  raw.data[is.na(raw.data)] <- "NA"
  raw.data <- raw.data %>% mutate_if(is.character, factor)
  error.message <- "NAs detected, remove them before proceeding (it can happen when converting to factor)"
  if (sum(is.na(raw.data))>0) stop(error.message)

  # 5) calculate gower distance
  gower_dist <- daisy(raw.data, metric="gower", warnBin=F, warnAsym=F, warnConst=F)
  gower_mat <- as.matrix(gower_dist)

  # 6) convert distance to number of differences and determine closest matching survey
  r <- unlist(lapply(1:nrow(raw.data), function(i) sort(gower_mat[i,]*ncol(raw.data))[2]))

  # 7) add relevant columns
  raw.data[["num_cols_not_NA"]] <- rowSums(raw.data!="NA")
  raw.data[[uuid]] <- uuids
  raw.data[["_id_most_similar_survey"]] <- uuids[as.numeric(names(r))]
  raw.data[["number_different_columns"]] <- as.numeric(r)
  raw.data <- raw.data %>% arrange(number_different_columns, uuid)

  return(raw.data)
}
