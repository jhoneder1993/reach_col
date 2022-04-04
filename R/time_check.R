#' Time Check
#'
#' Sirve para identifar encuestas que no se encuentran dentro del tiempo estipulado
#'
#' @param df La base de datos con las columnas start & end o inicio & final
#'
#' @return Base de datos con uuid, interview_duration y CHECK_interview_duration
#' @export
#'
#' @examples base <- time_check(base_datos)
#' @examples base <- time_check(df = base_datos)

time_check <- function(df, time_min=5, time_max=60, uuid = "_uuid"){
  if (all(c("inicio", "final", uuid) %in% names(df))){
    df <- df %>% mutate(interview_duration = difftime(as.POSIXct(ymd_hms(final)), as.POSIXct(ymd_hms(inicio)), units = "mins"),
                        CHECK_interview_duration = case_when(
                          interview_duration < time_min ~ "Too short",
                          interview_duration > time_max ~ "Too long",
                          TRUE ~ "Okay")) %>% select(uuid, interview_duration, CHECK_interview_duration)
  } else if (all(c("start", "end", uuid) %in% names(df))){
    df <- df %>% mutate(interview_duration = difftime(as.POSIXct(ymd_hms(end)), as.POSIXct(ymd_hms(start)), units = "mins"),
                        CHECK_interview_duration = case_when(
                          interview_duration < time_min ~ "Too short",
                          interview_duration > time_max ~ "Too long",
                          TRUE ~ "Okay")) %>% select(uuid, interview_duration, CHECK_interview_duration)
  } else {
    print("No se encontraton las variables inicio & final o start & end o el uuid suministrado")
  }

  return(df)
}
