#' Read Excel
#'
#' Genera una lista con las hojas del excel cargado
#'
#' @param filename direccion del archivo que se quiere cargar
#' @param tibble
#'
#' @return una lista del archivo excel con todas las hojas
#' @export
#'
#' @examples separate_excel("C:/Users/SIG/Dropbox/REACH_COL/2_Research_Management/2022_41XXX_ECHO/MSNA/1_Setup/5_Training/4_Registro_encuestadores/Correo_registro/Kobo_MSNA2022_JM_V5.xlsx")

r_excel <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_types = c("text") ))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
