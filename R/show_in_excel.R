#' show_in_excel
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
show_in_excel <- function(.data){

  tmp <- paste0(tempfile(), ".csv")

  write.csv(.data, tmp)

  fs::file_show(path = tmp)

}
