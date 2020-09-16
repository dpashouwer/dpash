#' read_excel_all_sheets
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_excel_all_sheets <- function(path){
  sheets = readxl::excel_sheets(path)
  map(sheets, ~ readxl::read_excel(path, sheet = .x)) %>%
    purrr::set_names(janitor::make_clean_names(sheets)) %>%
    list2env(envir = globalenv())
}
