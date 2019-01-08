#' Feed it a filepath where a query lives in a .txt file, it fetches the query
#'
#' @param path_to_query file path
#'
#' @return
#' @export
#'
#' @examples
tt2_get_query <- function(path_to_query){
  path_to_query %>%
    readChar(., file.size(.)) %>%
    RODBC::sqlQuery(dbhandler, ., stringsAsFactors = FALSE) %>%
    janitor::clean_names()
}
