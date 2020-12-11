#' to_clipboard
#'
#' @param x
#' @param row.names
#' @param col.names
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
to_clipboard <- function(x, row.names = FALSE, col.names = TRUE, ...) {
  write.table(x, "clipboard", sep="\t", row.names = row.names, col.names = col.names, ...)
}
