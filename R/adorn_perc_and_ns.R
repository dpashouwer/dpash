#' adorn_perc_and_ns
#'
#' @param tabyl
#' @param denominator
#' @param digits
#'
#' @return
#' @export
#'
#' @examples
adorn_perc_and_ns <- function(tabyl, denominator, digits = 0){

  tabyl %>%
    janitor::adorn_percentages(denominator) %>%
    janitor::adorn_pct_formatting(digits = digits) %>%
    janitor::adorn_ns()

}
