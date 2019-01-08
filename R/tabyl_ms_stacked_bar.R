
#' Data Formatted for MS Stacked Bar Charts
#'
#' @param dat data
#' @param var1 row variable
#' @param var2 column variable
#'
#' @return
#' @export
#'
#' @examples
tabyl_ms_stacked_bar <- function(dat, var1, var2){

  var1 <- dplyr::enquo(var1)
  var2 <- dplyr::enquo(var2)

  tabyl_perc <- dat %>%
    janitor::tabyl(!! var1, !! var2) %>%
    janitor::adorn_percentages("row") %>%
    janitor::adorn_totals("row") %>%
    janitor::adorn_pct_formatting(digits = 0, affix_sign = TRUE) %>%
    dplyr::mutate_all(as.character)

  tabyl_numbers <- dat %>%
    janitor::tabyl(!! var1, !! var2) %>%
    janitor::adorn_totals("col") %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::rename_at(vars(-c(!! var1)), funs(paste0(., "_num")))

  tabyl_perc %>%
    dplyr::left_join(tabyl_numbers) %>%
    dplyr::mutate(group_w_ns = paste0(!! var1, " (n=", Total_num, ")")) %>%
    dplyr::select(group_w_ns, everything(), - !! var1)

}
