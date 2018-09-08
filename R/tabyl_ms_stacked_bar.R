
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

  var1 <- enquo(var1)
  var2 <- enquo(var2)

  tabyl_perc <- dat %>%
    tabyl(!! var1, !! var2) %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 0, affix_sign = TRUE) %>%
    mutate_all(as.character)

  tabyl_numbers <- dat %>%
    tabyl(!! var1, !! var2) %>%
    adorn_totals("col") %>%
    mutate_all(as.character) %>%
    rename_at(vars(-c(!! var1)), funs(paste0(., "_num")))

  tabyl_perc %>%
    left_join(tabyl_numbers) %>%
    mutate(group_w_ns = paste0(!! var1, " (n=", Total_num, ")")) %>%
    select(group_w_ns, everything(), - !! var1)

}
