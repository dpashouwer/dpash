#' tabyl_3way_long
#'
#' @param tabyl
#' @param var3
#'
#' @return
#' @export
#'
#' @examples
tabyl_3way_long <- function(tabyl, var3){

  var1_name = rlang::sym(attributes(tabyl[[1]])$var_names$`row`)
  var2_name = rlang::sym(attributes(tabyl[[1]])$var_names$`col`)
  var3_name = rlang::sym(var3)

  tabyl %>%
    bind_rows(.id = var3) %>%
    gather(!!var2_name, n, names(tabyl[[1]])[2]:last(names(tabyl[[1]]))) %>%
    group_by(!!var3_name, !!var1_name) %>%
    mutate(total = sum(n)) %>%
    ungroup() %>%
    mutate(pct = round(n / total, digits = 3)) %>%
    select(-total) %>%
    arrange(!!var3_name, !!var1_name)
}
