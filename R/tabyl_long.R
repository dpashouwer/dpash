
#' tabyl_2way_long
#'
#' @param tabyl
#'
#' @return
#' @export
#'
#' @examples
tabyl_2way_long <- function(tabyl){

  var1_name = rlang::sym(attributes(tabyl)$var_names$`row`)
  var2_name = rlang::sym(attributes(tabyl)$var_names$`col`)

  tabyl %>%
    tidyr::gather(!!var2_name, n, names(tabyl)[2]:last(names(tabyl))) %>%
    group_by(!!var2_name) %>%
    mutate(total = sum(n)) %>%
    ungroup() %>%
    mutate(percent = round(n / total, digits = 3)) %>%
    select(-total)
}

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
    dplyr::bind_rows(.id = var3) %>%
    tidyr::gather(!!var2_name, n, names(tabyl[[1]])[2]:last(names(tabyl[[1]]))) %>%
    dplyr::group_by(!!var3_name, !!var1_name) %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percent = round(n / total, digits = 3)) %>%
    dplyr::select(-total) %>%
    dplyr::arrange(!!var3_name, !!var1_name)
}
