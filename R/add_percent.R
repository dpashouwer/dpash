
#' Title
#'
#' @param group_vars
#'
#' @return
#' @export
#'
add_percent <- function(.data, group_vars = ...){
  .data %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(perc = round(n / sum(n), 4)) %>%
    dplyr::ungroup()
}

