
#' Title
#'
#' @param group_vars
#'
#' @return
#' @export
#'
#' @examples tntpr::wisc %>%
#' count(grade, school) %>%
#' add_percent(grade)
add_percent <- function(.data, ...){
  .data %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(percent = round(n / sum(n), 4)) %>%
    dplyr::ungroup()
}

