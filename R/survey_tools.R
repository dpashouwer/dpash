

#' Fix Survey Monkey's dual row names
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
fix_SM_dual_row_names <- function(dat){
  current_names <- names(dat)
  row_1 <- unlist(dat[1, ])

  new_names <- ifelse(row_1 %in% c("Response", "Open-Ended Response", NA),
                      current_names,
                      row_1)
  dat[-1, ] %>%
    setNames(new_names) %>%
    janitor::clean_names()
}
