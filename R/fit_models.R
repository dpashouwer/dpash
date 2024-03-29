#' bar_chart_stacked
#'
#' @param dat data.frame
#' @param formula formula for lm
#'
#' @return data.frame
#' @export
#'
#' @examples
#' fit_lm_tidy_w_stars(trees, Girth ~ Volume)
fit_lm_tidy_w_stars <- function(dat, formula){
  dat %>%
    as.data.frame() %>%
    stats::lm(formula = formula) %>%
    broom::tidy() %>%
    dplyr::mutate(stars = dplyr::case_when(p.value < .001 ~ "***",
                                           p.value >= .001 & p.value < .01 ~ "**",
                                           p.value >= .01 & p.value < .05 ~ "*",
                                           TRUE ~ ""),
           estimate_w_stars = paste0(round(estimate, digits = 2), stars)) %>%
    dplyr::select(-stars)
}
