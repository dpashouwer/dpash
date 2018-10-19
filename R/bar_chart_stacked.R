#' Make stacked bar chart in TNTP style
#'
#' @param df data frame
#' @param yvar fill variable
#' @param xvar group
#' @param font
#' @param font_size
#'
#' @return
#' @export
#'
#' @examples
bar_chart_stack <- function (df,
                             yvar,
                             xvar,
                             font = "Segoe UI",
                             font_size = 12)
{
  if (!is.data.frame(df)) {
    stop("You must supply a data.frame to the df argument")
  }
  if (missing(yvar)) {
    stop("You must supply a column name to the yvar argument")
  }
  if (missing(xvar)) {
    plot_data <- df %>%
      dplyr::select_(.dots = list(vec = lazyeval::lazy(yvar))) %>%
      dplyr::mutate(vec.factor = as.factor(vec)) %>%
      dplyr::group_by(vec.factor) %>%
      dplyr::tally() %>%
      dplyr::mutate(perc = n/sum(n))

    plot <- plot_data %>%
      ggplot(aes(x = "", fill = vec.factor)) +
      geom_bar(position = position_fill())

    plot

  } else {
    plot_data <- df %>%
      dplyr::select_(.dots = list(vec = lazyeval::lazy(yvar), group.vec = lazyeval::lazy(xvar))) %>%
      dplyr::mutate(vec.factor = as.factor(vec), group.factor = as.factor(group.vec)) %>%
      dplyr::group_by(vec.factor, group.factor) %>%
      dplyr::tally() %>%
      dplyr::group_by(vec.factor) %>%
      dplyr::mutate(perc = n/sum(n)) %>%
      dplyr::ungroup() %>%
      tidyr::complete(vec.factor, group.factor, fill = list(n = NA, perc = NA))

    plot <- plot_data %>%
      ggplot(aes(x = group.factor, y = n, fill = vec.factor)) +
      geom_bar(position = position_fill(), stat = "identity")
  }

  plot <- plot +
    theme(axis.line.y = element_blank(),
          axis.line.x = element_line(color = "grey70", size = 0.2),
          axis.text.y = element_blank(), axis.text.x = element_text(family = font, size = font_size), axis.ticks = element_blank(),
          axis.title.x = element_text(family = font, size = font_size),
          axis.title.y = element_blank(), legend.key = element_blank(),
          legend.position = "bottom", legend.text = element_text(family = font, size = font_size), legend.title = element_blank(),
          panel.background = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), plot.title = element_text(family = font, face = "bold", size = font_size))
  plot

}
