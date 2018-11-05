#' Make stacked bar chart in TNTP style
#'
#' @param df data frame
#' @param font
#' @param font_size
#' @param var
#' @param group_var
#' @param labels
#' @param coord_flip
#' @param title
#' @param xlab
#' @param ylab
#'
#' @return
#' @export
#'
#' @examples
bar_chart_stack <- function (df,
                             var,
                             group_var,
                             labels = "pct",
                             coord_flip = FALSE,
                             title = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             font = "Segoe UI",
                             font_size = 8)
{
  if (!is.data.frame(df)) {
    stop("You must supply a data.frame to the df argument")
  }
  if (missing(var)) {
    stop("You must supply a column name to the var argument")
  }

  #### Single plot
  if (missing(group_var)) {

    # clean plot data
    plot_data <- df %>%
      dplyr::select_(.dots = list(vec = lazyeval::lazy(var))) %>%
      dplyr::mutate(vec.factor = as.factor(vec)) %>%
      dplyr::group_by(vec.factor) %>%
      dplyr::tally() %>%
      dplyr::mutate(perc = n/sum(n))

    # make plot
    plot <- plot_data %>%
      ggplot2::ggplot(aes(x = "", y = n, fill = vec.factor)) +
      ggplot2::geom_bar(position = position_fill(), stat = "identity")

    # labels
    if(labels == "pct"){
      plot <- plot +
        geom_text(aes(label = paste0((perc * 100) %>% round(digits = 0), "%")), position = position_fill(vjust = 0.5))
      } else if(labels == "n"){
      plot <- plot +
        ggplot2::geom_text(aes(label = n), position = position_fill(vjust = 0.5))
    }
  } else {

    #### Multi plot

    # clean plot data
    plot_data <- df %>%
      dplyr::select_(.dots = list(vec = lazyeval::lazy(var), group.vec = lazyeval::lazy(group_var))) %>%
      dplyr::mutate(vec.factor = as.factor(vec), group.factor = as.factor(group.vec)) %>%
      dplyr::group_by(vec.factor, group.factor) %>%
      dplyr::tally() %>%
      dplyr::group_by(vec.factor) %>%
      dplyr::mutate(perc = n/sum(n)) %>%
      dplyr::ungroup() %>%
      tidyr::complete(vec.factor, group.factor, fill = list(n = NA, perc = NA))

    # make plot
    plot <- plot_data %>%
      ggplot2::ggplot(aes(x = group.factor, y = n, fill = vec.factor)) +
      ggplot2::geom_bar(position = position_fill(), stat = "identity")

    # labels
    if(labels == "pct"){
      plot <- plot  +
        ggplot2::geom_text(aes(label = paste0((perc * 100) %>% round(digits = 0), "%")), position = position_fill(vjust = 0.5))} else if(labels == "n"){
      plot <- plot +
        ggplot2::geom_text(aes(label = n), position = position_fill(vjust = 0.5))
    }
  }

  # # coord_flip
  # if(coord_flip == TRUE){
  #   plot <- plot +
  #     ggplot2::coord_flip() +
  #     theme(axis.text.x = element_blank(),
  #           axis.text.y = element_text(family = font, size = font_size))
  # } else {
  #   plot <- plot +
  #     theme(axis.text.x = element_text(family = font, size = font_size),
  #           axis.text.y = element_blank())
  # }

  # themes
  plot <- plot +
    ggplot2::ggtitle(label = title) +
    ggplot2::labs(x = xlab, y = ylab) +

    tntpr::theme_tntp() +

    ggplot2::theme(# title
                   plot.title = element_text(family = font, face = "bold", size = font_size + 4, hjust = .5),

                   # axis decorations
                   axis.line.y = element_blank(),
                   axis.line.x = element_blank(),
                   axis.ticks = element_blank(),

                   # legend
                   legend.position = "bottom", legend.text = element_text(family = font, size = font_size), legend.title = element_blank(),

                   # panels
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()

                   )


  plot

}

