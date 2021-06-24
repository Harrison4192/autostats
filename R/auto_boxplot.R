#' auto_boxplot
#'
#' @param .data data
#' @param continuous_outcome continuous y variable. unquoted column name
#' @param categorical_variable categorical x variable. unquoted column name
#' @param categorical_facets categorical facet variable. character vector
#' @param alpha alpha points
#' @param width width of jitter
#' @param color_dots dot color
#' @param color_box box color
#'
#' @return ggplot
#' @export
auto_boxplot <- function(.data,
                         continuous_outcome,
                         categorical_variable,
                         categorical_facets = NULL,
                         alpha = .3,
                         width = .15,
                         color_dots = "black",
                         color_box = "red"){

  set.seed(1)

  .data %>%
    ggplot2::ggplot(aes(x = {{categorical_variable}},
                        y = {{continuous_outcome}})) +
    ggplot2::geom_boxplot(color = color_box) +
    ggplot2::geom_jitter(alpha = alpha, width = width,  color = color_dots) +
    ggthemes::theme_clean() -> p1

  if(!is.null(categorical_facets)){
    p1 +
      ggplot2::facet_wrap(categorical_facets) -> p1
  }

  p1
}
