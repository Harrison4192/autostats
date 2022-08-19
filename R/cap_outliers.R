#' cap_outliers
#'
#' Caps the outliers of a numeric vector by percentiles. Also outputs a plot of the capped distribution
#'
#'
#' @param x numeric vector
#' @param q decimal input to the quantile function to set cap. default .05 caps at the 95 and 5th percentile
#' @param type chr vector. where to cap: both, upper, or lower
#'
#' @return numeric vector
#' @export
#'
#' @examples
#'
#' cap_outliers(iris$Petal.Width)
#'
cap_outliers <- function(x, q = .05, type = c("both", "upper", "lower")){

  xname <-rlang::enexpr(x)
  xname1 <- deparse(xname)

  type <- match.arg(type)
  lower <- q
  upper <- 1 - q

  stats::quantile(x, p = c(lower, upper)) -> caps

  if(type == "both"){

    ifelse(x < caps[1], caps[1], x) -> x1
    ifelse(x1 > caps[2], caps[2], x1) -> x1
  } else if(type == "upper"){
    ifelse(x > caps[2], caps[2], x) -> x1

  } else if(type == "lower"){
    ifelse(x < caps[1], caps[1], x) -> x1

  }


  message(stringr::str_c("lower cap: ", caps[1], " upper cap: ", caps[2]))

  tibble::tibble(x = x) -> t1
  ggplot2::ggplot(t1, ggplot2::aes(x = x)) +
    ggplot2::geom_density() +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::xlab(xname1) +
    ggplot2::geom_vline(xintercept  = caps[2], color = "red") +
    ggplot2::geom_vline(xintercept  = caps[1], color = "red") +
    ggplot2::ggtitle("variable capped at red lines") -> p1

  print(p1)


  x1
}
