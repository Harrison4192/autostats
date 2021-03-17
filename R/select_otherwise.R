#' @param .data dataframe
#' @param ... tidyselect
#' @param otherwise tidyselect
#' @param col tidyselect
#' @param return_type choose to return column index, names, or df. defaults to index
#'
#' @return integer vector by default. possibly data frame or character vector
#' @keywords internal
#'
select_otherwise <- function(.data, ..., otherwise = NULL, col = NULL, return_type = c("index", "names", "df")){

  return_type <- match.arg(return_type)

  .dots <- rlang::expr(c(...))


  col <- rlang::enexpr(col)
  otherwise = rlang::enexpr(otherwise)


  tidyselect::eval_select(.dots, data = .data) -> eval1

  if(length(eval1) == 0){
    tidyselect::eval_select( otherwise, data = .data) -> eval1
  }

  tidyselect::eval_select(col, data = .data) %>%
    c(eval1) %>% sort() -> eval1


  if(return_type == "df"){

    out <- .data %>% dplyr::select(tidyselect::any_of(eval1))
  } else if(return_type == "names"){
    out <- names(eval1)
  } else{
    out <- eval1
  }

  out
}

