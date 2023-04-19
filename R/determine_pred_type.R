#' determine pred type
#'
#' @param x  a vector
#' @param original_col logical. is numeric
#' @keywords internal
#'
#' @return a string
#'
determine_pred_type <- function(x, original_col){

  x %>% dplyr::n_distinct() -> target_levels
  x %>% is.numeric() -> is_tg_numeric

  if(target_levels <= 2 & !original_col){

    type <- "binary"

  } else if(is_probability(x) & !original_col){

    type <- "binaryprob"}

  else if(target_levels > 2 & !is_tg_numeric){

    type <- "multiclass"
  } else {

    type <- "numeric"
    }
  type
}
