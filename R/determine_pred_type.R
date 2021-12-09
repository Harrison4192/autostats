#' determine pred type
#'
#' @param x  a vector
#' @keywords internal
#'
#' @return a string
#'
determine_pred_type <- function(x){

  x %>% dplyr::n_distinct() -> target_levels
  x %>% is.numeric() -> is_tg_numeric

  if(target_levels == 2){

    type <- "binary"

  } else if(is_probability(x)){

    type <- "binaryprob"}

  else if(target_levels > 2 & !is_tg_numeric){

    type <- "multiclass"
  } else {

    type <- "numeric"
    }
  type
}
