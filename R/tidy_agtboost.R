#' tidy agtboost
#'
#' Boosted tree regression using the  \href{https://github.com/Blunde1/agtboost}{agtboost} package.
#' Variable importance plot is printed along with returning the model. Noise features are eliminated from the plot.
#'
#' \href{https://arxiv.org/abs/2008.12625}{agtboost: Adaptive and Automatic Gradient Tree Boosting Computations}
#'
#'
#'
#' @param .data dataframe
#' @param formula formula
#' @param ... additional parameters to pass to \code{link[agtboos]{gbt.train}}
#'
#' @return agtboost model of class \code{Rcpp_ENSEMBLE}
#' @export
#'
#' @examples
#'
#' iris %>%
#'tidy_formula(target = Petal.Length) -> f1
#'
#'iris %>%
#'  tidy_agtboost(f1)
tidy_agtboost <- function(.data, formula, ...){

  formula %>%
    rlang::f_lhs() -> target

  formula %>%
    f_formula_to_charvec(.data = .data) -> predictor_vars

  .data %>%
    framecleaner::filter_missing(tidyselect::any_of(predictor_vars),
                                 tidyselect::any_of(target)) -> .data



  .data %>%
    dplyr::select(tidyselect::any_of(predictor_vars)) %>%
    framecleaner::create_dummies(remove_most_frequent_dummy = TRUE) -> train_data

  train_data %>%
  as.matrix -> train_matrix

  .data %>%
    dplyr::pull(!!target) -> tg

  tg %>%
    is.numeric() -> numer_tg

  if(!numer_tg){rlang::abort("Target must be numeric")}

  agtboost::gbt.train(y = tg, x = train_matrix, ...) -> gbt_model

  agtboost::gbt.importance(feature_names = names(train_data), object = gbt_model)

  gbt_model


}
