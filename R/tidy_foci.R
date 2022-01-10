#' Tidy FOCI
#'
#' varaible selection with FOCI
#'
#' @param .data data
#' @param formula formula
#' @param ... other arguments to FOCI
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' iris %>%
#' tidy_foci(Species ~ .) -> d1
#'
#' d1 %>%
#'   tibble::as_tibble()
tidy_foci <- function(.data, formula, ...) {
  formula %>%
    rlang::f_lhs() -> Y_col

  f_formula_to_charvec(formula,
                       .data = .data) -> X_cols

  .data %>%
    names -> dnames

  dnames %>%
    setdiff(X_cols) -> idcols

  .data %>%
    dplyr::select(tidyselect::any_of(X_cols)) -> X

  .data %>%
    dplyr::select(tidyselect::any_of(Y_col)) -> Y

  Y %>%
    unlist -> Y1

  if(is.character(Y1) | is.factor(Y1)){

    Y1 %>%
      as.factor %>%
      as.integer() -> Y1
  }


  FOCI::foci(Y = Y1, X = X, ..., numCores = 1) -> fc1

  fc1$selectedVar$names -> foci_vars

  .data %>%
   dplyr:: select(tidyselect::any_of(idcols),
           tidyselect::any_of(foci_vars)) -> .data1

  message(stringr::str_c("removed ", length(foci_vars), " variables out of ", length(X_cols)))

  .data1



}

