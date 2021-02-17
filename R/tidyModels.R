#' plot cforest variable importance
#'
#' @param vimp cforest vip
#' @keywords internal
#'
#' @return ggplot
#' @export
plot_varimp <- function(vimp){
  moreparty::ggVarImp(vimp) +
    ggthemes::theme_clean() +
    ggplot2::geom_bar(stat = "identity", fill = "blue")}

#' charvec to formula
#'
#' @param lhs lhs
#' @param rhs rhs
#' @keywords internal
#'
#' @return formula

charvec_to_formula <- function(lhs, rhs){

  if(rlang::is_empty(rhs)){return(NULL)} else{

    stringr::str_c(rhs, collapse = " + ") %>%
      stringr::str_c(lhs, " ~ ", ., collapse = "")  %>%
      parse(text = .) %>%
      eval()}
}
#' tidy formula construction
#'
#' @param data dataframe
#' @param target lhs
#' @param ... tidyselect. rhs
#'
#' @return a formula
#' @export
tidy_formula <- function(data, target, ...){

  rlang::as_name(rlang::ensym(target)) -> lhs_var


  if(missing(...)) {

    data %>% dplyr::select(-tidyselect::any_of(lhs_var)) %>% names() -> rhs_vars

  } else {

    data %>% dplyr::select(...) %>% names() %>% setdiff(lhs_var) -> rhs_vars
  }

  charvec_to_formula(lhs_var, rhs_vars)
}

#' get model vars
#'
#' @param data dataframe
#' @param target target variable
#' @param ... tidyselect predictor variables
#' @keywords internal
#'
#' @return character vector
get_model_vars <- function(data, target, ...){

  rlang::as_name(rlang::ensym(target)) -> lhs_var


  if(missing(...)) {

    data %>% dplyr::select(-tidyselect::any_of(lhs_var)) %>% names() -> rhs_vars

  } else {

    data %>% dplyr::select(...) %>% names() %>% setdiff(lhs_var) -> rhs_vars
  }

  stringr::str_c(lhs_var, rhs_vars)
}

#' tidy conditional inference forest
#'
#' @param data dataframe
#' @param target target variable
#' @param ... tidyselect predictor variables
#' @param seed seed integer
#'
#' @return a cforest model
#' @export

tidy_cforest <- function(data, target, ..., seed = 1) {
  set.seed(seed)

  data %>%
    dplyr::mutate(dplyr::across(where(is.character), factor)) -> data

  data %>%
    tidy_formula({{target}}, ...) -> my_formula

  data %>%
    dplyr::select({{target}}, ...) %>% names() -> all_vars

  data %>% ggplot2::remove_missing(vars = all_vars) -> data


  suppressWarnings({
    party::cforest(formula = my_formula,
                   data = data)
  })
}

#' tidy lightgbm
#'
#' @param data dataframe
#' @param target target variable
#' @param ... tidyselect predictor variables
#' @param objective_fun objective function string
#' @param verbose verbose integer
#' @param seed seed integer
#'
#' @return lightgbm model
#' @export
#'
tidy_lightgbm <- function(data, target, ..., objective_fun = "regression", verbose = -1, seed = 1 ){

  set.seed(seed)
  if(missing(...)) {

    data %>% dplyr::select(tidyselect::everything()) -> data

  } else {

    data %>% dplyr::select({{target}}, ...) -> data
  }

  data %>%
    tidyr::drop_na({{target}}) -> data

  data %>%
    dplyr::select(where(is.character) | where(is.factor)) %>%
    names() %>%
    setdiff(rlang::as_name(rlang::ensym(target))) -> char_names

  data %>% lightgbm::lgb.convert_with_rules() %>% purrr::pluck("data") -> d1

  d1 %>%
    dplyr::select(-{{target}}) -> d1_train

  d1 %>%
    dplyr::pull({{target}}) -> dlabel

  Matrix::Matrix(as.matrix(d1_train), sparse = T) -> dmatrix


  lightgbm::lgb.Dataset(data = dmatrix, label = dlabel, categorical_feature = char_names) -> lmatrix
  lightgbm::lgb.train(data = lmatrix, obj = objective_fun , verbose = verbose) -> lgbm_model

  lgbm_model
}

#' tidy glm
#'
#' @param data dataframe
#' @param target target variable
#' @param ... tidyselect predictor variables
#'
#' @return glm model
#' @export
tidy_glm <- function(data, target, ...) {

  data %>%
    tidy_formula({{target}}, ...) -> my_formula

  data %>% dplyr::pull({{target}}) -> trg
  trg %>% dplyr::n_distinct() -> target_levels
  trg %>% is.numeric() -> is_tg_numeric

  if(target_levels == 2){
    glm_family <-  "binomial"

    data %>%
      dplyr::mutate({{target}} := factor({{target}})) -> data

  } else if(target_levels != 2 & !is_tg_numeric){
    rlang::abort("Target variable is neither binary nor continuous")
  } else{
    glm_family <-  "gaussian"
  }

  suppressWarnings({
    stats::glm(formula = my_formula, family = glm_family, data = data)
  })
}
