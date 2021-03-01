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
#' @param regression_objective_fun objective function string
#' @param verbose verbose integer
#' @param seed seed integer
#'
#' @return lightgbm model
#' @export
#'
tidy_lightgbm <- function(data, target, ..., regression_objective_fun = "regression", verbose = -1, seed = 1 ){

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
    names() -> char_names

  rlang::as_name(rlang::ensym(target)) -> tg_name

  tg_name %in% char_names -> tg_is_char

    if(tg_is_char){



      char_names %>% setdiff(tg_name) -> char_names

      data %>%
        dplyr::pull({{target}}) %>%
        dplyr::n_distinct() -> tg_lvls

  data %>% lightgbm::lgb.convert_with_rules(rules = rlang::list2(!!tg_name := 0:(tg_lvls - 1))) %>% purrr::pluck("data") -> d1

  if(tg_lvls == 2){
    objective_fun <- "binary"
    tg_lvls <- 1
  } else{
    objective_fun <- "multiclass"}}
   else{
    data %>% lightgbm::lgb.convert_with_rules() %>% purrr::pluck("data") -> d1
    tg_lvls <- 1
    objective_fun <- regression_objective_fun
  }

  d1 %>%
    dplyr::select(-{{target}}) -> d1_train

  d1 %>%
    dplyr::pull({{target}}) -> dlabel

  Matrix::Matrix(as.matrix(d1_train), sparse = T) -> dmatrix


  lightgbm::lgb.Dataset(data = dmatrix, label = dlabel, categorical_feature = char_names) -> lmatrix
  lightgbm::lgb.train(data = lmatrix, obj = objective_fun , verbose = verbose, num_class = tg_lvls) -> lgbm_model

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
    glm_family <- "multinomial classification"

    data %>%
      dplyr::mutate({{target}} := factor({{target}})) -> data

    suppressMessages({
      nnet::multinom(formula = my_formula, data = data, trace = F) -> multnm    })

    multnm$family$family <- glm_family
    multnm$call$formula <- my_formula
    multnm$call$data <- data


    return(multnm)

  }
  else{
    glm_family <-  "gaussian"
  }

  suppressWarnings({
    stats::glm(formula = my_formula, family = glm_family, data = data)
  })
}
