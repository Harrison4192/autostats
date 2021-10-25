#' tidy xgboost
#'
#' Accepts a formula to run an xgboost model. Automatically determines whether the formula is
#' for classification or regression. Returns the xgboost model.
#'
#' @param .data dataframe
#' @param formula formula
#'
#' @return xgb.Booster model
#' @export
tidy_xgboost <- function(.data, formula){

  formula %>%
    rlang::f_lhs() -> target

  .data %>%
    dplyr::pull(!!target) %>%
    is.numeric() -> numer_tg

  xgboost_recipe <-
    recipes::recipe(data = .data, formula = formula) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_dummy(where(is.character) | where(is.factor), -!!target)

  xgboost_spec0 <-  parsnip::boost_tree()

  if(numer_tg){
    mode_set <- "regression"


    xgboost_spec0 %>%
      parsnip::set_mode(mode_set) %>%
      parsnip::set_engine("xgboost") -> xgboost_spec

  } else{
    mode_set <- "classification"
    eval_metric <- 'mlogloss'


    xgboost_spec0 %>%
      parsnip::set_mode(mode_set) %>%
      parsnip::set_engine("xgboost", eval_metric = eval_metric) -> xgboost_spec

  }



  xgboost_workflow <-
    workflows::workflow() %>%
    workflows::add_recipe(xgboost_recipe) %>%
    workflows::add_model(xgboost_spec)

  xgboost_workflow %>%
    parsnip::fit(.data) -> model_fit

  model_fit %>%
    workflows::pull_workflow_fit() %>%
    purrr::pluck("fit")

}
