#' tidy xgboost
#'
#' @param .data dataframe
#' @param target target variable
#' @param ... tidyselect
#'
#' @return xgb.Booster model
#' @export
tidy_xgboost <- function(.data, target, ...){

  .data %>%
    tidy_formula({{target}}, ...) -> form

  .data %>%
    dplyr::pull({{target}}) %>%
    is.numeric() -> numer_tg

  if(numer_tg){
    mode_set <- "regression"
  } else{
    mode_set <- "classification"
  }

  xgboost_recipe <-
    recipes::recipe(data = .data, formula = form) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_dummy(where(is.character) | where(is.factor), -{{target}})

  xgboost_spec <-
    parsnip::boost_tree() %>%
    parsnip::set_mode(mode_set) %>%
    parsnip::set_engine("xgboost", tree_method = "hist")

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
