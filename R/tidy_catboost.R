#' tidy catboost
#'
#' @param .data dataframe
#' @param target target variable
#' @param ... tidyselect
#'
#' @return catboost model
#' @export
tidy_catboost <- function(.data, target, ...){

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

  catboost_recipe <-
    recipes::recipe(data = .data, formula = form) %>%
    recipes::step_zv(recipes::all_predictors())

  catboost_spec <-
    parsnip::boost_tree(
      trees = 1000
    ) %>%
    parsnip::set_mode(mode_set) %>%
    parsnip::set_engine("catboost")

  catboost_workflow <-
    workflows::workflow() %>%
    workflows::add_recipe(catboost_recipe) %>%
    workflows::add_model(catboost_spec)

  catboost_workflow %>%
    parsnip::fit(.data) -> model_fit

  model_fit %>%
    workflows::pull_workflow_fit() %>%
    purrr::pluck("fit")

}
