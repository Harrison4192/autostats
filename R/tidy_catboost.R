# tidy catboost
#
# @param .data dataframe
# @param formula formula
#
# @return catboost model
# tidy_catboost <- function(.data, formula){
#
#   formula %>%
#     rlang::f_lhs() -> target
#
#   .data %>%
#     dplyr::pull(!!target) %>%
#     is.numeric() -> numer_tg
#
#   if(numer_tg){
#     mode_set <- "regression"
#   } else{
#     mode_set <- "classification"
#   }
#
#   catboost_recipe <-
#     recipes::recipe(data = .data, formula = formula) %>%
#     recipes::step_zv(recipes::all_predictors())
#
#   catboost_spec <-
#     parsnip::boost_tree(
#       trees = 1000
#     ) %>%
#     parsnip::set_mode(mode_set) %>%
#     parsnip::set_engine("catboost")
#
#   catboost_workflow <-
#     workflows::workflow() %>%
#     workflows::add_recipe(catboost_recipe) %>%
#     workflows::add_model(catboost_spec)
#
#   catboost_workflow %>%
#     parsnip::fit(.data) -> model_fit
#
#   model_fit %>%
#     workflows::pull_workflow_fit() %>%
#     purrr::pluck("fit")
#
# }
