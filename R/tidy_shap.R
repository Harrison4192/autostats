#' tidy shap
#'
#' plot shapley values from an xgboost model
#'
#' @param model xgboost model
#' @param newdata dataframe similar to model input
#' @param form formula used for model
#' @param ... additional parameters for shapley value
#' @param top_n top n features
#'
#' @return ggplot
#' @export
tidy_shap <- function(model, newdata, form = NULL, ..., top_n = 12){

  presenter::get_piped_name() -> model_name

  rlang::as_name(rlang::ensym(newdata)) -> data_name

  form %>%
    f_formula_to_charvec(.data = newdata) -> predictors

  newdata %>%
    dplyr::select(tidyselect::all_of(predictors)) -> newdata1


  newdata1 %>%
    as.matrix() -> newdata2

  predict(model, newdata = newdata2, predcontrib = TRUE) -> preds

  xgboost::xgb.ggplot.shap.summary(newdata2, preds, model = model, top_n = top_n, ...)  -> shaps


  new_name <- form %>%
    rlang::f_lhs() %>%
    stringr::str_c(" shaps from model ", model_name, " on dataset ", data_name)

shaps +
  ggplot2::labs(title = new_name, color = "normalized feature value", x = "shapley value")
}
