#' tidy shap
#'
#' plot and summarize shapley values from an xgboost model
#'
#' returns a list with the following entries
#'
#' \describe{
#' \item{\emph{shap_tbl}}{: table of shaply values}
#' \item{\emph{shap_summary}}{: table summarizing shapley values. Includes correlation between shaps and feature values.}
#' \item{\emph{swarmplot}}{: one plot showing the relation between shaps and features}
#' \item{\emph{scatterplots}}{: returns the top 9 most important features as determined by sum of absolute shapley values, as a facetted scatterplot of feature vs shap}
#' }
#'
#' @param model xgboost model
#' @param newdata dataframe similar to model input
#' @param form formula used for model
#' @param ... additional parameters for shapley value
#' @param top_n top n features
#'
#' @return list
#' @export
tidy_shap <- function(model, newdata, form = NULL, ..., top_n = 12){

  value <- sum_abs <- NULL

  presenter::get_piped_name() -> model_name

  rlang::as_name(rlang::ensym(newdata)) -> data_name

  form %>%
    f_formula_to_charvec(.data = newdata) -> predictors

  newdata %>%
    dplyr::select(tidyselect::all_of(predictors)) -> newdata1


  newdata1 %>%
    as.matrix() -> newdata2

  predict(model, newdata = newdata2, predcontrib = TRUE) -> preds

## swarm plot

  xgboost::xgb.ggplot.shap.summary(newdata2, preds, model = model, top_n = top_n, ...)  -> shaps


  new_name <- form %>%
    rlang::f_lhs() %>%
    stringr::str_c(" shaps from model ", model_name, " on dataset ", data_name)

shaps +
  ggplot2::labs(title = new_name, color = "normalized feature value", x = "shapley value") -> swarm_plot

## preds
name <- FEATURE <- SHAP <- BIAS <- TYPE <- NULL
suppressWarnings({

preds %>%
  tibble::as_tibble() %>%
  dplyr::select(-BIAS) -> preds1

# long shaps

preds1 %>%
  dplyr::mutate(TYPE = "SHAP") %>%
  tidyr::pivot_longer(cols = -TYPE) %>%
  dplyr::bind_rows(
    newdata1 %>%
      dplyr::select(-tidyselect::any_of(rlang::f_lhs(form))) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(TYPE = "FEATURE") %>%
      tidyr::pivot_longer(cols = -TYPE)
  ) %>%
  dplyr::arrange(name, TYPE) %>%
  tidyr::pivot_wider(names_from = TYPE, values_from = value) %>%
  tidyr::unnest(c(FEATURE, SHAP)) ->  gplottbl

## shaps summary


gplottbl %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(cor = stats::cor(FEATURE, SHAP),
            var = stats::var(SHAP),
            sum = sum(SHAP),
            sum_abs = sum(abs(SHAP))) %>%
  dplyr::arrange(dplyr::desc(sum_abs)) -> shaps_sum

## scatterplots

shaps_sum %>%
  dplyr::slice(1:9) %>%
  dplyr::pull(name) -> top_9

gplottbl %>%
  dplyr::filter(name %in% top_9) %>%
ggplot2::ggplot(ggplot2::aes(x = FEATURE, y = SHAP, color = name)) +
  ggplot2::geom_jitter(alpha = .5) +
  ggplot2::geom_smooth() +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~name, scales = "free_x") +
  ggplot2::theme(legend.position = "none") -> scatterplots

})

list(
  shap_tbl = preds1,
  shap_summary = shaps_sum,
  swarmplot = swarm_plot,
  scatterplots = scatterplots
) -> shapslist

shapslist
}
