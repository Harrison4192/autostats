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
#'
#' @examples
#'
#' iris %>%
#' tidy_xgboost(
#'   tidy_formula(., target= Petal.Length, tidyselect::everything())
#' )  -> xg1
#'
#' xg1 %>%
#'   plot_varimp_xgboost()
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

#' Plot varimp xgboost
#'
#' @rdname tidy_xgboost
#' @param xgb xgb.Booster model
#' @param font font
#' @param ... additional arguments for \code{\link[xgboost]{xgb.ggplot.importance}}
#'
#' @return plot
#' @export
#'
plot_varimp_xgboost <- function(xgb, font = c("", "HiraKakuProN-W3"), ...){

  font <- match.arg(font)


  xgb %>%
  xgboost::xgb.importance(model = . ) %>%
  xgboost::xgb.ggplot.importance(...) +
    ggplot2::theme_minimal(base_family= font) +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))+
    ggeasy::easy_remove_legend() +
    ggplot2::ylab("Importance from xgboost")
}
