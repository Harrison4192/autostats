#' tidy xgboost
#'
#' Accepts a formula to run an xgboost model. Automatically determines whether the formula is
#' for classification or regression. Returns the xgboost model.
#'
#' @param .data dataframe
#' @param formula formula
#' @param ... additional parameters to be passed to  \code{\link[parsnip]{boost_tree}}
#'
#' @return xgb.Booster model
#' @export
#'
#' @examples
#'
#' iris %>%
#' tidy_xgboost(
#'   tidy_formula(., target= Petal.Length),
#'   trees = 500,
#'   mtry = 2
#' )  -> xg1
#'
#' xg1 %>%
#'   visualize_model(top_n = 2)
tidy_xgboost <- function(.data, formula, ...){

  formula %>%
    rlang::f_lhs() -> target

  .data %>%
    dplyr::pull(!!target) %>%
    is.numeric() -> numer_tg

  xgboost_recipe <-
    recipes::recipe(data = .data, formula = formula) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_dummy(where(is.character) | where(is.factor), -!!target)


  xgboost_spec0 <-  parsnip::boost_tree(...)

  if(numer_tg){
    mode_set <- "regression"


    xgboost_spec0 %>%
      parsnip::set_mode(mode_set) %>%
      parsnip::set_engine("xgboost") -> xgboost_spec

  } else{
    mode_set <- "classification"


    xgboost_spec0 %>%
      parsnip::set_mode(mode_set) %>%
      parsnip::set_engine("xgboost") -> xgboost_spec

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
#'  recommended parameters to control;
#'
#' \itemize{
#' \item{\code{top_n}}{ number of features to include in the graph}
#' }
#'
#' @param xgb xgb.Booster model
#' @param font font
#' @param ... additional arguments for \code{\link[xgboost]{xgb.ggplot.importance}}
#'
#' @return ggplot
#'
plot_varimp_xgboost <- function(xgb, font = c("", "HiraKakuProN-W3"), top_n = 10, ...){

  font <- match.arg(font)


  xgb %>%
  xgboost::xgb.importance(model = . ) %>%
  xgboost::xgb.ggplot.importance(..., top_n = top_n) +
    ggplot2::theme_minimal(base_family= font) +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))+
    ggeasy::easy_remove_legend() +
    ggplot2::ylab("Importance from xgboost")
}

