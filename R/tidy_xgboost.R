#' tidy xgboost
#'
#' Accepts a formula to run an xgboost model. Automatically determines whether the formula is
#' for classification or regression. Returns the xgboost model.
#'
#' @param .data dataframe
#' @param formula formula
#' @param ... additional parameters to be passed to  \code{\link[parsnip]{set_engine}}
#' @param tree_depth: Tree Depth (type: integer, default: 6L); Typical values: 3-10
#' @param trees: # Trees (type: integer, default: 15L)
#' @param learn_rate: Learning Rate (type: double, default: 0.3); Typical values: 0.01-0.3
#' @param mtry: # Randomly Selected Predictors (type: integer)
#' @param min_n: Minimal Node Size (type: integer, default: 1L); Keep small value For highly imbalanced class data where leaf nodes can have smaller size groups.
#' @param loss_reduction: Minimum Loss Reduction (type: double, default: 0.0); gamma; reange: 0 to Inf; typical value: 0 - 1 assuming low-mid tree depth
#' @param sample_size: Proportion Observations Sampled (type: double, default: 1.0); Typical values: 0.5 - 1
#' @param stop_iter: # Iterations Before Stopping (type: integer, default: Inf)
#'
#'
#' @return xgb.Booster model
#' @export
#'
#' @examples
#'
#' options(rlang_trace_top_env = rlang::current_env())
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
#'
#' # multiclass classification that returns labels
#'
#'
#' iris %>%
#'  tidy_formula(Species) -> species_form
#'
#' iris %>%
#'tidy_xgboost(species_form,
#'             objective = "multi:softmax",
#'             trees = 100,
#'             tree_depth = 3L,
#'             loss_reduction = 0.5) -> xgb2
#'
#'
#'xgb2 %>%
#'  tidy_predict(newdata = iris, form = species_form) -> iris_preds
#'
#' # labels are given as integers
#'
#' iris_preds %>%
#'  dplyr::count(Species, Species_preds_xgb2)
#'
#'  # return to original labels
#'
#'  iris_preds %>%
#'  dplyr::mutate(Species_preds_xgb2 = factor(Species_preds_xgb2, labels = unique(Species))) %>%
#'  dplyr::count(Species, Species_preds_xgb2)
tidy_xgboost <- function(.data, formula, ...,
                         mtry = NULL,
                         trees = 15L,
                         min_n = 1L,
                         tree_depth = 6L,
                         learn_rate = 0.3,
                         loss_reduction = 0.0,
                         sample_size = 1.0,
                         stop_iter = Inf){



  formula %>%
    rlang::f_lhs() -> target


  .data %>%
    dplyr::pull(!!target) %>%
    is.numeric() -> numer_tg

  xgboost_recipe <-
    recipes::recipe(data = .data, formula = formula) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_dummy(where(is.character) | where(is.factor), -!!target)


  xgboost_spec0 <-  parsnip::boost_tree(
                                        mtry = NULL,
                                        trees = trees,
                                        min_n = min_n,
                                        tree_depth = tree_depth,
                                        learn_rate = learn_rate,
                                        loss_reduction = loss_reduction,
                                        sample_size = sample_size,
                                        stop_iter = stop_iter)

  if(numer_tg){
    mode_set <- "regression"


    xgboost_spec0 %>%
      parsnip::set_mode(mode_set) %>%
      parsnip::set_engine("xgboost", ...) -> xgboost_spec

  } else{
    mode_set <- "classification"

    .data %>%
      dplyr::mutate(!!target := as.factor(!!target) %>% forcats::fct_drop()) -> .data


    xgboost_spec0 %>%
      parsnip::set_mode(mode_set) %>%
      parsnip::set_engine("xgboost", ...) -> xgboost_spec

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

