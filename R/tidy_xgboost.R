#' tidy xgboost
#'
#' Accepts a formula to run an xgboost model. Automatically determines whether the formula is
#' for classification or regression. Returns the xgboost model.
#'
#' @param .data dataframe
#' @param formula formula
#' @param ... additional parameters to be passed to  \code{\link[parsnip]{set_engine}}
#' @param tree_depth Tree Depth (xgboost: max_depth) (type: integer, default: 6L); Typical values: 3-10
#' @param trees # Trees (xgboost: nrounds) (type: integer, default: 15L)
#' @param learn_rate Learning Rate (xgboost: eta) (type: double, default: 0.3); Typical values: 0.01-0.3
#' @param mtry # Randomly Selected Predictors (xgboost: colsample_bynode) (type: numeric, range 0 - 1) (or type: integer if \code{count = TRUE})
#' @param min_n  Minimal Node Size (xgboost: min_child_weight) (type: integer, default: 1L); Keep small value for highly imbalanced class data where leaf nodes can have smaller size groups.
#' @param loss_reduction Minimum Loss Reduction (xgboost: gamma) (type: double, default: 0.0);  range: 0 to Inf; typical value: 0 - 1 assuming low-mid tree depth
#' @param sample_size Proportion Observations Sampled (xgboost: subsample) (type: double, default: 1.0); Typical values: 0.5 - 1
#' @param stop_iter # Iterations Before Stopping (xgboost: early_stop) (type: integer, default: Inf)
#' @param counts if \code{TRUE} specify \code{mtry} as an integer number of cols. Default \code{FALSE} to specify \code{mtry} as fraction of cols from 0 to 1
#'
#' @return xgb.Booster model
#' @export
#'
#' @examples
#'
#' options(rlang_trace_top_env = rlang::current_env())
#'
#'
#'# regression on numeric variable
#'
#'iris %>%
#'  framecleaner::create_dummies(Species) -> iris_dummy
#'
#'iris_dummy %>%
#'  tidy_formula(target= Petal.Length) -> petal_form
#'
#'iris_dummy %>%
#'  tidy_xgboost(
#'    petal_form,
#'    trees = 500,
#'    mtry = .5
#'  )  -> xg1
#'
#'xg1 %>%
#'  visualize_model(top_n = 2)
#'
#'xg1 %>%
#'  tidy_predict(newdata = iris_dummy, form = petal_form) -> iris_preds
#'
#'iris_preds %>%
#'  eval_preds()
#'
#'
#'# binary classification
#'# returns probabilty and labels
#'
#'iris %>%
#'  tidy_formula(Species) -> species_form
#'
#'iris %>%
#'  dplyr::filter(Species != "versicolor") %>%
#'  dplyr::mutate(Species = forcats::fct_drop(Species)) -> iris_binary
#'
#'iris_binary %>%
#'  tidy_xgboost(formula = species_form, trees = 50L, mtry = 0.2) -> xgb_bin
#'
#'xgb_bin %>%
#'  tidy_predict(newdata = iris_binary, form = species_form) -> iris_binary1
#'
#'iris_binary1 %>%
#'  eval_preds()
#'
#'
#'# multiclass classification that returns labels
#'
#'
#'
#'
#'iris %>%
#'  tidy_xgboost(species_form,
#'               objective = "multi:softmax",
#'               trees = 100,
#'               tree_depth = 3L,
#'               loss_reduction = 0.5) -> xgb2
#'
#'
#'
#'xgb2 %>%
#'  tidy_predict(newdata = iris, form = species_form) -> iris_preds
#'
#'iris_preds %>%
#'  eval_preds(yardstick::j_index)
#'
#'
#'# multiclass classification that returns probabilities
#'
#'
#'iris %>%
#'  tidy_xgboost(species_form,
#'               objective = "multi:softprob",
#'               trees = 50L,
#'               sample_size = .2,
#'               mtry = .5,
#'               tree_depth = 2L,
#'               loss_reduction = 3) -> xgb2_prob
#'
#'xgb2_prob %>%
#'  tidy_predict(newdata = iris_preds, form = species_form) -> iris_preds1
#'
#'# also requires the labels in the dataframe to evaluate preds
#'# the model name must be supplied as well. Then roc metrics can be calculated
#'iris_preds1 %>%
#'  eval_preds(softprob_model = "xgb2_prob", yardstick::average_precision
#'  )
#'
#'
tidy_xgboost <- function(.data, formula, ...,
                         mtry = 1.0,
                         trees = 15L,
                         min_n = 1L,
                         tree_depth = 6L,
                         learn_rate = 0.3,
                         loss_reduction = 0.0,
                         sample_size = 1.0,
                         stop_iter = Inf,
                         counts = FALSE){



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
                                        mtry = mtry,
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
      parsnip::set_engine("xgboost", ..., counts = counts) -> xgboost_spec

  } else{
    mode_set <- "classification"

    .data %>%
      dplyr::mutate(!!target := as.factor(!!target) %>% forcats::fct_drop()) -> .data


    xgboost_spec0 %>%
      parsnip::set_mode(mode_set) %>%
      parsnip::set_engine("xgboost", ..., counts = counts) -> xgboost_spec

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
#' @param top_n top n important variables
#' @param ... additional arguments for \code{\link[xgboost]{xgb.ggplot.importance}}
#' @keywords internal
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

