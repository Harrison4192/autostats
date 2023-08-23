#' tidy xgboost
#'
#' Accepts a formula to run an xgboost model. Automatically determines whether the formula is
#' for classification or regression. Returns the xgboost model.
#'
#' In binary classification the target variable must be a factor with the first level set to the event of interest.
#' A higher probability will predict the first level.
#'
#' reference for parameters: \href{https://xgboost.readthedocs.io/en/stable/parameter.html}{xgboost docs}
#'
#' @param .data dataframe
#' @param formula formula
#' @param ... additional parameters to be passed to  \code{\link[parsnip]{set_engine}}
#' @param tree_depth Tree Depth (xgboost: max_depth) (type: integer, default: 7L); Typical values: 3-10
#' @param trees # Trees (xgboost: nrounds) (type: integer, default: 500L)
#' @param learn_rate Learning Rate (xgboost: eta) (type: double, default: 0.05); Typical values: 0.01-0.3
#' @param mtry # Randomly Selected Predictors; defaults to .75; (xgboost: colsample_bynode) (type: numeric, range 0 - 1) (or type: integer if \code{count = TRUE})
#' @param min_n  Minimal Node Size (xgboost: min_child_weight) (type: integer, default: 2L); [typical range: 2-10] Keep small value for highly imbalanced class data where leaf nodes can have smaller size groups. Otherwise increase size to prevent overfitting outliers.
#' @param loss_reduction Minimum Loss Reduction (xgboost: gamma) (type: double, default: 1.0);  range: 0 to Inf; typical value: 0 - 20 assuming low-mid tree depth
#' @param sample_size Proportion Observations Sampled (xgboost: subsample) (type: double, default: .75); Typical values: 0.5 - 1
#' @param stop_iter # Iterations Before Stopping (xgboost: early_stop) (type: integer, default: 15L) only enabled if validation set is provided
#' @param counts if \code{TRUE} specify \code{mtry} as an integer number of cols. Default \code{FALSE} to specify \code{mtry} as fraction of cols from 0 to 1
#' @param tree_method xgboost tree_method. default is \code{auto}. reference: \href{https://xgboost.readthedocs.io/en/stable/treemethod.html}{tree method docs}
#' @param monotone_constraints an integer vector with length of the predictor cols, of \code{-1, 1, 0} corresponding to decreasing, increasing, and no constraint respectively for the index of the predictor col. reference: \href{https://xgboost.readthedocs.io/en/stable/tutorials/monotonic.html}{monotonicity docs}.
#' @param num_parallel_tree should be set to the size of the forest being trained. default 1L
#' @param lambda [default=.5] L2 regularization term on weights. Increasing this value will make model more conservative.
#' @param alpha [default=.1] L1 regularization term on weights. Increasing this value will make model more conservative.
#' @param scale_pos_weight [default=1] Control the balance of positive and negative weights, useful for unbalanced classes. if set to TRUE, calculates sum(negative instances) / sum(positive instances). If first level is majority class, use values < 1, otherwise normally values >1 are used to balance the class distribution.
#' @param verbosity [default=1] Verbosity of printing messages. Valid values are 0 (silent), 1 (warning), 2 (info), 3 (debug).
#' @param validate default TRUE. report accuracy metrics on a validation set.
#' @param booster defaults to 'gbtree' for tree boosting but can be set to 'gblinear'
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
#'    trees = 20,
#'    mtry = .5
#'  )  -> xg1
#'
#'
#'xg1 %>%
#'  tidy_predict(newdata = iris_dummy, form = petal_form) -> iris_preds
#'
#'iris_preds %>%
#'  eval_preds()
#'
#'
# # binary classification
# # returns probability and labels
# if(FALSE){
#
#
# iris %>%
#  tidy_formula(Species) -> species_form
#
# iris %>%
#  dplyr::filter(Species != "versicolor") %>%
#  dplyr::mutate(Species = forcats::fct_drop(Species)) -> iris_binary
#
# iris_binary %>%
#  tidy_xgboost(formula = species_form, trees = 30L, mtry = 0.2) -> xgb_bin
#
# xgb_bin %>%
#  tidy_predict(newdata = iris_binary, form = species_form) -> iris_binary1
#
# iris_binary1 %>%
#  eval_preds()
#
#
# # multiclass classification that returns labels
#
#
#
#
# iris %>%
#  tidy_xgboost(species_form,
#               objective = "multi:softmax",
#               trees = 15L,
#               tree_depth = 3L,
#               loss_reduction = 0.5) -> xgb2
#
#
#
# xgb2 %>%
#  tidy_predict(newdata = iris, form = species_form) -> iris_preds
#
# # additional yardstick metrics can be supplied to the dots in eval_preds
#
# iris_preds %>%
#  eval_preds(yardstick::j_index)
#
#
# # multiclass classification that returns probabilities
#
#
# iris %>%
#  tidy_xgboost(species_form,
#               objective = "multi:softprob",
#               trees = 20L,
#               sample_size = .2,
#               mtry = .5,
#               tree_depth = 2L,
#               loss_reduction = 3) -> xgb2_prob
#
# # predict on the data that already has the class labels, so the resulting data frame
# # has class and prob predictions
#
# xgb2_prob %>%
#  tidy_predict(newdata = iris_preds, form = species_form) -> iris_preds1
#
# # also requires the labels in the dataframe to evaluate preds
# # the model name must be supplied as well. Then roc metrics can be calculated
# #iris_preds1 %>%
# #  eval_preds( yardstick::average_precision, softprob_model = "xgb2_prob"
# #  )}
#'
#'
tidy_xgboost <- function(.data, formula, ...,
                         mtry = 0.75,
                         trees = 500L,
                         min_n = 2L,
                         tree_depth = 7L,
                         learn_rate = 0.05,
                         loss_reduction = 1,
                         sample_size = 0.75,
                         stop_iter = 15L,
                         counts = FALSE,
                         tree_method = c("auto", "exact", "approx", "hist", "gpu_hist"),
                         monotone_constraints = 0L,
                         num_parallel_tree = 1L,
                         lambda = 0.5,
                         alpha = 0.1,
                         scale_pos_weight = 1,
                         verbosity = 0L,
                         validate = TRUE,
                         booster = c("gbtree", "gblinear")){


  tree_method <- match.arg(tree_method)
  booster <- match.arg(booster)


  formula %>%
    rlang::f_lhs() -> target

  n <- NULL


  .data %>%
    dplyr::pull(!!target) %>%
    is.numeric() -> numer_tg

  .data %>%
    dplyr::pull(!!target) %>%
    is.character() -> chr_tg

if(isTRUE(scale_pos_weight)){
  .data %>%
    dplyr::count(!!target) %>%
    dplyr::pull(n) -> classcounts

  scale_pos_weight <- classcounts[2] / classcounts[1]
}




  xgboost_recipe <-
    recipes::recipe(data = .data, formula = formula)
    # recipes::step_zv(recipes::all_predictors()) %>%
    # recipes::step_dummy(where(is.character) | where(is.factor), -!!target)


  xgboost_spec0 <-  parsnip::boost_tree(
    mtry = mtry,
    trees = trees,
    min_n = min_n,
    tree_depth = tree_depth,
    learn_rate = learn_rate,
    loss_reduction = loss_reduction,
    sample_size = sample_size,
    stop_iter = stop_iter
  ) %>%
    parsnip::set_engine("xgboost", ...,
                        counts = counts,
                        tree_method = tree_method,
                        monotone_constraints = monotone_constraints,
                        num_parallel_tree = num_parallel_tree,
                        lambda = lambda,
                        alpha = alpha,
                        scale_pos_weight = scale_pos_weight,
                        verbosity = verbosity,
                        booster = booster)




  if (numer_tg) {
    mode_set <- "regression"


    xgboost_spec0 %>%
      parsnip::set_mode(mode_set)  -> xgboost_spec

  } else{
    mode_set <- "classification"

    if (chr_tg) {
      message("classification requires target to be a factor with the first level as the event class")
      stop()
    }


    xgboost_spec0 %>%
      parsnip::set_mode(mode_set)  -> xgboost_spec

  }

  xgboost_workflow <-
    workflows::workflow() %>%
    workflows::add_recipe(xgboost_recipe) %>%
    workflows::add_model(xgboost_spec)

  xgboost_workflow %>%
    parsnip::fit(.data) -> model_fit

  model_fit %>%
    workflows::pull_workflow_fit() %>%
    purrr::pluck("fit") -> xgbooster



  if (utils::packageVersion("parsnip") > "1.0.0") {
    xgbooster$call$objective -> xgb_obj
  } else {
    xgbooster$call$params$objective -> xgb_obj
  }

  if(validate & xgb_obj != "multi:softprob"){

    rsample::initial_split(.data) -> split1
    rsample::assessment(split1) -> assessment_set
    rsample::analysis(split1) -> analysis_set


    xgboost_workflow %>%
      parsnip::fit(assessment_set) -> val_fit

    val_fit %>%
      workflows::pull_workflow_fit() %>%
      purrr::pluck("fit") -> val_booster

suppressMessages({
    val_booster %>%
      tidy_predict(newdata = analysis_set, form = formula) -> val_frame})

}

model <- .estimate <- .estimator <- .metric <-  NULL

  if(mode_set == "regression"){
    val_frame %>%
      eval_preds() %>%
      dplyr::select(.metric, .estimate) -> val_acc

  }

else if(xgb_obj == "binary:logistic"){

      val_frame %>%
        eval_preds() %>%
        dplyr::select(.metric, .estimate) %>%
        dplyr::filter(.metric == "roc_auc") -> val_acc

      val_frame %>%
        yardstick::conf_mat(truth = !!target
                            , estimate = -1) -> val_conf

      val_frame %>%
        dplyr::count(!!target, sort = T) %>%
        dplyr::pull(n) -> target_counts

      val_frame %>%
        dplyr::count(!!target, sort = T) %>%
        dplyr::slice(1) %>%
        dplyr::pull(1) -> majority_class


      val_frame %>%
        yardstick::conf_mat(truth = !!target, estimate = -1) -> val_conf_mat

      val_conf_mat %>%
        summary %>%
        dplyr::select(-.estimator) %>%
        dplyr::mutate(.formula = c("TP + TN / total",
                            NA,
                            "TP / actually P",
                            "TN / actually N",
                            "TP / predicted P",
                            "TN / predicted N",
                            NA,
                            NA,
                            "sens + spec / 2",
                            "predicted P / total",
                            "PPV, 1-FDR",
                            "sens, TPR",
                            "HM(ppv, sens)")) -> val_conf_tbl

      print(ggplot2::autoplot(val_conf_mat, type = "heatmap"))

      event_prop <- target_counts[1] / (target_counts[1] + target_counts[2])

      tibble::tibble(.metric = "baseline_accuracy",
                     .estimate = event_prop,
                     .formula = "majority class / total") -> baseline_acc

      val_conf_tbl %>%
        dplyr::bind_rows(baseline_acc, val_acc)-> val_acc
    }

else if(xgb_obj == "multi:softmax"){




  val_frame %>%
    yardstick::conf_mat(truth = !!target, estimate = -1) -> val_conf_mat

  val_conf_mat %>%
    summary %>%
    dplyr::select(-.estimator) -> val_acc

  print(ggplot2::autoplot(val_conf_mat, type = "heatmap"))


}


    message("accuracy tested on a validation set")

    print(val_acc)

# xgbooster1 <- xgbooster
# xgbooster1$feature_names <- f_formula_to_charvec(formula)


visualize_model(xgbooster) -> imp_plot

print(imp_plot)

xgbooster

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
#' @param aggregate a character vector. Predictors containing the string will be aggregated, and renamed to that string.
#' @param as_table logical, default FALSE. If TRUE returns importances in a data frame
#' @param formula formula for the model. Use to provide original names if xgboost is scrambling the names internally
#' @param measure choose between Gain, Cover, or Frequency for xgboost importance measure
#' @param ... additional arguments for \code{\link[xgboost]{xgb.ggplot.importance}}
#' @keywords internal
#'
#' @return ggplot
#'
plot_varimp_xgboost <- function(xgb,
                                top_n = 10L,
                                aggregate = NULL,
                                as_table = FALSE,
                                formula = NULL,
                                measure = c("Gain", "Cover", "Frequency"), ...){

  agg <- Feature <- new_names <- NULL

  if(!is.null(formula)){
    new_names <- formula %>%
      f_formula_to_charvec()
  } else {
    new_names <- xgb$feature_names
  }

  xgb$feature_names <- new_names
  xgb$feature_names -> f1

  length(f1) -> lf

  as.character(1:lf) -> nms

  measure <- match.arg(measure)

  xgb$feature_names <- nms

  xgboost::xgb.importance(model = xgb ) -> xgb_imp

  as.integer(xgb_imp$Feature) -> rg_ind

  f1[rg_ind] -> unscrambled_names

  xgb_imp$Feature <- unscrambled_names

  xgb$feature_names <- f1



  if(!is.null(aggregate)){

    xgb_imp %>%
      dplyr::mutate(agg = stringr::str_extract(Feature, stringr::str_c(
        aggregate, collapse = "|"))) %>%
      dplyr::mutate(Feature = dplyr::coalesce(agg, Feature)) %>%
      dplyr::select(-agg) %>%
      dplyr::group_by(Feature) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), sum)) %>%
      data.table::as.data.table() -> xgb_imp
  }


  xgb_imp %>%
    xgboost::xgb.ggplot.importance(..., measure = measure, top_n = top_n) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))+
    ggeasy::easy_remove_legend() +
    ggplot2::ylab(stringr::str_c("Importance from xgboost ", measure)) -> xgb_plot

  if(as_table){

    imp_out <- xgb_imp
  } else{
    imp_out <- xgb_plot
  }

  imp_out
}


#' create monotone constraints
#'
#' helper function to create the integer vector to pass to the \code{monotone_constraints} argument in xgboost
#'
#' @param .data dataframe, training data for tidy_xgboost
#' @param formula formula used for tidy_xgboost
#' @param decreasing character vector or tidyselect regular expression to designate decreasing cols
#' @param increasing character vector or tidyselect regular expression to designate increasing cols
#'
#' @return a named integer vector with entries of 0, 1, -1
#' @export
#'
#' @examples
#'
#'
#'
#' iris %>%
#'framecleaner::create_dummies(Species) -> iris_dummy
#'
#'iris_dummy %>%
#'  tidy_formula(target= Petal.Length) -> petal_form
#'
#'iris_dummy %>%
#'  create_monotone_constraints(petal_form,
#'                              decreasing = tidyselect::matches("Petal|Species"),
#'                              increasing = "Sepal.Width")
#'
create_monotone_constraints <- function(.data, formula, decreasing = NULL, increasing = NULL){

  formula %>%
    f_formula_to_charvec(.data = .data) -> cols

  .data %>%
    framecleaner::select_otherwise(decreasing) -> dec

  .data %>%
    framecleaner::select_otherwise(increasing) -> inc

  mc <- list()

  for(i in cols){

    mc[[i]] <- dplyr::case_when(
      i %in% dec ~ -1L,
      i %in% inc ~ 1L,
      TRUE ~ 0L
    )
  }

  unlist(mc)
}
