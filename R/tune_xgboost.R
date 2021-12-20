
#' auto_tune_xgboost
#'
#' Automatically tunes an xgboost model using grid or bayesian optimization
#'
#' Default is to tune all 7 xgboost parameters. Individual parameter values can be optionally fixed to reduce tuning complexity.
#'
#' @param .data dataframe
#' @param formula formula
#' @param tune_method method of tuning. defaults to grid
#' @param event_level for binary classification, which factor level is the positive class. specify "second" for second level
#' @param n_fold integer. n folds in resamples
#' @param seed seed
#' @param n_iter n iterations for tuning (bayes); paramter grid size (grid)
#' @param save_output FASLE. If set to TRUE will write the output as an rds file
#' @param parallel default TRUE; If set to TRUE, will enable parallel processing on resamples for grid tuning
#' @inheritParams tidy_xgboost
#'
#' @return workflow object
#' @export
#' @examples
#'
#' if(FALSE){
#'
#'
#'iris %>%
#'  framecleaner::create_dummies() -> iris1
#'
#'iris1 %>%
#'  tidy_formula(target = Petal.Length) -> petal_form
#'
#'iris1 %>%
#'  rsample::initial_split() -> iris_split
#'
#'iris_split %>%
#'  rsample::analysis() -> iris_train
#'
#'iris_split %>%
#'  rsample::assessment() -> iris_val
#'
#'iris_train %>%
#'  auto_tune_xgboost(formula = petal_form, n_iter = 10,
#'  parallel = TRUE, method = "bayes") -> xgb_tuned
#'
#'xgb_tuned %>%
#'  fit(iris_train) %>%
#'  parsnip::extract_fit_engine() -> xgb_tuned_fit
#'
#'xgb_tuned_fit %>%
#'  tidy_predict(newdata = iris_val, form = petal_form) -> iris_val1
#'
#'
#' }
auto_tune_xgboost <- function(.data,
                              formula,
                              tune_method = c("grid", "bayes"),
                              event_level = c("first", "second"),
                              n_fold = 5L,
                              seed = 1,
                              n_iter = 100L,
                              save_output = FALSE,
                              parallel = TRUE,
                              trees = tune::tune(),
                              min_n = tune::tune(),
                              mtry = tune::tune(),
                              tree_depth = tune::tune(),
                              learn_rate = tune::tune(),
                              loss_reduction = tune::tune(),
                              sample_size = tune::tune(),
                              stop_iter = tune::tune(),
                              counts = FALSE,
                              tree_method = c("auto", "exact", "approx", "hist", "gpu_hist"),
                              monotone_constraints = 0L,
                              num_parallel_tree = 1L,
                              lambda = 1,
                              alpha = 0,
                              scale_pos_weight = 1,
                              verbosity = 0L){



  presenter::get_piped_name() -> data_name

  tune_method <- match.arg(tune_method)
  event_level <- match.arg(event_level)
  tree_method <- match.arg(tree_method)


  formula %>%
    rlang::f_lhs() -> target

  .data %>%
    dplyr::pull(!!target) %>%
    is.numeric() -> numer_tg

  if(numer_tg){
    mode_set <- "regression"
  } else{
    mode_set <- "classification"
  }


xgboost_spec1 <-
  workflows::workflow() %>%
  workflows::add_model(
  parsnip::boost_tree(
             trees = !!trees,
             min_n = !!min_n,
             mtry = !!mtry,
             tree_depth = !!tree_depth,
             learn_rate = !!learn_rate,
             loss_reduction = !!loss_reduction,
             sample_size = !!sample_size,
             stop_iter = !!stop_iter
             ) %>%
  parsnip::set_mode(mode_set) %>%
  parsnip::set_engine("xgboost",
                      nthread = 8,
                      counts = counts,
                      tree_method = tree_method,
                      monotone_constraints = monotone_constraints,
                      num_parallel_tree = num_parallel_tree,
                      lambda = lambda,
                      alpha = alpha,
                      scale_pos_weight = scale_pos_weight,
                      verbosity = verbosity)) %>%
  workflows::add_recipe(
    recipe = recipes::recipe( formula = formula,
                     data = .data))


params <- tune::parameters(xgboost_spec1) %>%
  dials::finalize(.data)



.data %>%
  rsample::vfold_cv(v = n_fold) -> folds_tune

if(parallel){

cl <- parallel::makePSOCKcluster(parallel::detectCores(logical = FALSE))
doParallel::registerDoParallel(cl)
}

if(mode_set == "regression"){
  metrics_boost <- yardstick::metric_set(yardstick::rmse, yardstick::rsq)
} else {
  metrics_boost <- yardstick::metric_set(yardstick::roc_auc, yardstick::accuracy)
}

if(tune_method == "bayes"){

xgboost_tune <-
  xgboost_spec1 %>%
  tune::tune_bayes(
    resamples = folds_tune,
    param_info = params,
    initial = 10,
    iter = n_iter,
    metrics = metrics_boost,
    control = tune::control_bayes(save_pred = T,
                                  verbose = T,
                                  no_improve = 30L,
                                  seed = seed,
                                  uncertain = 15,
                                  parallel_over = "resamples",
                                  event_level = event_level
                                  )
  )

} else if(tune_method == "grid"){

  grid_tbl <- dials::grid_max_entropy(params,  size = n_iter)

  xgboost_tune <-
    xgboost_spec1 %>%
    tune::tune_grid(
      resamples = folds_tune,
      grid = grid_tbl,
      metrics = metrics_boost,
      control = tune::control_grid(allow_par = parallel,
                                    verbose = TRUE,
                                    parallel_over = "everything",
                                    event_level = event_level)
      )

}

if(parallel){


doParallel::stopImplicitCluster()
parallel::stopCluster(cl)
}

xgboost_wkflow_tuned <- tune::finalize_workflow(
  xgboost_spec1,
  tune::select_best(xgboost_tune, "rmse")
)

if(save_output){

lubridate::now() %>%
    janitor::make_clean_names( )-> timenow

file_name <- stringr::str_c("xgboost", "wkflow_tuned_", data_name, "_", timenow, ".rds")

tryCatch({

xgboost_wkflow_tuned %>%
  readr::write_rds(file_name)
}, error = function(e) "model didn't save to rds",
warning = function(e) "model didn't save to rds")
}

xgboost_wkflow_tuned

}



