
#' auto_tune_xgboost
#'
#' Automatically tunes an xgboost model
#'
#' @param .data dataframe
#' @param formula formula
#' @param event_level for binary classification, which factor level is the positive class. specify "second" for second level
#' @param n_fold integer. n folds in resamples
#' @param seed seed
#' @param n_iter n iterations for tuning
#'
#' @return workflow object
#' @export
auto_tune_xgboost <- function(.data,
                              formula,
                              event_level = c("first", "second"),
                              n_fold = 5,
                              seed = 1,
                              n_iter = 100){

  presenter::get_piped_name() -> data_name

  event_level <- match.arg(event_level)

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
             trees = tune::tune(),
             min_n = tune::tune(),
             mtry = tune::tune(),
             tree_depth = tune::tune(),
             learn_rate = tune::tune(),
             loss_reduction = tune::tune(),
             sample_size = tune::tune(),
             stop_iter = 10
             ) %>%
  parsnip::set_mode(mode_set) %>%
  parsnip::set_engine("xgboost", nthread = 8)) %>%
  workflows::add_recipe(
    recipe = recipes::recipe( formula = formula,
                     data = .data) %>%
      recipes::step_zv(recipes::all_predictors()) %>%
      recipes::step_dummy(where(is.character) | where(is.factor), -!!target)

  )


params <- dials::parameters(xgboost_spec1) %>%
  dials::finalize(.data)


.data %>%
  rsample::vfold_cv(v = n_fold) -> folds_tune

# cl <- parallel::makePSOCKcluster(parallel::detectCores(logical = FALSE))
# doParallel::registerDoParallel(cl)

if(mode_set == "regression"){
  metrics_boost <- yardstick::metric_set(yardstick::rmse, yardstick::rsq)
} else {
  metrics_boost <- yardstick::metric_set(yardstick::roc_auc, yardstick::accuracy)
}

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
                                  parallel_over = "everything",
                                  event_level = event_level
                                  )
  )





# doParallel::stopImplicitCluster()
# parallel::stopCluster(cl)


xgboost_wkflow_tuned <- tune::finalize_workflow(
  xgboost_spec1,
  tune::select_best(xgboost_tune, "rmse")
)


lubridate::now() -> timenow

file_name <- stringr::str_c("xgboost", "wkflow_tuned_", data_name, "_", timenow, ".rds")

xgboost_wkflow_tuned %>%
  readr::write_rds(file_name)

xgboost_wkflow_tuned
}



