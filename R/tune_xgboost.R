
#' auto_tune_xgboost
#'
#' @param .data dataframe
#' @param target target
#' @param ... explanatory vars
#' @param boost_engine default is xgboost. catboost and lightgbm currently not supported
#' @param event_level for binary classification, which factor level is the positive class. specify "second" for second level
#' @param n_fold integer. n folds in resamples
#' @param seed seed
#' @param n_iter n iterations for tuning
#'
#' @return workflow object
#' @export
auto_tune_xgboost <- function(.data, target, ...,
                              boost_engine = c("xgboost"),
                              event_level = c("first", "second"),
                              n_fold = 5,
                              seed = 1,
                              n_iter = 100){

  get_piped_name() -> data_name

  boost_engine <- match.arg(boost_engine)
  event_level <- match.arg(event_level)

  .data %>%
    tidy_formula({{target}}, ...) -> form

  .data %>%
    dplyr::pull({{target}}) %>%
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
  parsnip::set_engine(boost_engine, nthread = 8)) %>%
  workflows::add_recipe(
    recipe = recipes::recipe( formula = form,
                     data = .data)
  )

params <- dials::parameters(xgboost_spec1) %>%
  dials::finalize(.data)


.data %>%
  rsample::vfold_cv(v = n_fold) -> folds_tune

# cl <- parallel::makePSOCKcluster(parallel::detectCores(logical = FALSE))
# doParallel::registerDoParallel(cl)

xgboost_tune <-
  xgboost_spec1 %>%
  tune::tune_bayes(
    resamples = folds_tune,
    param_info = params,
    initial = 10,
    iter = n_iter,
    metrics = yardstick::metric_set(rmse, rsq),
    control = tune::control_bayes(save_pred = T,
                                  verbose = T,
                                  no_improve = 30L,
                                  seed = seed,
                                  uncertain = 15,
                                  parallel_over = "everything",
                                  event_level = event_level)
  )


# doParallel::stopImplicitCluster()
# parallel::stopCluster(cl)


xgboost_wkflow_tuned <- tune::finalize_workflow(
  xgboost_spec1,
  select_best(xgboost_tune, "rmse")
)


lubridate::now() -> timenow

file_name <- stringr::str_c(boost_engine, "wkflow_tuned_", data_name, "_", timenow, ".rds")

xgboost_wkflow_tuned %>%
  readr::write_rds(file_name)

xgboost_wkflow_tuned
}



