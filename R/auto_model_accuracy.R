
#' auto model accuracy
#'
#' Runs a cross validated xgboost and regularized linear regression, and reports accuracy metrics.
#' Automatically determines whether the provided formula is a regression or classification.
#'
#' @param data data frame
#' @param formula formula
#' @param ... any other params for xgboost
#' @param n_folds number of cross validation folds
#' @param as_flextable if FALSE, returns a tibble
#' @param include_linear if TRUE includes a regularized linear model
#' @param theme make_flextable theme
#' @param seed seed
#' @inheritParams tidy_xgboost
#' @param penalty linear regularization parameter
#' @param mixture linear model parameter, combines l1 and l2 regularization
#'
#' @return a table
#' @export
#'
auto_model_accuracy <- function(data,
                               formula,
                               ...,
                               n_folds = 4,
                               as_flextable = TRUE,
                               include_linear = FALSE,
                               theme = "tron",
                               seed = 1,
                               mtry = 1.0,
                               trees = 15L,
                               min_n = 1L,
                               tree_depth = 6L,
                               learn_rate = 0.3,
                               loss_reduction = 0.0,
                               sample_size = 1.0,
                               stop_iter = 10L,
                               counts = FALSE,
                               penalty = .015,
                               mixture = .35){

  set.seed(seed)

  data1 <- rlang::enexpr(data)

  .config <- model <- .metric <- n <- .estimator <- NULL

  formula %>%
    rlang::f_lhs() -> target

  data %>% dplyr::pull(!!target) -> trg
  trg %>% dplyr::n_distinct() -> target_levels
  trg %>% is.numeric() -> is_tg_numeric

  if(target_levels == 2){
    linear_spec <-
      parsnip::logistic_reg(penalty = penalty, mixture = mixture)

    data %>%
      dplyr::mutate(!!target := factor(!!target)) -> data

  } else if(target_levels != 2 & !is_tg_numeric){
    linear_spec <-
      parsnip::multinom_reg(penalty = penalty, mixture = mixture)

    data %>%
      dplyr::mutate(!!target := factor(!!target)) -> data
  } else {
    linear_spec <-  parsnip::linear_reg(penalty = penalty, mixture = mixture)}


  if(is_tg_numeric){
    mode <- "regression"
  } else{
    mode <- "classification"
  }

  my_rec <-
    recipes::recipe(formula, data = data) %>%
    recipes::step_dummy(recipes::all_nominal(), -recipes::all_outcomes())

  # %>%
  #   recipes::step_nzv(recipes::all_numeric(), -recipes::all_outcomes()) %>%
  #   recipes::step_corr(recipes::all_numeric(), -recipes::all_outcomes()) %>%
  #   recipes::step_impute_median(recipes::all_numeric()) %>%
  #   recipes::step_impute_mode(recipes::all_nominal()) %>%

  my_workflow <-
    workflows::workflow() %>%
    workflows::add_recipe(my_rec)

  boost_spec <-
    parsnip::boost_tree(   mtry = mtry,
                           trees = trees,
                           min_n = min_n,
                           tree_depth = tree_depth,
                           learn_rate = learn_rate,
                           loss_reduction = loss_reduction,
                           sample_size = sample_size,
                           stop_iter = stop_iter) %>%
    parsnip::set_mode(mode) %>%
    parsnip::set_engine("xgboost", counts = counts, ...)




  boost_workflow <-
    my_workflow %>%
    workflows::add_model(boost_spec)

    linear_spec %>%
      parsnip::set_mode(mode) %>%
      parsnip::set_engine("glmnet") -> linear_spec

    linear_workflow <-
      my_workflow %>%
      workflows::add_model(linear_spec)



  data %>% rsample::vfold_cv(v = n_folds) -> rsamples

  tune::control_resamples(
    verbose = FALSE,
    allow_par = TRUE,
    extract = NULL,
    save_pred = FALSE,
    pkgs = NULL,
    save_workflow = FALSE,
    event_level = "first",
    parallel_over = NULL
  ) -> controls



  boost_workflow %>%
    tune::fit_resamples(rsamples, control = controls) -> boost_sam

  if(include_linear){
  linear_workflow %>%
    tune::fit_resamples(rsamples, control = controls) -> lin_sam}



  boost_sam %>%
    tune::collect_metrics() %>%
    dplyr::mutate(.config = "xgboost") -> c1

  if(include_linear){
  lin_sam %>%
    tune::collect_metrics() %>%
    dplyr::mutate(.config = "regularized linear model") -> c2
  }

  if(include_linear){
    dplyr::bind_rows(c1, c2) -> c1}

   c1 %>%
    dplyr::rename(model = .config) %>%
    dplyr::relocate(model) %>%
    dplyr::arrange(.metric, model ) %>%
    dplyr::rename(n_folds = n,
                  mean_score = mean) %>%
    dplyr::select(-.estimator) %>%
    dplyr::rename(metric = .metric)  -> res


  if(as_flextable){

    target_nm <- rlang::as_name(rlang::ensym(target))


    presenter::get_piped_name(!!data1) -> tbl_name

    res %>%
      dplyr::select(-n_folds) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~format(., digits = 3L, trim = T))) -> res

    title <- stringr::str_c(n_folds, " - fold ", " cross-validated accuracy for ", mode, " model of ", target_nm, " on dataset ", tbl_name )

    res %>%
      presenter::make_flextable(last_id_col = 2, theme = theme) %>%
      flextable::add_header_lines(title) -> res


  }

  res
}


