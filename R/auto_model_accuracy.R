
#' auto model accuracy
#'
#' Runs a cross validated xgboost and regularized linear regression, and reports accuracy metrics.
#' Automatically determines whether the provided formula is a regression or classification.
#'
#' @param data data frame
#' @param formula formula
#' @param n_folds number of cross validation folds
#' @param as_flextable if FALSE, returns a tibble
#' @param theme make_flextable theme
#' @param seed seed
#'
#' @return a table
#' @export
#'
auto_model_accuracy <- function(data,
                               formula,
                               n_folds = 4,
                               as_flextable = TRUE,
                               theme = "tron",
                               seed = 1){

  set.seed(seed)

  data1 <- rlang::ensym(data)

  .config <- model <- .metric <- n <- .estimator <- NULL

  formula %>%
    rlang::f_lhs() -> target

  data %>% dplyr::pull(!!target) -> trg
  trg %>% dplyr::n_distinct() -> target_levels
  trg %>% is.numeric() -> is_tg_numeric

  if(target_levels == 2){
    linear_spec <-
      parsnip::logistic_reg(penalty = .015, mixture = .35)

    data %>%
      dplyr::mutate(!!target := factor(!!target)) -> data

  } else if(target_levels != 2 & !is_tg_numeric){
    linear_spec <-
      parsnip::multinom_reg(penalty = .015, mixture = .35)

    data %>%
      dplyr::mutate(!!target := factor(!!target)) -> data
  } else {
    linear_spec <-  parsnip::linear_reg(penalty = .015, mixture = .35)}


  if(is_tg_numeric){
    mode <- "regression"
  } else{
    mode <- "classification"
  }

  my_rec <-
    recipes::recipe(formula, data = data)  %>%
    recipes::step_nzv(recipes::all_numeric(), -recipes::all_outcomes()) %>%
    recipes::step_corr(recipes::all_numeric(), -recipes::all_outcomes()) %>%
    recipes::step_medianimpute(recipes::all_numeric()) %>%
    recipes::step_modeimpute(recipes::all_nominal()) %>%
    recipes::step_dummy(recipes::all_nominal(), -recipes::all_outcomes())

  my_workflow <-
    workflows::workflow() %>%
    workflows::add_recipe(my_rec)

  boost_spec <-
    parsnip::boost_tree() %>%
    parsnip::set_mode(mode) %>%
    parsnip::set_engine("xgboost")


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


  boost_workflow %>%
    tune::fit_resamples(rsamples) -> boost_sam

  linear_workflow %>%
    tune::fit_resamples(rsamples) -> lin_sam


  boost_sam %>%
    tune::collect_metrics() %>%
    dplyr::mutate(.config = "xgboost") -> c1

  lin_sam %>%
    tune::collect_metrics() %>%
    dplyr::mutate(.config = "regularized linear model") -> c2

  dplyr::bind_rows(c1, c2) %>%
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
      dplyr::mutate(dplyr::across(where(is.numeric), ~format(., digits = 3, trim = T))) -> res

    title <- stringr::str_c(n_folds, " - fold ", " cross-validated accuracy for ", mode, " model of ", target_nm, " on dataset ", tbl_name )

    res %>%
      presenter::make_flextable(last_id_col = 2, theme = theme) %>%
      flextable::add_header_lines(title) -> res


  }

  res
}


