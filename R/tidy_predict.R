#' tidy predict
#'
#' @param model model
#' @param newdata dataframe
#' @importFrom stats predict
#' @param form the formula used for the model
#' @param ... other parameters to pass to \code{predict}
#'
#' @return dataframe
#' @export
tidy_predict <- function(model, newdata, form = NULL, ...){


    UseMethod("tidy_predict", model)
}

#' @rdname tidy_predict
#' @method tidy_predict Rcpp_ENSEMBLE
#' @export
tidy_predict.Rcpp_ENSEMBLE <- function(model, newdata, form = NULL, ...){

 presenter::get_piped_name() -> model_name



  form %>%
    f_formula_to_charvec(.data = newdata) -> predictors

  newdata %>%
    dplyr::select(tidyselect::all_of(predictors)) -> newdata1


  newdata1 %>%
    as.matrix() -> newdata2

  predict(model, newdata = newdata2) -> preds


  new_name <- form %>%
    rlang::f_lhs() %>%
    stringr::str_c("_preds_", model_name)

  newdata %>%
    dplyr::mutate("{new_name}" := preds)
}

#' @rdname tidy_predict
#' @method tidy_predict glm
#' @export
tidy_predict.glm <- function(model, newdata, form = NULL, ...){

  presenter::get_piped_name() -> model_name



  form %>%
    f_formula_to_charvec(.data = newdata) -> predictors

  newdata %>%
    dplyr::select(tidyselect::all_of(predictors)) -> newdata1


  predict(model, newdata = newdata1, ...) -> preds

  new_name <- form %>%
    rlang::f_lhs() %>%
    stringr::str_c("_preds_", model_name)

  newdata %>%
    dplyr::mutate("{new_name}" := preds)

}

#' @rdname tidy_predict
#' @method tidy_predict default
#' @export
tidy_predict.default <- function(model, newdata, form = NULL, ...){

  presenter::get_piped_name() -> model_name



  form %>%
    f_formula_to_charvec(.data = newdata) -> predictors

  newdata %>%
    dplyr::select(tidyselect::all_of(predictors)) -> newdata1


  newdata1 %>%
    as.matrix() -> newdata2


  predict(model, newdata = newdata2, ...) -> preds

  form %>%
    rlang::f_lhs() -> target

  new_name <- target %>%
    stringr::str_c("_preds_", model_name)

  newdata %>%
    dplyr::mutate("{new_name}" := preds)

}

#' @rdname tidy_predict
#' @method tidy_predict xgb.Booster
#' @export
tidy_predict.xgb.Booster <- function(model, newdata, form = NULL, ...){

  presenter::get_piped_name() -> model_name

  model$call$params$objective -> objective


  form %>%
    f_formula_to_charvec(.data = newdata) -> predictors


  newdata %>%
    dplyr::select(tidyselect::all_of(predictors)) -> newdata1


  newdata1 %>%
    as.matrix() -> newdata2



  predict(model, newdata = newdata2, ...) -> preds


 form %>%
  rlang::f_lhs() -> lhs1

 lhs1 %>%
  stringr::str_c("_preds_", model_name) -> new_name




if(objective == "multi:softmax" ){

  classpred_name <-  lhs1 %>% stringr::str_c("_preds_", "class_", model_name)


  newdata %>%
    dplyr::mutate("{classpred_name}" := preds) -> newdata1

  newdata1 %>%
    dplyr::mutate("{classpred_name}" := factor(newdata1[[classpred_name]], labels = unique(newdata1[[lhs1]]))) -> newdata1
} else if(objective == "multi:softprob" ){
  datarows <- newdata[[lhs1]] %>% length
  datanames <- newdata[[lhs1]] %>% levels %>% stringr::str_c("_preds_", "prob_", model_name)
  datacols <-  datanames %>% length




  preds %>%
    matrix(datacols, datarows) %>%
    tibble::as_tibble() %>%
    presenter::pivot_summary() %>%
    dplyr::select(-tidyselect::all_of(1)) %>%
    rlang::set_names(datanames) -> preds1


     newdata %>%
       dplyr::bind_cols(preds1) -> newdata1

  } else if(objective == "binary:logistic"){

    prob_pred_name <-  lhs1 %>% stringr::str_c("_preds_", "prob_", model_name)
    print(prob_pred_name)


    newdata %>%
      dplyr::mutate("{prob_pred_name}" := preds) -> newdata1

    classpred_name <-  lhs1 %>% stringr::str_c("_preds_", "class_", model_name)

    newdata1 %>%
    dplyr::mutate("{classpred_name}" := factor(ifelse(preds > .5,
                                                            levels(!!rlang::sym(lhs1))[1],
                                                            levels(!!rlang::sym(lhs1))[2] ))) -> newdata1


  } else{


    newdata %>%
      dplyr::mutate("{new_name}" := preds) -> newdata1
  }

  newdata1

}


