#' tidy predict
#'
#' @param model model
#' @param newdata dataframe
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

  new_name <- form %>%
    rlang::f_lhs() %>%
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


  newdata %>%
    dplyr::mutate("{new_name}" := preds) -> newdata1



  if(objective == "multi:softmax" ){
    newdata1 %>%
      mutate("{new_name}" := factor(newdata1[[new_name]], labels = unique(newdata1[[lhs1]]))) -> newdata1
  }
  if(objective == "binary:hinge"){


    # newdata1 %>%
    #   mutate("{new_name}" := factor(newdata1[[new_name]], labels = levels(newdata1[[lhs1]]))) -> newdata1
  }

  newdata1

}


