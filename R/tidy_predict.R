#' tidy predict
#'
#' @param model model
#' @param newdata dataframe
#' @importFrom stats predict
#' @param form the formula used for the model
#' @param olddata training data set
#' @param bind_preds set to TURE if newdata is a dataset without any labels, to bind the new and old data with the predictions under the original target name
#' @param ... other parameters to pass to \code{predict}
#'
#' @return dataframe
#' @export
tidy_predict <- function(model, newdata, form = NULL, olddata = NULL, bind_preds = FALSE, ...){


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
tidy_predict.xgb.Booster <- function(model, newdata, form = NULL, olddata = NULL,  bind_preds = FALSE, ...){

  presenter::get_piped_name() -> model_name

  .ispred <- n <- NULL

  model$call$params$objective -> objective

  if(is.null(olddata)){
    olddata <- newdata}


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

  olddata %>%
    dplyr::pull(!!rlang::sym(lhs1)) %>%
    levels() -> class_levels

  newdata1 %>%
    dplyr::mutate("{classpred_name}" := factor(newdata1[[classpred_name]], labels = class_levels)) -> newdata1

  message(stringr::str_c("created the following column: ", classpred_name))

} else if(objective == "multi:softprob" ){
  datarows <- newdata %>% nrow()
  datanames <- olddata[[lhs1]] %>% levels %>% stringr::str_c("_preds_", "prob_", model_name)
  datacols <-  datanames %>% length




  preds %>%
    matrix(datacols, datarows) %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    presenter::pivot_summary() %>%
    dplyr::select(-tidyselect::all_of(1)) %>%
    rlang::set_names(datanames) -> preds1


     newdata %>%
       dplyr::bind_cols(preds1) -> newdata1

    message("created the following columns: \n", stringr::str_c( stringr::str_c(datanames, "\n")))


  } else if(objective == "binary:logistic"){

    olddata %>%
      dplyr::pull(!!rlang::sym(lhs1)) %>%
      levels() -> class_levels


    prob_pred_name <-  lhs1 %>% stringr::str_c("_preds_", "prob_", model_name)


    newdata %>%
      dplyr::mutate("{prob_pred_name}" := preds) -> newdata1


    classpred_name <-  lhs1 %>% stringr::str_c("_preds_", "class_", model_name)



    newdata1 %>%
    dplyr::mutate("{classpred_name}" := factor(ifelse(preds > .5,
                                                            class_levels[1], class_levels[2]),
                                                levels = class_levels)) -> newdata1

    message("created the following columns: \n", stringr::str_c( prob_pred_name, "\n", classpred_name))

  } else{

    classpred_name <- new_name


    newdata %>%
      dplyr::mutate("{classpred_name}" := preds) -> newdata1

    message(stringr::str_c("created the following column: ", classpred_name))

  }


  if(bind_preds){
    newdata1 %>%
      dplyr::rename("{lhs1}" := !!rlang::sym(classpred_name)) %>%
      dplyr::mutate(.ispred = TRUE) %>%
      dplyr::bind_rows(olddata %>% dplyr::mutate(.ispred = FALSE)) -> newdata1

  }

 newdata1

}


