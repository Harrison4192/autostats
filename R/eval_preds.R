#' eval_preds
#'
#' Automatically evaluates predictions created by \code{\link{tidy_predict}}. No need to supply column names.
#'
#' @param .data dataframe as a result of \code{\link{tidy_predict}}
#' @param softprob_model character name of the model used to create multiclass probabilities
#' @param ... additional metrics from \href{https://yardstick.tidymodels.org/articles/metric-types.html}{yarstick} to be calculated
#'
#' @return tibble of summarized metrics
#' @export
#'
eval_preds <- function(.data, ..., softprob_model = NULL){



  .data %>%
    names %>%
    stringr::str_subset("_preds_") %>%
    stringr::str_remove("_preds_.*") %>%
    unique() -> col_chr




stringr::str_c(col_chr, "_preds_") %>%
  stringr::str_c(collapse = "|") -> col_regex


.data %>%
  names %>%
  stringr::str_subset(col_regex) -> pred_cols



pred_cols %>%
  stringr::str_subset("_prob_", negate = TRUE) -> pred_cols



if(!is.null(softprob_model)) {
  softprob_model <- rlang::as_name(rlang::enexpr(softprob_model))


 softprob_cols <- .data %>% names %>% stringr::str_subset(softprob_model)

 pred_cols <- setdiff(pred_cols, softprob_cols)
}



#
#   if(rlang::is_empty(pred_cols) & rlang::is_empty(softprob_cols)  ){
#     rlang::abort("you only supplied columns that weren't created by tidy_predict")
#   }


 metric_list <- list()



  for(pred in pred_cols){


    .data %>%
      dplyr::pull(pred) %>%
      determine_pred_type() -> pred_type


    pred %>%
      stringr::str_extract("_preds_.*") %>%
      stringr::str_remove("_preds_") %>%
      stringr::str_remove("class_|prob_")-> model_name


    pred %>%
      stringr::str_remove("_preds_.*")  -> col_chr


    if(!col_chr %in% names(.data)){
      rlang::abort(stringr::str_c("the truth column ", col_chr, " for estimate ", pred, " is not available in the data"))
    }




    if(pred_type %in% c("binary", "binaryprob")) {

      class_preds <- stringr::str_c(col_chr, "_preds_","class_", model_name)
      pred <- stringr::str_c(col_chr, "_preds_","prob_", model_name)




      yardstick::metric_set(yardstick::f_meas, yardstick::roc_auc, yardstick::accuracy, ...) -> eval_func

      list(eval_func(.data, truth = !!rlang::sym(col_chr), !!rlang::sym(pred), estimate = !!rlang::sym(class_preds)) %>%
             dplyr::mutate(model = model_name,
                    target = col_chr)) %>%
        rlist::list.append(metric_list) -> metric_list

    } else if(pred_type == "multiclass") {

      class_preds <- stringr::str_c(col_chr, "_preds_class_", model_name)

      if(!is.null(softprob_model)) {


      yardstick::metric_set(yardstick::f_meas, yardstick::roc_auc, yardstick::accuracy, ...) -> eval_func
      rlang::syms(softprob_cols) -> softprob_cols1

      list(eval_func(.data, truth = !!rlang::sym(col_chr), !!!softprob_cols1, estimate = !!rlang::sym(class_preds)) %>%
             dplyr::mutate(model = model_name,
                    target = col_chr)) %>%
        rlist::list.append(metric_list) -> metric_list
      } else {

        yardstick::metric_set(yardstick::f_meas, yardstick::accuracy, ...) -> eval_func

        list(eval_func(.data, truth = !!rlang::sym(col_chr), estimate = !!rlang::sym(class_preds)) %>%
               dplyr::mutate(model = model_name,
                             target = col_chr)) %>%
          rlist::list.append(metric_list) -> metric_list

      }


    } else if(pred_type == "numeric"){

      yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::ccc, ...) -> eval_func


      list(eval_func(.data, truth = !!rlang::sym(col_chr), estimate = !!rlang::sym(pred)) %>%
             dplyr::mutate(model = model_name,
                    target = col_chr)) %>%
        append(metric_list) -> metric_list

    }



  }
 model <- target <-  .metric <- NULL

  metric_list %>%
    rlist::list.rbind() %>%
    dplyr::arrange(.metric, model, target)

}
