eval_preds <- function(.data, col = NULL){

  col <- rlang::enexpr(col)


  if (!is.null(col)) {
    rlang::as_name(col) -> col_chr
  }
  else {
    .data %>%
      names %>%
      str_subset("_preds_") %>%
      stringr::str_remove("_preds_.*") %>%
      unique() -> col_chr


  }

stringr::str_c(col_chr, "_preds_") %>%
  stringr::str_c(collapse = "|") -> col_regex


.data %>%
  names %>%
  stringr::str_subset(col_regex) -> pred_cols



pred_cols %>%
  stringr::str_subset("_class_", negate = TRUE) -> pred_cols




  if(rlang::is_empty(pred_cols)){
    rlang::abort("you only supplied columns that weren't created by tidy_predict")
  }


 metric_list <- list()

  for(pred in pred_cols){


    .data %>%
      dplyr::pull(pred) %>%
      determine_pred_type() -> pred_type


    pred %>%
      stringr::str_extract("_preds_.*") %>%
      stringr::str_remove("_preds_") -> model_name


    pred %>%
      stringr::str_remove("_preds_.*")  -> col_chr

    if(!col_chr %in% names(.data)){
      rlang::abort(stringr::str_c("the truth column ", col_chr, " for estimate ", pred, " is not available in the data"))
    }




    if(pred_type %in% c("binary", "binaryprob")) {

      class_preds <- stringr::str_c(col_chr, "_preds_", "class_", model_name)


      yardstick::metric_set(yardstick::f_meas, yardstick::roc_auc, yardstick::accuracy) -> eval_func

      list(eval_func(.data, truth = !!rlang::sym(col_chr), !!rlang::sym(pred), estimate = !!rlang::sym(class_preds)) %>%
             mutate(model = model_name,
                    target = col_chr)) %>%
        rlist::list.append(metric_list) -> metric_list

    } else if(pred_type == "multiclass") {

      yardstick::metric_set( yardstick::roc_auc, yardstick::accuracy) -> eval_func

      class_preds <- stringr::str_c(col_chr, "_preds_", "class_", model_name)


      yardstick::metric_set(yardstick::f_meas, yardstick::roc_auc, yardstick::accuracy) -> eval_func

      list(eval_func(.data, truth = !!rlang::sym(col_chr), !!rlang::sym(pred)) %>%
             mutate(model = model_name,
                    target = col_chr)) %>%
        rlist::list.append(metric_list) -> metric_list


    } else if(pred_type == "numeric"){

      yardstick::metric_set(yardstick::rmse, yardstick::rsq) -> eval_func

      list(eval_func(.data, truth = !!rlang::sym(col_chr), estimate = !!rlang::sym(pred)) %>%
             mutate(model = model_name,
                    target = col_chr)) %>%
        append(metric_list) -> metric_list

    }



  }
 model <- target <-  .metric <- NULL

  metric_list %>%
    rlist::list.rbind() %>%
    dplyr::arrange(.metric, model, target)

}
