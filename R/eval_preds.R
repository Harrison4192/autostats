eval_preds <- function(.data, col){

  rlang::as_name(rlang::ensym(col)) -> col_chr

  stringr::str_c(col_chr, "_preds_") -> col_regex


  .data %>%
    names %>%
    stringr::str_subset(col_regex) -> pred_cols


  if(rlang::is_empty(pred_cols)){
    rlang::abort("you only supplied columns that weren't created by tidy_predict")
  }

  .data %>%
    dplyr::pull({{col}}) %>%
    determine_pred_type() -> pred_type

  if(pred_type == "binary") {

    eval_func <- yardstick::classification_cost

  } else if(pred_type == "multiclass") {

    eval_func <- yardstick::accuracy


  } else if(pred_type == "numeric"){

    eval_func <- yardstick::rmse
  }


 metric_list <- list()

  for(pred in pred_cols){

    pred %>%
      stringr::str_extract("_preds_.*") %>%
      stringr::str_remove("_preds_") -> model_name


    list(eval_func(.data, truth = .data[[col_chr]], estimate = .data[[pred]]) %>%
      mutate(model = model_name,
             target = col_chr)) %>%
    rlist::list.append(metric_list)-> metric_list

  }

  metric_list %>%
    rlist::list.rbind()
}
