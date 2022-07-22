baseline_regression <- function(target_vec_train, target_vec_val){

  mean(target_vec_train, na.rm = T) -> avg

  tibble::tibble(truth = target_vec_val, estimate = avg) -> avg_tib

  yardstick::rmse(avg_tib, truth, estimate)
}
