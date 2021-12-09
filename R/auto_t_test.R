#' auto t test
#'
#' Performs a t.test on 2 populations for numeric variables.
#'
#' @param data dataframe
#' @param col a column with 2 categories representing the 2 populations
#' @param ... numeric variables to perform t.test on. Default is to select all numeric variables
#' @param var_equal default FALSE; t.test parameter
#' @param abbrv  default TRUE; remove some extra columns from output
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
#'
#'iris %>%
#'  dplyr::filter(Species != "setosa") %>%
#'  auto_t_test(col = Species)
auto_t_test <- function(data, col, ..., var_equal = FALSE, abbrv = TRUE){

  values <- group <- name <- value <- t_test <- estimate1 <- estimate2 <- p.value <- NULL

  data %>%
    framecleaner::select_otherwise(..., otherwise = where(is.numeric), col = {{col}}, return_type = "df") %>%
    framecleaner::set_chr({{col}}) %>%
    janitor::remove_constant(., na.rm = T) %>%
    tidyr::pivot_longer(cols = where(is.numeric)) %>%
    rlang::set_names(c("group", "name", "value")) %>%
    framecleaner::filter_missing(value) -> data1



  data1$group %>% sort %>% unique -> gv1

  stopifnot(length(gv1) == 2)


    rlang::syms(gv1) -> group_vals



  data1 %>%
    dplyr::group_by(group, name) %>%
    tidyr::nest() %>%
    tidyr::spread(key = group, value = data) %>%
    dplyr::mutate(
      t_test = purrr::map2(!!group_vals[[1]], !!group_vals[[2]], ~{t.test(.x$value, .y$value, var.equal = var_equal) %>% broom::tidy(.)}),
      "{group_vals[1]}_nsize" := purrr::map_dbl(!!group_vals[[1]], nrow),
      "{group_vals[2]}_nsize" := purrr::map_dbl(!!group_vals[[2]], nrow)
    )  %>%
    tidyr::unnest_wider(t_test)  %>%
    dplyr::rename(
      "{group_vals[1]}_mean" := estimate1,
      "{group_vals[2]}_mean" := estimate2) %>%
    dplyr::arrange(p.value) -> t1


  if(abbrv){

    t1 %>%
      dplyr::select(name,
             tidyselect::matches("mean|nsize"),
             p.value) %>%
      dplyr::mutate(significance = gtools::stars.pval(p.value)) -> t1

  }

  t1 %>% dplyr::ungroup(.)

}
