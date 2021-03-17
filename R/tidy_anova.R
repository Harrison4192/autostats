#' Tidy anova
#'
#' @param data a data frame
#' @param cols tidyselect specification
#'
#' @return data frame
#' @export
#'
tidy_anova <- function(data, ... ){

  everything <- p.value <- term <- target <- predictor <- predictor_p.value <- predictor_significance <- NULL
  std.error <- statistic <- level_p.value <- level <- NULL
  value <- estimate <- intercept <- intercept_name <- level_significance <- star_meaning <- anova_meaning <- NULL


  data %>%
    janitor::remove_constant(., na.rm = T) -> data

  data %>%
    select_otherwise(..., otherwise = -where(~guess_id_col(., min_distinct = 10)), return_type = "index") -> cols

  data %>%
    dplyr::select(tidyselect::any_of(cols)) -> data1


  data1 %>% dplyr::select(where(is.numeric)) %>% names -> target_names
  data1 %>% dplyr::select(!where(is.numeric)) %>% names -> term_names

  if(rlang::is_empty(term_names)){

    data1 %>%
      get_min_unique_col_names() -> term_names
  }


  reslist <- list()
  nmslist <- list()

  for(i in target_names){
    for(j in term_names){


      data %>%
        stats::lm(rlang::new_formula(rlang::sym(i), rlang::sym(j)), data = .) -> lm1

      lm1 %>%
        stats::anova() %>%
        broom::tidy() %>%
        dplyr::slice(1)  %>%
        dplyr::mutate(predictor_significance = gtools::stars.pval(p.value),
               predictor_p.value = p.value, .keep = "unused") %>%
        dplyr::rename(predictor = term) %>%
        dplyr::mutate(target = i, predictor = j, .before = 1) %>%
        dplyr::select(target, predictor, predictor_p.value, predictor_significance) -> anova_output

      lm1 %>%
        broom::tidy() %>%
        dplyr::mutate(
          target = i,
          predictor = j,
          level = stringr::str_remove(term, j),
          .before = 1,
          .keep = "unused"
        ) %>%
        dplyr::rename(level_p.value = p.value) %>%
        dplyr::select(-std.error, -statistic) %>%
        dplyr::mutate(level_significance = gtools::stars.pval(level_p.value)) %>%
        dplyr::left_join(anova_output, by = c("target", "predictor")) -> res1

      reslist %>% rlist::list.append(res1) -> reslist

    }

  }

  reslist %>%
    purrr::reduce(.f = dplyr::bind_rows) %>%
    dplyr::arrange(target, predictor, level) -> res1

suppressMessages({
  res1$predictor %>% unique() -> anova_cols

  data %>%
    dplyr::select(tidyselect::all_of(anova_cols)) %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(anova_cols)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(value = as.character(value)) %>%
    rlang::set_names(c("predictor", "level")) %>%
    dplyr::mutate(intercept_name = level) -> nm_tbl


  res1 %>%
    dplyr::filter(level == "(Intercept)") %>%
    dplyr::rename(intercept = estimate) -> t1
  res1 %>% dplyr::left_join(t1) %>%
    tidyr::fill(intercept) %>%
   dplyr::mutate(level_mean = ifelse(level != "(Intercept)", estimate + intercept, estimate)) -> ta1


  nm_tbl %>%
    dplyr::anti_join(ta1) %>% dplyr::mutate(level = "(Intercept)") -> nm_tbl1

  ta1 %>%
    dplyr::left_join(nm_tbl1) %>%
    tidyr::fill(intercept_name) -> ta2

  ta2 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(star_meaning = star_switcher(level_significance),
             anova_meaning = star_switcher(predictor_significance)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(conclusion = ifelse(level == "(Intercept)",
                               stringr::str_glue("the mean of {target} is {anova_meaning} between the levels of {predictor}") ,
                               stringr::str_glue("the {level} group in {predictor} has a {target} mean of {format(level_mean, digits = 3, trim = T)} which is {star_meaning} from the reference group of {intercept_name} with value {format(intercept, digits = 3, trim = T)}"))) %>%
    dplyr::select(-intercept, -intercept_name, -star_meaning, -anova_meaning) -> ta3
})
  ta3
}


star_switcher <- function(stars) {
  switch(
    stars,
    "***" = "HIGHLY significantly different ",
    "**" = "VERY significantly different ",
    "*" = "significantly different ",
    "." = "SLIGHTLY significantly different ",
    "NOT significantly different "
  )
}
