#' Tidy anova
#'
#' @param data a data frame
#' @param cols tidyselect specification
#'
#' @return data frame
#' @export
#'
tidy_anova <- function(data, cols = everything() ){

  everything <- p.value <- term <- target <- predictor <- predictor_p.value <- predictor_significance <- NULL
  std.error <- statistic <- level_p.value <- level <- NULL


  data %>%
    janitor::remove_constant(., na.rm = T) -> data

  data %>%
    dplyr::select({{cols}}) -> data1

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
    dplyr::arrange(target, predictor, level)
}


star_switcher <- function(stars) {
  switch(
    stars,
    "***" = "HIGHLY significantly different from",
    "**" = "VERY significantly different from",
    "*" = "significantly different from",
    "." = "SLIGHTLY significantly different from",
    "NOT significantly different from"
  )
}
