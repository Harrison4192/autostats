#' Tidy anova
#'
#' @param data a data frame
#' @param cols tidyselect specification
#'
#' @return data frame
#' @export
#'
tidy_anova <- function(data, cols = everything() ){

  data %>%
    dplyr::select({{cols}}) %>%
    janitor::remove_constant(., na.rm = T)-> data1

  data1 %>% dplyr::select(where(is.numeric)) %>% names -> response_names
  data1 %>% dplyr::select(!where(is.numeric)) %>% names -> term_names


  reslist <- list()
  nmslist <- list()

  for(i in response_names){
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
        dplyr::mutate(response = i, predictor = j, .before = 1) %>%
        dplyr::select(response, predictor, predictor_p.value, predictor_significance) -> anova_output

      lm1 %>%
        broom::tidy() %>%
        dplyr::mutate(
          response = i,
          predictor = j,
          level = stringr::str_remove(term, j),
          .before = 1,
          .keep = "unused"
        ) %>%
        dplyr::rename(level_p.value = p.value) %>%
        dplyr::select(-std.error, -statistic) %>%
        dplyr::mutate(level_significance = gtools::stars.pval(level_p.value)) %>%
        dplyr::left_join(anova_output, by = c("response", "predictor")) -> res1

      reslist %>% rlist::list.append(res1) -> reslist

    }

  }

  reslist %>%
    purrr::reduce(.f = dplyr::bind_rows) %>%
    dplyr::arrange(response, predictor, level)
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
