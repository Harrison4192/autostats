tidy_anova <- function(data, cols = everything() ){

  data %>%
    select({{cols}}) %>%
    janitor::remove_constant(., na.rm = T)-> data1

  data1 %>% dplyr::select(where(is.numeric)) %>% names -> response_names
  data1 %>% dplyr::select(!where(is.numeric)) %>% names -> term_names


  reslist <- list()
  nmslist <- list()

  for(i in response_names){
    for(j in term_names){


      data %>%
        lm(rlang::new_formula(sym(i), sym(j)), data = .) -> lm1

      lm1 %>%
        anova %>%
        tidy %>%
        slice(1)  %>%
        mutate(predictor_significance = gtools::stars.pval(p.value),
               predictor_p.value = p.value, .keep = "unused") %>%
        rename(predictor = term) %>%
        mutate(response = i, predictor = j, .before = 1) %>%
        select(response, predictor, predictor_p.value, predictor_significance) -> anova_output

      lm1 %>%
        tidy %>%
        mutate(
          response = i,
          predictor = j,
          level = stringr::str_remove(term, j),
          .before = 1,
          .keep = "unused"
        ) %>%
        rename(level_p.value = p.value) %>%
        select(-std.error, -statistic) %>%
        mutate(level_significance = gtools::stars.pval(level_p.value)) %>%
        left_join(anova_output, by = c("response", "predictor")) -> res1

      reslist %>% rlist::list.append(res1) -> reslist

    }

  }

  reslist %>%
    reduce(.f = bind_rows) %>%
    arrange(response, predictor, level)
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
