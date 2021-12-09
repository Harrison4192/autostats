#' auto anova
#'
#' A wrapper around lm and anova to run a regression of a continuous variable against categorical variables.
#' Used for determining the whether the mean of a continuous variable is statistically significant amongst different levels
#' of a categorical variable.
#'
#' Columns can be inputted as unquoted names or tidyselect. Continuous and categorical variables are automatically determined.
#' If no character or factor column is present, the column with the lowest amount of unique values will be considered
#' the categorical variable.
#'
#' Description of columns in the output
#'
#' \itemize{
#' \item{\emph{target}}{ continuous variables}
#' \item{\emph{predictor}}{ categorical variables}
#' \item{\emph{level}}{ levels in the categorical variables}
#' \item{\emph{estimate}}{ difference between level target mean and baseline}
#' \item{\emph{target_mean}}{ target mean per level}
#' \item{\emph{n}}{ rows in predictor level}
#' \item{\emph{std.error}}{ standard error of target in predictor level}
#' \item{\emph{level_p.value}}{ p.value for t.test of whether target mean differs significantly between level and baseline}
#' \item{\emph{level_significance}}{ level p.value represented by stars}
#' \item{\emph{predictor_p.value}}{ p.value for significance of entire predictor given by F test}
#' \item{\emph{predictor_significance}}{ predictor p.value represented by stars}
#' \item{\emph{conclusion}}{ text interpretation of tests}
#' }
#'
#'
#' @param data a data frame
#' @param ... tidyselect specification or cols
#' @param sparse default FALSE; if true returns a truncated output with only significant results
#' @param pval_thresh control significance level for sparse output filtering
#' @param baseline choose from "mean", "median", "first_level", "user_supplied". what is the baseline to compare each category to? can use the mean and median of the target variable as a global baseline
#' @param user_supplied_baseline if intercept is "user_supplied", can enter a numeric value
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' iris %>%
#' auto_anova(tidyselect::everything()) -> iris_anova1
#'
#'
#' iris_anova1 %>%
#' print(width = Inf)
auto_anova <- function(data, ... , baseline = c("mean", "median", "first_level", "user_supplied"), user_supplied_baseline = NULL, sparse = FALSE, pval_thresh = .1){

  everything <- p.value <- term <- target <- predictor <- predictor_p.value <- predictor_significance <- NULL
  std.error <- statistic <- level_p.value <- n <- level <- conclusion <- NULL
  value <- estimate <- intercept <- intercept_name <- level_significance <- star_meaning <- anova_meaning <- NULL
  baseline <-  match.arg(baseline)


  data %>%
    janitor::remove_constant(., na.rm = T) -> data


  data %>%
    framecleaner::select_otherwise(..., otherwise = NULL, return_type = "df") -> data1


  data1 %>% dplyr::select(where(is.numeric)) %>% names -> target_names
  data1 %>% dplyr::select(!where(is.numeric)) %>% names -> term_names

  if(rlang::is_empty(term_names)){

    data1 %>%
      get_min_unique_col_names() -> term_names

    target_names <- target_names %>% setdiff(term_names)

    data <- data %>%
      framecleaner::set_chr(tidyselect::any_of(term_names))

  }


  reslist <- list()
  nmslist <- list()

suppressWarnings({

  for(i in target_names){
    for(j in term_names){


     if(baseline == "mean"){
       data1 %>%
         framecleaner::set_chr(tidyselect::any_of(j)) %>%
         dplyr::bind_rows(tibble::tibble("{i}" := data1[[i]], "{j}" := rep("GLOBAL_MEAN", nrow(data1)))) %>%
         framecleaner::set_fct(tidyselect::all_of(j), first_level = "GLOBAL_MEAN") -> data2
     }

     else if(baseline == "median"){
        data1 %>%
         framecleaner::set_chr(tidyselect::any_of(j)) %>%
          dplyr::bind_rows(tibble::tibble("{i}" := stats::median(data1[[i]], na.rm = T), "{j}" := rep("GLOBAL_MEDIAN", nrow(data1)))) %>%
         framecleaner::set_fct(tidyselect::all_of(j), first_level = "GLOBAL_MEDIAN")-> data2
     }

      else  if(baseline == "user_supplied"){
        data1 %>%
          framecleaner::set_chr(tidyselect::any_of(j)) %>%
          dplyr::bind_rows(tibble::tibble("{i}" := user_supplied_baseline, "{j}" := rep("VALUE", nrow(data1)))) %>%
          framecleaner::set_fct(tidyselect::all_of(j), first_level = "VALUE")-> data2
      }

      else if(baseline == "first_level"){
        data1 %>%
          framecleaner::set_fct(tidyselect::all_of(j))-> data2
      }

      data2 %>%
        ggplot2::remove_missing(vars = c(i,j), na.rm = T) -> data2

      data2 %>%
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


      data2 %>%
        dplyr::count(!!rlang::sym(j)) %>%
        rlang::set_names(c("level", "n")) %>%
        framecleaner::set_chr(1) -> target_count

      target_count[[1,1]] <- "(Intercept)"


      lm1 %>%
        broom::tidy() %>%
        dplyr::mutate(
          target = i,
          predictor = j,
          level = stringr::str_remove(enc2utf8(term), j),
          .before = 1,
          .keep = "unused"
        ) %>%
        dplyr::rename(level_p.value = p.value) %>%
        dplyr::select(-statistic) %>%
        dplyr::mutate(level_significance = gtools::stars.pval(level_p.value)) %>%
        dplyr::left_join(anova_output, by = c("target", "predictor")) %>%
        dplyr::left_join(target_count, by = "level") %>%
        dplyr::relocate(n, .after = "estimate") -> res1

      reslist %>% rlist::list.append(res1) -> reslist

    }

  }

})

  reslist %>%
    purrr::reduce(.f = dplyr::bind_rows) %>%
    dplyr::arrange(target, predictor, level) -> res1

suppressMessages({
  res1$predictor %>% unique() -> anova_cols

  data2 %>%
    dplyr::select(tidyselect::all_of(anova_cols)) %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(anova_cols)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(value = as.character(value)) %>%
    rlang::set_names(c("predictor", "level")) %>%
    dplyr::mutate(intercept_name = level) %>%
    ggplot2::remove_missing(vars = "level", na.rm = T) -> nm_tbl


  res1 %>%
    dplyr::filter(level == "(Intercept)") %>%
    dplyr::rename(intercept = estimate) -> t1
  res1 %>% dplyr::left_join(t1) %>%
    tidyr::fill(intercept) %>%
   dplyr::mutate(target_mean = ifelse(level != "(Intercept)", estimate + intercept, estimate), .after = "estimate") -> ta1


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
                               stringr::str_glue("the {level} group in {predictor} has a {target} mean of {format(target_mean, digits = 3, trim = T)} which is {star_meaning} from the reference group of {intercept_name} with value {format(intercept, digits = 3, trim = T)}"))) %>%
    dplyr::mutate(level = ifelse(level == "(Intercept)", stringr::str_c(level, intercept_name, sep = "_"), level)) %>%
    dplyr::select(-intercept, -intercept_name, -star_meaning, -anova_meaning) -> ta3
})

if(sparse){

  ta3 %>%
    dplyr::select(-predictor_p.value, -predictor_significance, -conclusion, -std.error) %>%
    dplyr::filter(level_p.value <= pval_thresh) %>%
    dplyr::filter(stringr::str_detect(level, "(Intercept)", negate = T)) -> ta3

}
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
