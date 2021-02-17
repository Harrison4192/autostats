#' Plot Variable Contributions
#'
#' Return a variable importance plot and coefficient plot from a linear model. Used to easily visualize
#' the contributions of explanatory variables in a supervised model
#'
#' @param data dataframe
#' @param target target variable name
#' @param ... tidyselect specification for explanatory variables
#'
#' @return a ggplot object
#' @export
plot_variable_contributions <- function(data, target, ...){


  suppressWarnings({
    rlang::as_name(rlang::ensym(target)) -> trg

    purrr::possibly(tidy_glm, otherwise = "multiclass classification problem", quiet = T) -> safe_glm



    data %>%
      safe_glm({{target}}, ...) -> tglm

    if(!is.character(tglm)){

      tglm$family$family -> glm_family

      if(glm_family == "binomial"){
        glm_type <- "logistic regression"
      } else{
        glm_type <- "linear regression"
      }}


    data %>%
      tidy_lightgbm({{target}}, ..., objective_fun = "regression") -> tcf

    tcf %>%
      lightgbm::lgb.importance() %>%
      xgboost::xgb.ggplot.importance() +
      ggplot2::theme_minimal() +
      ggeasy::easy_remove_legend() +
      ggplot2::ylab("Importance from boosted trees")  +
      ggplot2::ggtitle(stringr::str_c("Variable contributions in explaining ", trg)) -> pcf


    purrr::possibly(~jtools::plot_coefs(., robust = "HC0") +
                      ggplot2::theme_minimal() +
                      ggplot2::ylab(label = "") +
                      ggplot2::xlab(stringr::str_c("Coefficient from ", glm_type)),
                    otherwise = "error", quiet = T) -> safe_glm_plot

    safe_glm_plot(tglm) -> plm

    if(!is.character(plm)){
      patchwork::wrap_plots( pcf, plm ) -> plots1
    } else{
      pcf -> plots1
    }

    plots1
  })
}


