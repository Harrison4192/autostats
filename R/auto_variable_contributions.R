#' Plot Variable Contributions
#'
#' Return a variable importance plot and coefficient plot from a linear model. Used to easily visualize
#' the contributions of explanatory variables in a supervised model
#'
#' @param data dataframe
#' @param formula formula
#' @param scale logical. If FALSE puts coefficients on original scale
#' @param font font
#'
#' @return a ggplot object
#' @export
#' @examples
#'
#' iris %>%
#' auto_variable_contributions(
#'  tidy_formula(., target = Petal.Width)
#'  )
#'
#' iris %>%
#' auto_variable_contributions(
#' tidy_formula(., target = Species)
#' )
auto_variable_contributions <- function(data, formula, scale = TRUE, font = c("", "HiraKakuProN-W3")){


  font <- match.arg(font)

  formula %>%
    rlang::f_lhs() -> target

  suppressWarnings({
    rlang::as_name(rlang::ensym(target)) -> trg

    purrr::possibly(tidy_glm, otherwise = "error", quiet = T) -> safe_glm



    data %>%
      safe_glm(formula) -> tglm


    if (!is.character(tglm)) {
      tglm$family$family -> glm_family

      if (glm_family == "binomial") {
        glm_type <- "logistic regression"
      }
      else if (glm_family == "multinomial classification") {
        glm_type <- glm_family
      }
      else{
        glm_type <- "linear regression"
      }
    }


    data %>%
      tidy_xgboost(formula) -> tcf


      xgboost::xgb.importance(model = tcf)   -> tcf_imp


    if(nrow(tcf_imp) > 0){
      tcf_imp %>%
      xgboost::xgb.ggplot.importance() +
        ggplot2::theme_minimal(base_family= font) +
        ggplot2::theme(panel.border = ggplot2::element_blank(),
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              axis.line = ggplot2::element_line(colour = "black"))+
        ggeasy::easy_remove_legend() +
        ggplot2::ylab("Importance from boosted trees")  +
        ggplot2::ggtitle(stringr::str_c("Variable contributions in explaining ", trg)) -> pcf}
    else{
      data %>%
        tidy_cforest(formula) -> tcf

      tcf %>%
        plot_varimp_cforest() +
        ggeasy::easy_remove_legend() +
        ggplot2::xlab("Importance from trees")  +
        ggplot2::ggtitle(stringr::str_c("Variable contributions in explaining ", trg)) -> pcf

    }


    purrr::possibly(~jtools::plot_coefs(., scale = scale) +
                      ggplot2::theme_minimal(base_family= font) +
                      ggplot2::theme(panel.border = ggplot2::element_blank(),
                            panel.grid.major = ggplot2::element_blank(),
                            panel.grid.minor = ggplot2::element_blank(),
                            axis.line = ggplot2::element_line(colour = "black"))+
                      ggplot2::ylab(label = "") +
                      ggplot2::xlab(stringr::str_c("Coefficients from ", glm_type)),
                    otherwise = "error", quiet = F) -> safe_glm_plot

    safe_glm_plot(tglm) -> plm

    if(!is.character(plm)){
      patchwork::wrap_plots( pcf, plm ) -> plots1
    } else{
      pcf -> plots1
    }

    plots1
  })
}


