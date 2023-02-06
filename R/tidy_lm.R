# tidy_lm <- function(.data, formula, ...){
#
#   lm(formula, data = .data) -> m1
#
#   model_output <- list()
#
#   model_output$model <- m1
#
#   olsrr::ols_coll_diag(m1) -> coll_diag
#
#   model_output$VIF <- coll_diag$vif_t
#   model_output$CI <- coll_diag$eig_cindex
#
#
#   print(model_output$VIF)
#   print(model_output$CI)
#
#   model_output$model
# }
