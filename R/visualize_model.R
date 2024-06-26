#' visualize model
#'
#' s3 method to automatically visualize the output of of a model object. Additional arguments can be supplied for the original function.
#' Check the corresponding plot function documentation for any custom arguments.
#'
#' @param model a model
#' @param ... additional arguments
#' @param method choose amongst different visualization methods
#' @param top_n return top n elements
#' @param aggregate = summarize
#' @param as_table = false, table or graph,
#' @param formula = formula,
#' @param measure = c("Gain", "Cover", "Frequency")
#'
#' @return a plot
#' @export
visualize_model <- function(model, ...){

    UseMethod("visualize_model", model)
}


#' @rdname visualize_model
#' @method visualize_model RandomForest
#' @export
visualize_model.RandomForest <- function(model, ..., method){

  plot_varimp_cforest(model, ...)
}

#' @rdname visualize_model
#' @method visualize_model BinaryTree
#' @export
visualize_model.BinaryTree <- function(model, ..., method){

  plot_ctree(model, ...)
}

#' @rdname visualize_model
#' @method visualize_model glm
#' @export
visualize_model.glm <- function(model, ..., method){

  plot_coefs_glm(model, ...)
}

#' @rdname visualize_model
#' @method visualize_model multinom
#' @export
visualize_model.multinom <- function(model, ..., method){

  plot_coefs_glm(model, ...)
}

#' @rdname visualize_model
#' @method visualize_model xgb.Booster
#' @export
visualize_model.xgb.Booster <- function(model,
                                        top_n = 10L,
                                        aggregate = NULL,
                                        as_table = FALSE,
                                        formula = NULL,
                                        measure = c("Gain", "Cover", "Frequency"), ..., method){

# if(model$params$booster == "gblinear") {
#
#
#   xgboost::xgb.importance(model = model) -> lin_imp
#
#   if(as_table){
#     lin_imp
#   } else {
#
#  lin_imp %>%
#     xgboost::xgb.ggplot.importance(top_n = top_n)
#   }
#
# } else{

  plot_varimp_xgboost(model,
                      top_n,
                      aggregate,
                      as_table,
                      formula,
                      measure,
                      ...)}
# }

#' @rdname visualize_model
#' @method visualize_model default
#' @export
visualize_model.default <- function(model, ..., method){

  summary(model)
}


#' visualize_model.lgb.Booster <- function(model, ..., method){
#'
#'   lgb.importance(model, ...) %>%
#'     lgb.plot.importance()
#'   }
