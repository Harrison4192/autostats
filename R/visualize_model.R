#' visualize model
#'
#' s3 method to automatically visualize the output of of a model object. Additional arguments can be supplied for the original function.
#' Check the corresponding plot function documentation for any custom arguments.
#'
#' @param model a model
#' @param ... additional arguments
#' @param method choose amongst different visualization methods
#'
#' @return a plot
#' @export
visualize_model <- function(model, ..., method = NULL){

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
visualize_model.xgb.Booster <- function(model, ..., method){

  plot_varimp_xgboost(model, ...)
}

#' @rdname visualize_model
#' @method visualize_model default
#' @export
visualize_model.default <- function(model, ..., method){

  summary(model)
}

