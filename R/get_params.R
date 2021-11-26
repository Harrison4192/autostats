#' get params
#'
#' s3 method to extract params of a model with names consistent for use in the `autostats` package
#'
#' @param model a model
#' @param ... additional arguments
#'
#' @return list of params
#' @export
#'
#' @examples
#'
#' iris %>%
#' tidy_formula(target = Petal.Length) -> p_form
#'
#' iris %>%
#' tidy_xgboost(p_form, mtry = .5, trees = 5L, loss_reduction = 2, sample_size = .7) -> xgb
#'
#' ## reuse these parameters to find the cross validated error
#'
#' rlang::exec(auto_model_accuracy, data = iris, formula = p_form, !!!get_params(xgb))
get_params <- function(model, ...){

  UseMethod("get_params", model)
}


#' @rdname get_params
#' @method get_params xgb.Booster
#' @export
get_params.xgb.Booster <- function(model, ...){

  model$params %>%
    `[`(c(1:3, 5:7)) %>%
    rlang::set_names(c("learn_rate",
                       "tree_depth",
                       "loss_reduction",
                       "mtry",
                       "min_n",
                       "sample_size"))

}

