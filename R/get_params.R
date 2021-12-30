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
#'   framecleaner::create_dummies() -> iris_dummies
#'
#' iris_dummies %>%
#'   tidy_formula(target = Petal.Length) -> p_form
#'
#' iris_dummies %>%
#'   tidy_xgboost(p_form, mtry = .5, trees = 5L, loss_reduction = 2, sample_size = .7) -> xgb
#'
#' ## reuse these parameters to find the cross validated error
#'
#' rlang::exec(auto_model_accuracy, data = iris_dummies, formula = p_form, !!!get_params(xgb))
get_params <- function(model, ...){

  UseMethod("get_params", model)
}


#' @rdname get_params
#' @method get_params xgb.Booster
#' @export
get_params.xgb.Booster <- function(model, ...){


  model ->  model1

  model$params[c("eta", "max_depth", "gamma",
                 "colsample_bynode", "min_child_weight", "subsample")] %>%
    c(model[c("niter")])-> newparams


  model1$params[c("eta", "max_depth", "gamma",
                  "colsample_bynode", "min_child_weight", "subsample")] <- NULL



  newparams %>% rlang::set_names(nm = c(eta = "learn_rate",
                                        max_depth = "tree_depth",
                                        gamma = "loss_reduction",
                                        colsample_bynode = "mtry",
                                        min_child_weight = "min_n",
                                        subsample = "sample_size", niter = "trees")) -> newparams1
  c(newparams1,
    model1$params)




}


