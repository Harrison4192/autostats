

#' tidy conditional inference forest
#'
#' Runs a conditional inference forest.
#'
#' @param data dataframe
#' @param formula formula
#' @param seed seed integer
#'
#' @return a cforest model
#' @export
#'
#' @examples
#'
#' iris %>%
#' tidy_cforest(
#'   tidy_formula(., Petal.Width)
#' ) -> iris_cfor
#'
#' iris_cfor
#'
#' iris_cfor %>%
#' visualize_model()

tidy_cforest <- function(data, formula, seed = 1) {
  set.seed(seed)

  data %>%
    dplyr::mutate(dplyr::across(where(is.character), factor)) -> data

  formula %>%
    rlang::f_lhs() -> target


  formula %>%
    f_formula_to_charvec(include_lhs = TRUE) -> all_vars

  data %>% framecleaner::filter_missing(tidyselect::any_of(all_vars)) -> data


  suppressWarnings({
    party::cforest(formula = formula,
                   data = data)
  })
}

#' plot cforest variable importance
#'
#' @param cfar cforest model
#' @param font font
#' @keywords internal
#'
#' @return ggplot
plot_varimp_cforest <- function(cfar, font = c("", "HiraKakuProN-W3")) {

  font = match.arg(font)

  cfar %>%
    party::varimp() %>%
    moreparty::ggVarImp() +
    ggplot2::geom_bar(stat = "identity", fill = "blue") +
    ggplot2::theme_minimal(base_family=font)+
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))

}

# tidy lightgbm
#
# @param data dataframe
# @param target target variable
# @param ... tidyselect predictor variables
# @param regression_objective_fun objective function string
# @param verbose verbose integer
# @param seed seed integer
#
# @return lightgbm model
#
#tidy_lightgbm <- function(data, target, ..., regression_objective_fun = "regression", verbose = -1, seed = 1 ){
#'
#  set.seed(seed)
#'
#   data %>%
#    select_otherwise(...,
#                     otherwise = tidyselect::everything(),
#                     col = !!target,
#                     return_type = "df") -> data
#'
#  data %>%
#    tidyr::drop_na(!!target) -> data
#'
#'
#'
#  data %>%
#    dplyr::select(where(is.character) | where(is.factor)) %>%
#    names() -> char_names
#'
#  rlang::as_name(rlang::ensym(target)) -> tg_name
#'
#  tg_name %in% char_names -> tg_is_char
#'
#    if(tg_is_char){
#'
#'
#'
#      char_names %>% setdiff(tg_name) -> char_names
#'
#      data %>%
#        dplyr::pull(!!target) %>%
#        dplyr::n_distinct() -> tg_lvls
#'
#  data %>% lightgbm::lgb.convert_with_rules(rules = rlang::list2(!!tg_name := 0:(tg_lvls - 1))) %>% purrr::pluck("data") -> d1
#'
#  if(tg_lvls == 2){
#    objective_fun <- "binary"
#    tg_lvls <- 1
#  } else{
#    objective_fun <- "multiclass"}}
#   else{
#    data %>% lightgbm::lgb.convert_with_rules() %>% purrr::pluck("data") -> d1
#    tg_lvls <- 1
#    objective_fun <- regression_objective_fun
#  }
#'
#  d1 %>%
#    dplyr::select(-!!target) -> d1_train
#'
#  d1 %>%
#    dplyr::pull(!!target) -> dlabel
#'
#  Matrix::Matrix(as.matrix(d1_train), sparse = T) -> dmatrix
#'
#'
#  lightgbm::lgb.Dataset(data = dmatrix, label = dlabel, categorical_feature = char_names) -> lmatrix
#  lightgbm::lgb.train(data = lmatrix, obj = objective_fun , verbose = verbose, num_class = tg_lvls) -> lgbm_model
#'
#  lgbm_model
#}

#' tidy glm
#'
#' Runs either a linear regression, logistic regression, or multinomial classification. The model is
#' automatically determined based off the nature of the target variable.
#'
#' @param data dataframe
#' @param formula formula
#'
#' @return glm model
#' @export
#'
#' @examples
#'
#' # linear regression
#' iris %>%
#' tidy_glm(
#' tidy_formula(., target = Petal.Width)) -> glm1
#'
#' glm1
#'
#' glm1 %>%
#' visualize_model()
#'
#' # multinomial classification
#'
#' tidy_formula(iris, target = Species) -> species_form
#'
#' iris %>%
#' tidy_glm(species_form) -> glm2
#'
#'
#' glm2 %>%
#' visualize_model()
#'
#' #  logistic regression
#' iris %>%
#' dplyr::filter(Species != "setosa") %>%
#' tidy_glm(species_form) -> glm3
#'
#'suppressWarnings({
#' glm3 %>%
#' visualize_model()})
tidy_glm <- function(data, formula) {

  formula %>%
    rlang::f_lhs() -> target

  data %>% dplyr::pull(!!target) -> trg
  trg %>% dplyr::n_distinct() -> target_levels
  trg %>% is.numeric() -> is_tg_numeric

  if(target_levels == 2){
    glm_family <-  "binomial"

    data %>%
      dplyr::mutate(!!target := factor(!!target)) -> data

    data %>%
      dplyr::ungroup() %>%
      dplyr::count(!!target, sort = T) %>%
      dplyr::slice(2) %>%
      dplyr::pull(1) -> target_class

    # data %>%
    #   dplyr::mutate(target_flag := ifelse(!!target == target_class, 1, 0),
    #   mean1 = (nrow(.) - sum(target_flag)) / sum(target_flag),
    #   mean2 = ifelse(target_flag == 1, mean1, 1)) %>%
    #   dplyr::pull(mean2) %>%
    #   ceiling -> weight_vec

    suppressWarnings({
      model <- stats::glm(formula = formula,  data = data, family = glm_family)
    })


  } else if(target_levels != 2 & !is_tg_numeric){
    glm_family <- "multinomial classification"

    data %>%
      dplyr::mutate(!!target := factor(!!target)) -> data


    suppressMessages({
      nnet::multinom(formula = formula, data = data, trace = FALSE) -> model    })

    model$family$family <- glm_family
    model$call$formula <- formula
    model$call$data <- data




  }
  else{
    glm_family <-  "gaussian"
    suppressWarnings({
    model <- stats::glm(formula = formula,  data = data, family = glm_family)
    })
  }

  model
}

#' plot glm coefs
#'
#' @param glm glm
#' @param font font
#' @keywords internal
#'
#' @return plot
plot_coefs_glm <- function(glm, font = c("", "HiraKakuProN-W3")){

  font = match.arg(font)

  glm %>%
  jtools::plot_coefs(., scale = scale) +
    ggplot2::theme_minimal(base_family= font) +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))+
    ggplot2::ylab(label = "") +
    ggplot2::xlab(stringr::str_c("Coefficients from ",  glm$family$family, " model"))
}

is_probability <- function (x)
{
  is.double(x) && all(dplyr::between(x, 0, 1), na.rm = T) &
    dplyr::n_distinct(x) > 2
}
