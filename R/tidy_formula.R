
#' charvec to formula
#'
#' takes the lhs and rhs of a formula as character vectors and outputs a formula
#'
#' @param lhs lhs atomic chr vec
#' @param rhs rhs chr vec
#'
#' @return formula
#' @export
#'
#' @examples
#'
#' lhs <- "Species"
#' rhs <- c("Petal.Width", "Custom_Var")
#'
#' f_charvec_to_formula(lhs, rhs)
#'
f_charvec_to_formula <- function(lhs, rhs){

  if(rlang::is_empty(rhs)){return(NULL)} else{

    stringr::str_c(rhs, collapse = " + ") %>%
      stringr::str_c(lhs, " ~ ", ., collapse = "")  %>%
      parse(text = .) %>%
      eval()}
}
#' tidy formula construction
#'
#' Takes a dataframe and allows for use of tidyselect to construct a formula.
#'
#' @param data dataframe
#' @param target lhs
#' @param ... tidyselect. rhs
#' @importFrom framecleaner select_otherwise
#'
#' @return a formula
#' @export
#'
#' @examples
#'
#' iris %>%
#' tidy_formula(Species, tidyselect::everything())
tidy_formula <- function(data, target, ...){


  rlang::as_name(rlang::ensym(target)) %>% enc2utf8() -> lhs_var

  data %>%
    framecleaner::select_otherwise(...,
                                   otherwise = tidyselect::everything(),
                                   return_type = "names") %>%
    setdiff(lhs_var) %>%
    enc2utf8() -> rhs_vars

  f_charvec_to_formula(lhs_var, rhs_vars)
}


#' Formula_rhs to chr vec
#'
#' Accepts a formula and returns the rhs as a character vector.
#'
#' @param f formula
#' @param include_lhs FALSE. If TRUE, appends lhs to beginning of vector
#' @param .data dataframe for names if necessary
#'
#' @return chr vector
#' @export
#'
#' @examples
#'
#' iris %>%
#' tidy_formula(target = Species, tidyselect::everything()) -> f
#'
#' f
#'
#' f %>%
#' f_formula_to_charvec()
f_formula_to_charvec <- function(f, include_lhs = FALSE, .data = NULL){

  f %>%
    rlang::f_lhs() %>%
    as.character() -> lhs



  f %>%
    rlang::f_text() %>%
    enc2utf8() %>%
    stringr::str_split(pattern = "\\+ ", simplify = T) %>%
    stringr::str_remove("\n") %>%
    trimws() %>%
    as.vector() -> form

  f %>%
    rlang::f_rhs() -> rhs

  if(!is.null(.data) &rhs == "."){

    .data %>%
      names() %>%
      setdiff(lhs) -> form
  }

  if(include_lhs){

    form <- append(lhs, form)
  }

  form
}

#' Modify Formula
#'
#' Modify components of a formula by adding / removing vars from the rhs or replacing the lhs.
#'
#' @param f formula
#' @param rhs_remove regex or character vector for dropping variables from the rhs
#' @param rhs_add character vector for adding variables to rhs
#' @param lhs_replace string to replace formula lhs if supplied
#' @param negate should \code{rhs_remove} keep or remove the specified vars. Set to \code{FALSE} to keep
#'
#' @return formula
#' @export
#'
#' @examples
#'
#' iris %>%
#' tidy_formula(target = Species, tidyselect::everything()) -> f
#'
#' f
#'
#' f %>%
#'   f_modify_formula(
#' rhs_remove = c("Petal.Width", "Sepal.Length"),
#' rhs_add = "Custom_Variable"
#' )
#'
#' f %>%
#'   f_modify_formula(
#' rhs_remove = "Petal",
#' lhs_replace = "Petal.Length"
#' )
f_modify_formula <- function(f,
                     rhs_remove = NULL,
                     rhs_add = NULL,
                     lhs_replace = NULL,
                     negate = TRUE){

  if(length(rhs_remove) > 1){
    rhs_remove <- stringr::str_c(rhs_remove, collapse = "|")
  }



  f %>%
    f_formula_to_charvec() %>%
    stringr::str_subset(rhs_remove, negate = negate) %>%
    append(rhs_add) -> rhs

  if(is.null(lhs_replace)){
    lhs_replace <- rlang::f_lhs(f)
  }

  f_charvec_to_formula(lhs_replace, rhs)
}


