




# create decision tree models ---------------------------------------------

#' tidy ctree
#'
#' tidy conditional inference tree. Creates easily interpretable decision tree models that be shown with the \code{\link{visualize_model}} function.
#' Statistical significance required for a split , and minimum necessary samples in a terminal leaf can be controlled to create the desired tree visual.
#'
#'
#' @param .data dataframe
#' @param formula formula
#' @param minbucket minimum amount of samples in terminal leaves, default is 7
#' @param mincriterion (1 - alpha)   value between 0 -1, default is .95. lowering this value creates more splits, but less significant
#' @param ... optional parameters to \code{\link[party]{ctree_control}}
#'
#' @return a ctree object
#' @export
#'
#' @examples
#'
#' iris %>%
#' tidy_formula(., Sepal.Length) -> sepal_form
#'
#' iris %>%
#' tidy_ctree(sepal_form) %>%
#' visualize_model()
#'
#' iris %>%
#' tidy_ctree(sepal_form, minbucket = 30) %>%
#' visualize_model(plot_type = "box")
#'
#'
tidy_ctree <- function(.data, formula, minbucket = 7L, mincriterion = .95, ...){

party::ctree(formula = formula,
             data = .data,
      controls =  party::ctree_control(..., minbucket = minbucket, mincriterion = mincriterion))
}

#' plot ctree
#'
#' @param ctree_obj output of tidy_ctree
#' @param plot_type type of plot
#' @keywords internal
#'
#' @return decision tree plot
plot_ctree <- function(ctree_obj, plot_type = c("sample", "box", "bar")){

plot_type <- match.arg(plot_type)

if(plot_type == "sample"){
  tree_plot <- function(ctree_obj0){

    { plot(
      ctree_obj0,
      inner_panel = party::node_inner(
        ctree_obj0,
        digits = 3,
        abbreviate = FALSE,
        fill = "skyblue",
        pval = TRUE,
        id = TRUE
      ),
      terminal_panel = party::node_terminal(
        ctree_obj0,
        digits = 3,
        abbreviate = FALSE,
        fill = c("lightgray", "white"),
        id = T
      ),
      edge_panel = party::edge_simple(
        ctree_obj0,
        digits = 2
      )
    )}
  }
} else if(plot_type == "box"){
  tree_plot <- function(ctree_obj0){
    plot(
      ctree_obj0,
      inner_panel = party::node_inner(
        ctree_obj0,
        digits = 3,
        abbreviate = FALSE,
        fill = "skyblue",
        pval = TRUE,
        id = TRUE
      ),
      terminal_panel = party::node_boxplot(
        ctree_obj0
      ),
      edge_panel = party::edge_simple(
        ctree_obj0,
        digits = 2
      )
    )
  }
} else {
  tree_plot <- function(ctree_obj0){
    plot(
      ctree_obj0,
      inner_panel = party::node_inner(
        ctree_obj0,
        digits = 3,
        abbreviate = FALSE,
        fill = "skyblue",
        pval = TRUE,
        id = TRUE
      ),
      terminal_panel = party::node_barplot(
        ctree_obj0
      ),
      edge_panel = party::edge_simple(
        ctree_obj0,
        digits = 2
      )
    )
  }
}

  tree_plot(ctree_obj)
}
