#' auto correlation
#'
#' Finds the correlation between numeric variables in a data frame, chosen using tidyselect.
#' Additional parameters for the correlation test can be specified as in \code{\link[stats]{cor.test}}
#'
#' @param .data data frame
#' @param ... tidyselect cols
#' @param use method to deal with na. Default is to remove rows with NA
#' @param method correlation method
#'
#' @return data frame of correlations
#' @export
#'
#' @examples
#'
#' iris %>%
#' auto_cor()
auto_cor <- function(.data, ...,
                     use = c("pairwise.complete.obs", "all.obs", "complete.obs",
                                         "everything", "na.or.complete"),
                     method =  c("pearson", "kendall", "spearman")){

  na.method <- match.arg(use)

  cor.method <- match.arg(method)

  .data %>%
    framecleaner::select_otherwise(..., otherwise = where(is.numeric), return_type = "names") -> dnames

cor_list <- list()
  for(i in dnames){

    for(j in setdiff(dnames, i)){

      stats::cor.test(.data[[i]], .data[[j]], use = na.method, method = cor.method) -> cor1

      cor_list <- cor_list %>%
        rlist::list.append(
      tibble::tibble(x = i, y = j, cor = cor1$estimate, p.value = cor1$p.value)
        )
    }
  }


rlist::list.rbind(cor_list)
}
