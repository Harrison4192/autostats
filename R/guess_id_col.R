#' #' guess id col
#' #'
#' #' this function uses a series of heuristics to guess whether a column is a type of ID column.
#' #'
#' #' @param x a vector
#' #' @param min_distinct integer, allows the user to choose a minimum threshold for how many unique values are in an ID column
#' #'
#' #' @return logical
#' #' @export
#' guess_id_col <- function(x, min_distinct = 3L){
#'
#'
#' if(!initial_test(x, min_distinct)) return(FALSE)
#' if(!test_integer(x)) return(FALSE)
#' if(!test_character(x)) return(FALSE)
#'
#' TRUE
#'
#' }
#'
#'
#' guess_distribution <- function(x){
#'
#'   d1 <- fitdistrplus::fitdist(x, "pois") %>% fitdistrplus::gofstat()  %>% `$`(chisqpvalue)
#'   d2 <- fitdistrplus::fitdist(x, "norm") %>% fitdistrplus::gofstat() %>% `$`(chisqpvalue)
#'   d3 <- fitdistrplus::fitdist(x, "unif") %>% fitdistrplus::gofstat() %>% `$`(chisqpvalue)
#'
#'   pvals <- c(pois = d1, norm = d1, unif = d3)
#'   names(pvals)[which.max(pvals)]
#' }
#'
#'
#'
#'
#' test_integer <- function(x){if(rlang::is_bare_integer(x)){
#'
#'   clist <- list()
#'
#'   rlist::list.append(clist, function(x) !any(x < 0, na.rm = T) ) -> clist
#'   rlist::list.append(clist, function(x) all(is.finite(x), na.rm = T)) -> clist
#'   rlist::list.append(clist, function(x) !all(dplyr::between(x, 0, 10), na.rm = T)) -> clist
#'   rlist::list.append(clist, function(x) BBmisc::computeMode(x) != 0) -> clist
#'   rlist::list.append(clist, function(x) guess_distribution(x) == "unif") -> clist
#'
#'   for(fn in clist){
#'
#'     if(!rlang::exec(fn, x)){
#'       return(F)
#'     }
#'   }
#'
#' }
#'   TRUE
#' }
#'
#'
#' test_character <- function(x){if(is.character(x)){
#'
#'   clist <- list()
#'
#'   rlist::list.append(clist, function(x) !any(stringr::str_detect(x, "http|www|:|yes|no"), na.rm = T) ) -> clist
#'   rlist::list.append(clist, function(x) !all(stringr::str_length(x) <= 1, na.rm = T)) -> clist
#'
#'   for(fn in clist){
#'
#'     if(!rlang::exec(fn, x)){
#'       return(F)
#'     }
#'   }
#'
#' }
#'
#'   TRUE
#' }
#'
#' initial_test <- function(x, min_distinct = 3){
#'
#'   clist <- list()
#'
#'   rlist::list.append(clist, function(x) !rlang::is_bare_double(x) ) -> clist
#'   rlist::list.append(clist, function(x) !is.logical(x)) -> clist
#'   rlist::list.append(clist, function(x) dplyr::n_distinct(x) >= min_distinct) -> clist
#'
#'
#'   for(fn in clist){
#'
#'     if(!rlang::exec(fn, x)){
#'       return(F)
#'     }
#'   }
#'
#'   TRUE
#' }
#'
#'
