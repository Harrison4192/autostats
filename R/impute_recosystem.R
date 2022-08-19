
#' impute_recosystem
#'
#' Imputes missing values of a numeric matrix using stochastic gradient descent.
#' \href{https://CRAN.r-project.org/package=recosystem}{recosystem}
#'
#' input is a long data frame with 3 columns: ID col, Item col (the column names from pivoting longer),
#'  and the ratings (values from pivoting longer)
#'
#'  pre-processing generally requires pivoting a wide user x item matrix to long format.
#'    The missing values from the matrix must be retained as NA values in the rating column.
#'  The values will be predicted and filled in by the algorithm.
#'  Output is a long data frame with the same number of rows as input, but no missing values.
#'
#'  This function automatically tunes the recosystem learner before applying. Parameter values can be supplied for tuning.
#'  To avoid tuning, use single values for the parameters.
#'
#'
#'
#' @param .data long format data frame
#' @param lrate learning rate
#' @param costp_l1 l1 cost p
#' @param costq_l1 l1 cost q
#' @param costp_l2 l2 cost p
#' @param costq_l2 l2 cost q
#' @param nthread nthreads
#' @param loss loss function. also can use "l1"
#' @param niter training iterations for tune
#' @param verbose show training loss?
#' @param nfold folds for tune validation
#' @param seed seed for randomness
#'
#' @return long format data frame
#' @export
#'
impute_recosystem <- function(.data,
                              lrate = c(.05, .1),
                              costp_l1 =c(0, .05),
                              costq_l1 =c(0, .05),
                              costp_l2 =c(0, .05),
                              costq_l2 =c(0, .05),
                              nthread = 8,
                              loss = "l2",
                              niter = 15 ,
                              verbose = FALSE,
                              nfold = 4,
                              seed = 1){


  type <- user <- item <- rating <- user_index <- item_index <- NULL

  stopifnot(
    ncol(.data) == 3,
    .data[[3]] %>% is.numeric()
  )



# recosystem imputation ---------------------------------------------------




.data %>%
    rlang::set_names(c("user", "item", "rating")) %>%
  dplyr::arrange(user, item) -> data1


  data1 %>%
  dplyr::mutate(dplyr::across(c(user, item), ~as.integer(factor(.)), .names = "{col}_index"))-> data3

  data3 %>%
    dplyr::select(-rating) %>%
    dplyr::distinct() -> name_index_dict



  data3 %>%
  dplyr::filter(!is.na(rating)) -> data4


# train reco object
reco_learner <- recosystem::Reco()




data1$item %>% dplyr::n_distinct() -> datacols
ndims <- c(round(datacols / 3),
           round(datacols / 2),
           datacols)




tuned_opts = reco_learner$tune(recosystem::data_memory(user_index = data4$user_index,
                                           item_index = data4$item_index,
                                           rating = data4$rating, index1 = TRUE),
                               opts = list(dim = ndims,
                                           lrate = lrate,
                                           costp_l1 = costp_l1,
                                           costq_l1 = costq_l1,
                                           costp_l2 = costp_l2,
                                           costq_l2 = costq_l2,
                                           nthread = nthread,
                                           loss = loss,
                                           niter = niter ,
                                           verbose = verbose,
                                           nfold = nfold))





tuned_opts$min -> reco_opts



reco_learner$train(recosystem::data_memory(user_index = data4$user_index,
                               item_index = data4$item_index,
                               rating = data4$rating, index1 = TRUE),
                   opts = rlang::list2(
                     !!!reco_opts,
                     loss = loss,
                     niter = 100,
                     verbose = verbose)
                   )



data3 %>%
  dplyr::filter(is.na(rating)) %>%
  dplyr::select(user_index, item_index) -> data5



reco_pred <- reco_learner$predict(recosystem::data_memory(user_index = data5$user_index,
                                              item_index = data5$item_index,
                                              rating = NULL, index1 = TRUE), recosystem::out_memory())

dplyr::bind_cols(data5, tibble::tibble(rating = reco_pred)) -> reco_preds_df

suppressMessages({

reco_preds_df %>%
  dplyr::left_join(name_index_dict) %>%
  dplyr::select(-tidyselect::matches("index"))  -> reco_preds_df1

})

data4 %>%
  dplyr::mutate(type = "real") %>%
  dplyr::bind_rows(reco_preds_df1 %>% dplyr::mutate(type = "pred")) %>%
  dplyr::select(-tidyselect::matches("index"))-> reco_full_df

# validate

reco_learner_val <- recosystem::Reco()


1:nrow(data4) -> drow
set.seed(seed)

sample(drow, size = as.integer(nrow(data4) * .66)) -> train
setdiff(drow, train) -> test


data4 %>%
  dplyr::slice(train) %>%
  dplyr::arrange(user, item) -> dtrain

data4 %>%
  dplyr::slice(test) %>%
  dplyr::arrange(user, item)-> dtest


dtest %>%
  dplyr::select(-rating) %>%
  dplyr::arrange(user, item) -> dtest1





reco_learner_val$train(recosystem::data_memory(user_index = dtrain$user_index,
                                           item_index = dtrain$item_index,
                                           rating = dtrain$rating, index1 = TRUE),
                   opts = rlang::list2(
                     !!!reco_opts,
                     loss = loss,
                     niter = 100,
                     verbose = verbose))



data3 %>%
  dplyr::filter(is.na(rating)) %>%
  dplyr::select(user_index, item_index) -> data5

reco_pred_val <- reco_learner_val$predict(recosystem::data_memory(user_index = dtest1$user_index,
                                                          item_index = dtest1$item_index,
                                                          rating = NULL, index1 = TRUE), recosystem::out_memory())



yardstick::rsq_vec(dtest$rating, reco_pred_val) -> r2

print(stringr::str_c("R^2 score on a 2/3 validation split is ", round(r2, digits = 3)))

reco_full_df %>%
  ggplot2::ggplot(ggplot2::aes(x = rating, color = type)) +
  ggplot2::geom_density() +
  ggplot2::theme_light() +
  ggplot2::ggtitle("Distribution of predictions vs. actual values") -> ggpred

print(ggpred)

reco_full_df

}
