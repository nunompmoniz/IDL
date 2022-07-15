#' Returns the names of classes
#'
#' @description This function returns the names of classes in a nominal and binary target variable.
#' The vector is ordered: the majority class name is in the first position, and the minority class in the second.
#'
#' @param form A model formula
#' @param ds A data.frame object with the training data
#'
#' @return A vector
#' @export
#'
#' @examples
classNames <- function(tgt) {

  tbl <- table(tgt)

  ind.y <- as.numeric(which(table(tgt)==max(table(tgt))))

  maj.y <- names(tbl)[ind.y]
  min.y <- names(tbl)[-ind.y]

  c(maj.y, min.y)

}

#################


#####

# show_best_metrics <- function(x, n = 5, ...) {
#
#   summary_res <- estimate_tune_results(x)
#   metrics <- unique(summary_res$.metric)
#   directions <- metrics %>% map_chr(function(x) attr(match.fun(x),"direction") )
#
#   summary_res_aux <- c()
#
#   for(i in 1:length(metrics)) {
#     temp_metric <- summary_res %>% filter(.metric==metrics[[i]])
#     if(directions[i]=="maximize") {
#       temp_metric <- temp_metric %>% dplyr::arrange(dplyr::desc(mean))
#     } else {
#       temp_metric <- temp_metric %>% dplyr::arrange(mean)
#     }
#
#     show_ind <- 1:min(nrow(temp_metric), n)
#     temp_metric <- temp_metric %>% dplyr::slice(show_ind)
#
#     summary_res_aux <- rbind(summary_res_aux, temp_metric)
#
#   }
#
#   summary_res_aux
#
# }



