#' Tune parameter for undersampling percentage
#'
#' @param range Vector of size 2 with the bounds for the range of the parameter. By default, ]0,1[
#' @param trans Transformation function.
#'
#' @return
#' @export
#'
#' @examples
#' und.perc()
#' und.perc() %>% value_sample(5)
und.perc <- function(range = c(0, 1), trans = NULL) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(FALSE, FALSE),
    trans = trans,
    label = c(und.perc = "Undersampling %"),
    finalize = NULL
  )
}

#' Tune parameter for oversampling percentage
#'
#' @param range Vector of size 2 with the bounds for the range of the parameter. By default, ]1,4]
#' @param trans Transformation function.
#'
#' @return
#' @export
#'
#' @examples
#' ove.perc()
#' ove.perc() %>% value_sample(5)
ove.perc <- function(range = c(1, 4), trans = NULL) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(FALSE, TRUE),
    trans = trans,
    label = c(ove.perc = "Oversampling %"),
    finalize = NULL
  )
}

# ove.perc()
# ove.perc() %>% value_sample(5)
