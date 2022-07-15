#' Vector implementation of the SERA measure
#'
#' @param truth
#' @param estimate
#' @param ph
#' @param na_rm
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' data("solubility_test")
#' sera_vec(
#' truth = solubility_test$solubility,
#' estimate = solubility_test$prediction,
#' ph = IRon::phi.control(solubility_test$solubility)
#' )
sera_vec <- function(truth, estimate, ph, na_rm = TRUE, ...) {

  sera_impl <- function(truth, estimate, ph) {
    as.numeric(IRon::sera(trues = truth, preds = estimate, ph = ph))
  }

  metric_vec_template(
    metric_impl = sera_impl,
    truth = truth,
    estimate = estimate,
    ph = ph,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )

}

#' Interface for the SERA measure
#'
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sera <- function(data, ...) {
  UseMethod("sera")
}

sera <- yardstick::new_numeric_metric(sera, direction = "minimize")

#' Data.frame implementation of the SERA measure
#'
#' @param data
#' @param truth
#' @param estimate
#' @param ph
#' @param na_rm
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sera.data.frame <- function(data, truth, estimate, ph, na_rm = TRUE, ...) {

  yardstick::metric_summarizer(
    metric_nm = "sera",
    metric_fn = sera_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    ph = ph,
    metric_fn_options = list(ph=ph),
    na_rm = na_rm,
    ...
  )

}

#' Workaround to add SERA measure results to the
#'
#' @param x
#' @param form
#'
#' @return
#' @export
#'
#' @examples
add_sera <- function(x, form) {

  # change this to a function also, like add_sera()
  for(rsmp in 1:length(x)) {
    aux <- x$.metrics[[rsmp]] %>% group_by(.config) %>% filter(row_number()==1)
    aux <- aux %>% mutate(.metric="sera")

    assess.ds <- as.data.frame(assessment(x$splits[[rsmp]]))
    tgt.id <- which(colnames(assess.ds)==as.character(form[[2]]))
    ph <- IRon::phi.control(assess.ds[,tgt.id])
    preds.ds <- x$.predictions[[rsmp]]
    tgt.id <- which(colnames(preds.ds)==as.character(form[[2]]))
    sera.res <- preds.ds %>% group_by(.config) %>% group_map( ~ IRon::sera(trues = as.data.frame(.x)[,tgt.id], preds = .x$.pred, ph = ph)) %>% unlist() %>% as.numeric()
    aux$.estimate <- sera.res

    x$.metrics[[rsmp]] <- rbind(x$.metrics[[rsmp]],aux) %>% arrange(.config)
  }

  metrics.vec <- unique(x$.metrics[[1]]$.metric)
  attr(x, "metrics") <- do.call(metric_set, syms(metrics.vec))

  x

}

eval_metrics_classification <- yardstick::metric_set(
  yardstick::accuracy,
  yardstick::bal_accuracy,
  yardstick::f_meas,
  yardstick::precision,
  yardstick::recall,
  yardstick::roc_auc)
