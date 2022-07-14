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

#und.perc()
#und.perc() %>% value_sample(5)

#################

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

#################

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

data("solubility_test")
sera_vec(
  truth = solubility_test$solubility,
  estimate = solubility_test$prediction,
  ph = IRon::phi.control(solubility_test$solubility)
)

sera <- function(data, ...) {
  UseMethod("sera")
}

sera <- new_numeric_metric(sera, direction = "minimize")

sera.data.frame <- function(data, truth, estimate, ph, na_rm = TRUE, ...) {

  metric_summarizer(
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

ph.aux <- IRon::phi.control(solubility_test$solubility)
sera(solubility_test, truth = solubility, estimate = prediction, ph = ph.aux)

eval_metrics_regression <- metric_set(rmse, rsq, sera)
eval_metrics_regression(solubility_test, truth=solubility, estimate=prediction, ph=ph.aux)

eval_metrics_classification <- metric_set(accuracy, bal_accuracy, f_meas, precision, recall, roc_auc)

#####

show_best_metrics <- function(x, n = 5, ...) {

  summary_res <- estimate_tune_results(x)
  metrics <- unique(summary_res$.metric)
  directions <- metrics %>% map_chr(function(x) attr(match.fun(x),"direction") )

  summary_res_aux <- c()

  for(i in 1:length(metrics)) {
    temp_metric <- summary_res %>% filter(.metric==metrics[[i]])
    if(directions[i]=="maximize") {
      temp_metric <- temp_metric %>% dplyr::arrange(dplyr::desc(mean))
    } else {
      temp_metric <- temp_metric %>% dplyr::arrange(mean)
    }

    show_ind <- 1:min(nrow(temp_metric), n)
    temp_metric <- temp_metric %>% dplyr::slice(show_ind)

    summary_res_aux <- rbind(summary_res_aux, temp_metric)

  }

  summary_res_aux

}

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
