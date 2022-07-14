#' SMOTE for Imbalanced Classification Tasks
#'
#' @param recipe
#' @param ...
#' @param role
#' @param trained
#' @param und.perc
#' @param ove.perc
#' @param neighbors
#' @param dist
#' @param perc.list
#' @param C.perc
#' @param target
#' @param skip
#' @param id
#'
#' @return
#' @export
#'
#' @examples
#' library(tidymodels)
#' library(modeldata)
#' data(hpc_data)
#'
#' hpc_data0 <- hpc_data %>%
#'   select(-protocol, -day)
#'
#' smote_rec <- recipe(class ~ ., data = hpc_data0) %>%
#'   step_SmoteClassif(class, ove.perc = 2, und.perc=0.5, neighbors = 5) %>%
#'   prep()
#'
#' tidy(smote_rec, number=1)
#'
#' training <- smote_rec %>%
#'   bake(new_data = NULL)
#'
#' table(hpc_data0$class)
#' table(training$class)
#'
#' folds <- vfold_cv(hpc_data0, v = 5)
#'
#' tune_rec <- recipe(class ~ ., data = hpc_data0) %>%
#'   step_SmoteClassif(class, ove.perc = tune(), und.perc=tune(), neighbors = tune())
#'
#' lin_mod <-
#'   decision_tree() %>%
#'   set_mode("classification") %>%
#'   set_engine("rpart")
#'
#' wf <- workflow() %>%
#'   add_recipe(tune_rec) %>%
#'   add_model(lin_mod)
#'
#' lambda_grid <- grid_random(ove.perc(),und.perc(),neighbors(),size = 9) #strange bug
#'
#' res <- tune_grid(wf,
#'                  resamples = folds,
#'                  grid = lambda_grid,
#'                  metrics = eval_metrics_classification)
#'
#' estimates <- collect_metrics(res)
#' estimates
#'
#' show_best(res, metric = "accuracy")
#' show_best(res, metric = "bal_accuracy")
#' show_best(res, metric = "roc_auc")
#' show_best(res, metric = "f_meas")
#'
step_SmoteClassif <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  und.perc = NULL,
  ove.perc = 0.5,
  neighbors = 3,
  dist = "Euclidean",
  perc.list = NULL,
  C.perc = NULL,
  target = NULL,
  skip = TRUE,
  id = rand_id("SmoteClassif")
) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_SmoteClassif_new(
      terms = terms,
      trained = trained,
      role = role,
      und.perc = und.perc,
      ove.perc = ove.perc,
      neighbors = neighbors,
      dist = dist,
      perc.list = perc.list,
      C.perc = C.perc,
      target = target,
      skip = skip,
      id = id
    )
  )
}

######################################################################
######################################################################
######################################################################

step_SmoteClassif_new <-
  function(terms, role, trained, und.perc, ove.perc, neighbors, dist, perc.list, C.perc, target, skip, id) {
    step(
      subclass = "SmoteClassif",
      terms = terms,
      role = role,
      trained = trained,
      und.perc = und.perc,
      ove.perc = ove.perc,
      neighbors = neighbors,
      dist = dist,
      perc.list = perc.list,
      C.perc = C.perc,
      target = target,
      skip = skip,
      id = id
    )
  }

######################################################################
######################################################################
######################################################################

#' @export
prep.step_SmoteClassif <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  #print(col_names)

  tgt.info <- info %>% filter(role=="outcome")

  if(tgt.info %>% count() == 0) { rlang::abort("A target variable is required!") }
  if(tgt.info %>% count() > 1) { rlang::abort("Multi-label is not supported at the moment!") }
  if(tgt.info$type != "nominal") { rlang::abort("Expecting a nominal target variable!") }
  if(!(all_of(col_names) %in% tgt.info$variable)) { rlang::abort("Selected variable is not a target variable!") }

  # check perc/perc.list (TO DO)

  # else, assume that the perc is for the most under-represented class

  cls <- training %>% select(col_names) %>% table() %>% sort(decreasing=TRUE) %>% names()
  ove.cls <- cls %>% last()
  und.cls <- cls %>% first()
  C.perc <- NULL
  C.perc[[ove.cls]] <- x$ove.perc
  if(!is.null(x$und.perc)) C.perc[[und.cls]] <- x$und.perc
  x$C.perc <- C.perc

  nom_vars <- info %>% filter(role=="predictor", type!="numeric") %>% nrow()
  if(nom_vars>0) x$dist="HEOM"

  step_SmoteClassif_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    und.perc = x$und.perc,
    ove.perc = x$ove.perc,
    neighbors = x$neighbors,
    dist = x$dist,
    perc.list = x$perc.list,
    C.perc = x$C.perc,
    target = col_names,
    skip = x$skip,
    id = x$id
  )

}

######################################################################
######################################################################
######################################################################

#' @export
bake.step_SmoteClassif <- function(object, new_data, ...) {

  # UBL does not support tibbles yet
  new_data <- UBL::SmoteClassif(form = formula(paste0(object$target,"~ .")), dat = as.data.frame(new_data),
                                dist=object$dist, k=object$neighbors, C.perc = object$C.perc)

  tibble::tibble(new_data)

}

######################################################################
######################################################################
######################################################################

#' @export
print.step_SmoteClassif <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("SMOTE on outcome variable ",x$target,": ",sep="")
    printer(
      # Names before prep (could be selectors)
      untr_obj = x$terms,
      # Names after prep:
      tr_obj = x$ove.perc,
      # Has it been prepped?
      trained = x$trained,
      # An estimate of how many characters to print on a line:
      width = width
    )
  }

######################################################################
######################################################################
######################################################################

#' @rdname tidy.recipe
#' @param x A `step_SmoteClassif` object.
#' @export
tidy.step_SmoteClassif <- function(x, ...) {
  res <- tibble::tibble(
    Class = names(x$C.perc),
    Percentage = unlist(x$C.perc)
  )
  # Always return the step id:
  res$id <- x$id
  res
}

######################################################################
######################################################################
######################################################################

#' @rdname tune.recipe
#' @param x A `step_SmoteClassif` object.
#' @export
tunable.step_SmoteClassif <- function (x, ...) {
  tibble::tibble(
    name = c("ove.perc","und.perc","neighbors"),
    call_info = list(list(pkg = NULL, fun = "ove.perc"),
                     list(pkg = NULL, fun = "und.perc"),
                     list(pkg = "dials", fun = "neighbors")),
    source = "recipe",
    component = "step_SmoteClassif",
    component_id = x$id
  )
}

######################################################################
######################################################################
######################################################################
