library(recipes)
library(modeldata)

load("~/Desktop/Academia/Datasets/RegressionDatasets_FULL.RData")
ds <- DSs[[48]]@data
form <- DSs[[48]]@formula

ds <- tibble(ds)

up_rec <- recipe(shares ~ ., data = ds) %>%
  step_RandUnderRegress(shares, und.perc = 0.5) %>%
  prep()

#tidy(up_rec, number=1)

training <- up_rec %>%
  bake(new_data = NULL)

plot(density(ds$shares))
lines(density(training$shares), col=34)

folds <- vfold_cv(ds, v = 5)

tune_rec <- recipe(shares ~ ., data = ds) %>%
  step_RandUnderRegress(shares, und.perc = tune())

lin_mod <-
  decision_tree() %>%
  set_mode("regression") %>%
  set_engine("rpart")

wf <- workflow() %>%
  add_recipe(tune_rec) %>%
  add_model(lin_mod)

lambda_grid <- grid_random(und.perc(),size = 3)

# ph.aux <- IRon::phi.control(ds$shares)
# seraphi <- metric_tweak("seraphi", sera, ph=ph.aux)
# aux_metrics <- metric_set(rmse, rsq, sera)

res <- tune_grid(wf,
                 resamples = folds,
                 grid = lambda_grid,control = control_grid(save_pred=TRUE))
                 # metrics = aux_metrics)

res <- add_sera(res, form)

show_best(res, metric="rmse")
show_best(res, metric="rsq")
show_best(res, metric="sera")

# estimates <- collect_metrics(res)
# estimates

# show_best(res, metric = "rmse")
# show_best(res, metric = "rsq")
# show_best(res, metric = "sera")


step_RandUnderRegress <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  und.perc = 0.5,
  perc.list = NULL,
  C.perc = NULL,
  threshold = 0.9,
  target = NULL,
  skip = TRUE,
  id = rand_id("RandUnderRegress")
) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_RandUnderRegress_new(
      terms = terms,
      trained = trained,
      role = role,
      und.perc = und.perc,
      perc.list = perc.list,
      C.perc = C.perc,
      threshold = threshold,
      target = target,
      skip = skip,
      id = id
    )
  )
}

######################################################################
######################################################################
######################################################################

step_RandUnderRegress_new <-
  function(terms, role, trained, und.perc, perc.list, C.perc, threshold, target, skip, id) {
    step(
      subclass = "RandUnderRegress",
      terms = terms,
      role = role,
      trained = trained,
      und.perc = und.perc,
      perc.list = perc.list,
      C.perc = C.perc,
      threshold = threshold,
      target = target,
      skip = skip,
      id = id
    )
  }

######################################################################
######################################################################
######################################################################

prep.step_RandUnderRegress <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  #print(col_names)

  tgt.info <- info %>% filter(role=="outcome")

  if(tgt.info %>% count() == 0) { rlang::abort("A target variable is required!") }
  if(tgt.info %>% count() > 1) { rlang::abort("Multi-label is not supported at the moment!") }
  if(tgt.info$type != "numeric") { rlang::abort("Expecting a numeric target variable!") }
  if(!(all_of(col_names) %in% tgt.info$variable)) { rlang::abort("Selected variable is not a target variable!") }

  # check perc/perc.list (TO DO)

  # else, assume that the perc is for the most under-represented class

  # cls <- training %>% select(col_names) %>% table() %>% sort(decreasing=TRUE) %>% names() %>% first()
  C.perc <- list(x$und.perc)
  # C.perc[[cls]] <- x$und.perc
  # print(C.perc)

  step_RandUnderRegress_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    und.perc = x$und.perc,
    perc.list = x$perc.list,
    C.perc = C.perc,
    threshold = x$threshold,
    target = col_names,
    skip = x$skip,
    id = x$id
  )

}

######################################################################
######################################################################
######################################################################

bake.step_RandUnderRegress <- function(object, new_data, ...) {

  # UBL does not support tibbles yet
  new_data <- UBL::RandUnderRegress(form = formula(paste0(object$target,"~ .")),
                                    dat = as.data.frame(new_data),
                                    thr.rel = object$threshold,
                                    C.perc = object$C.perc)

  tibble::tibble(new_data)

}

######################################################################
######################################################################
######################################################################

print.step_RandUnderRegress <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Random Undersampling on outcome variable ",x$target,": ",sep="")
    printer(
      # Names before prep (could be selectors)
      untr_obj = x$terms,
      # Names after prep:
      tr_obj = x$und.perc,
      # Has it been prepped?
      trained = x$trained,
      # An estimate of how many characters to print on a line:
      width = width
    )
  }

######################################################################
######################################################################
######################################################################

tidy.step_RandUnderRegress <- function(x, ...) {
  res <- tibble::tibble(
    Threshold = x$threshold,
    Percentage = unlist(x$C.perc)
  )
  # Always return the step id:
  res$id <- x$id
  res
}

######################################################################
######################################################################
######################################################################

tunable.step_RandUnderRegress <- function (x, ...) {
  tibble::tibble(
    name = c("und.perc"),
    call_info = list(list(pkg = NULL, fun = "und.perc")),
    source = "recipe",
    component = "step_RandUnderRegress",
    component_id = x$id
  )
}

######################################################################
######################################################################
######################################################################

# required_pkgs.step_RandUnderRegress <- function(x, ...) {
# c("RandUnderRegress", "IDL")
# }
