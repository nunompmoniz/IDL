library(recipes)
library(modeldata)
data(hpc_data)

hpc_data0 <- hpc_data %>%
  select(-protocol, -day)

up_rec <- recipe(class ~ ., data = hpc_data0) %>%
  step_RandUnderClassif(class, und.perc = 0.5) %>%
  prep()

#tidy(up_rec, number=1)

training <- up_rec %>%
    bake(new_data = NULL)

table(hpc_data0$class)
table(training$class)

folds <- vfold_cv(hpc_data0, v = 5)

tune_rec <- recipe(class ~ ., data = hpc_data0) %>%
  step_RandUnderClassif(class, und.perc = tune())

lin_mod <-
  decision_tree() %>%
  set_mode("classification") %>%
  set_engine("rpart")

wf <- workflow() %>%
  add_recipe(tune_rec) %>%
  add_model(lin_mod)

lambda_grid <- grid_random(und.perc(),size = 3)

res <- tune_grid(wf,
         resamples = folds,
         grid = lambda_grid,
         metrics = eval_metrics_classification)

estimates <- collect_metrics(res)
estimates

show_best(res, metric = "accuracy")
show_best(res, metric = "bal_accuracy")
show_best(res, metric = "roc_auc")
show_best(res, metric = "f_meas")

step_RandUnderClassif <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  und.perc = 0.5,
  perc.list = NULL,
  C.perc = NULL,
  target = NULL,
  skip = TRUE,
  id = rand_id("RandUnderClassif")
) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_RandUnderClassif_new(
      terms = terms,
      trained = trained,
      role = role,
      und.perc = und.perc,
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

step_RandUnderClassif_new <-
  function(terms, role, trained, und.perc, perc.list, C.perc, target, skip, id) {
    step(
      subclass = "RandUnderClassif",
      terms = terms,
      role = role,
      trained = trained,
      und.perc = und.perc,
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

prep.step_RandUnderClassif <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  #print(col_names)

  tgt.info <- info %>% filter(role=="outcome")

  if(tgt.info %>% count() == 0) { rlang::abort("A target variable is required!") }
  if(tgt.info %>% count() > 1) { rlang::abort("Multi-label is not supported at the moment!") }
  if(tgt.info$type != "nominal") { rlang::abort("Expecting a nominal target variable!") }
  if(!(all_of(col_names) %in% tgt.info$variable)) { rlang::abort("Selected variable is not a target variable!") }

  # check perc/perc.list (TO DO)

  # else, assume that the perc is for the most under-represented class

  cls <- training %>% select(col_names) %>% table() %>% sort(decreasing=TRUE) %>% names() %>% first()
  C.perc <- NULL
  C.perc[[cls]] <- x$und.perc
  # print(C.perc)

  step_RandUnderClassif_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    und.perc = x$und.perc,
    perc.list = x$perc.list,
    C.perc = C.perc,
    target = col_names,
    skip = x$skip,
    id = x$id
  )

}

######################################################################
######################################################################
######################################################################

bake.step_RandUnderClassif <- function(object, new_data, ...) {

  # UBL does not support tibbles yet
  new_data <- UBL::RandUnderClassif(form = formula(paste0(object$target,"~ .")), dat = as.data.frame(new_data), C.perc = object$C.perc)

  tibble::tibble(new_data)

}

######################################################################
######################################################################
######################################################################

print.step_RandUnderClassif <-
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

tidy.step_RandUnderClassif <- function(x, ...) {
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

tunable.step_RandUnderClassif <- function (x, ...) {
  tibble::tibble(
    name = c("und.perc"),
    call_info = list(list(pkg = NULL, fun = "und.perc")),
    source = "recipe",
    component = "step_RandUnderClassif",
    component_id = x$id
  )
}

######################################################################
######################################################################
######################################################################

# required_pkgs.step_RandUnderClassif <- function(x, ...) {
  # c("RandUnderClassif", "IDL")
# }
