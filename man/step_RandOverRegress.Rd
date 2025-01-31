% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RandOverRegress.R
\name{step_RandOverRegress}
\alias{step_RandOverRegress}
\title{Random Oversampling for Imbalanced Regression Tasks}
\usage{
step_RandOverRegress(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  ove.perc = 0.5,
  perc.list = NULL,
  C.perc = NULL,
  threshold = 0.9,
  target = NULL,
  skip = TRUE,
  id = rand_id("RandOverRegress")
)
}
\arguments{
\item{id}{}
}
\value{

}
\description{
Random Oversampling for Imbalanced Regression Tasks
}
\examples{
library(tidymodels)

load("~/Desktop/Academia/Datasets/RegressionDatasets_FULL.RData")
ds <- DSs[[48]]@data
form <- DSs[[48]]@formula

ds <- tibble(ds)

ove_rec <- recipe(shares ~ ., data = ds) \%>\%
  step_RandOverRegress(shares, ove.perc = 2) \%>\%
  prep()

tidy(ove_rec, number=1)

training <- ove_rec \%>\%
  bake(new_data = NULL)

plot(density(ds$shares))
lines(density(training$shares), col=34)

folds <- vfold_cv(ds, v = 5)

tune_rec <- recipe(shares ~ ., data = ds) \%>\%
  step_RandOverRegress(shares, ove.perc = tune())

lin_mod <-
  decision_tree() \%>\%
  set_mode("regression") \%>\%
  set_engine("rpart")

wf <- workflow() \%>\%
  add_recipe(tune_rec) \%>\%
  add_model(lin_mod)

lambda_grid <- grid_random(ove.perc(),size = 3)

res <- tune_grid(wf,
                 resamples = folds,
                 grid = lambda_grid,control = control_grid(save_pred=TRUE))

res <- add_sera(res, form)

show_best(res, metric="rmse")
show_best(res, metric="rsq")
show_best(res, metric="sera")

}
