library(topicmodels)
library(tibble)
library(magrittr)
library(methods)
library(tm)

#' latent_dirichlet_alloc class
#'
#' A wrapper class for the topicmodels LDA object.
#'
#' @keywords internal
#' @export
setClass("latent_dirichlet_alloc",
         slots = list(
           control = "list",
           num_topics = "numeric",
           model = "LDA"
         ))

#' Constructor for latent_dirichlet_alloc class
#'
#' @param num_topics A numeric value specifying the number of topics.
#' @param control A list specifying the control parameters for LDA.
#' @return An instance of latent_dirichlet_alloc class.
#' @export
latent_dirichlet_alloc <- function(num_topics, control = list(alpha = 0.5, burnin = 1000, iter=1000, keep = 50, seed = 44)) {
  obj <- new("latent_dirichlet_alloc")
  obj@num_topics <- num_topics
  obj@control <- control
  return(obj)
}

#' Fitting function for latent_dirichlet_alloc class
#' 
#' @param object An instance of the latent_dirichlet_alloc class.
#' @param data Data for fitting the LDA model.
#' @return An instance of a fitted latent_dirichlet_alloc class.
#' @export
fit.latent_dirichlet_alloc <- function(object, data) {
  object@model <- LDA(data, method = "Gibbs", control = object@control, k = object@num_topics)
  return(object)
}

#' Summary function for latent_dirichlet_alloc class
#' 
#' @param object A fitted instance of the latent_dirichlet_alloc class.
#' @param num_terms Number of terms to show per topic
#' @return Summary information about the LDA model.
#' @export
extract_fit_summary.latent_dirichlet_alloc <- function(object, num_terms = 5) {
  
  gamma <- object@model@gamma
  gamma_probabilities <- tibble(id = 1:nrow(gamma))
  for (i in 1:ncol(gamma)) {
    col_name <- paste0("Topic ", i)
    gamma_probabilities[[col_name]] <- gamma[,i]
  }
  
  tt <- terms(object@model, num_terms)
  topic_terms <- tibble(id = 1:nrow(tt))
  for (i in 1:ncol(tt)) {
    col_name <- paste0("Topic ", i)
    topic_terms[[col_name]] <- tt[,i]
  }
  
  summary <- list(
    gamma_probabilities = gamma_probabilities,
    topic_terms = tt
  )
  
  return(summary)
}

#' Predict method for latent_dirichlet_alloc objects
#'
#' Performs prediction based on the type.
#'
#' @param object An object of class latent_dirichlet_alloc
#' @param type Changes representation of results.
#' @param newdata Data to be clustered into topics.
#' @return The predicted result.
#' @export
predict.latent_dirichlet_alloc <- function(object, type, newdata, ...) {
  results <- posterior(object@model, newdata)
  if (type == "cluster") { return(predict_cluster(results)) }
  else if (type == "prob") { return(predict_prob(results)) }
  else if (type == "raw") { return(predict_raw(results)) }
  else { return("Invalid 'type' argument") }
}

#' Helper function
predict_cluster <- function(results) {
  clusters <- tibble(id = numeric(), cluster = numeric())
  for (i in 1:nrow(results$topics)) {
    clusters <- rbind(clusters, tibble(id = i, cluster = which.max(results$topics[i,])))
    print(i)
  }
  return(clusters)
}

#' Helper function
predict_prob <- function(results) {
  probabilities <- tibble(id = 1:nrow(results$topics))
  for (i in 1:ncol(results$topics)) {
    col_name <- paste0("prob_", i)
    probabilities[[col_name]] <- results$topics[,i]
  }
  return(probabilities)
}

#' Helper function
predict_raw <- function(results) {
  return("raw")
}


#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------

# Testing
# data("AssociatedPress", package = "topicmodels")
# 
# lda_spec <- latent_dirichlet_alloc(num_topics=3, control=list(alpha = .5, seed=44))
# 
# lda_fit <- lda_spec %>%
#   fit(data = AssociatedPress[1:20, ])
# 
# res <- predict(lda_fit, type="cluster", newdata=AssociatedPress[21:30,])
# print(res)
# 
# res <- predict(lda_fit, type="prob", newdata=AssociatedPress[21:30,])
# print(res)
# 
# res <- predict(lda_fit, type="raw", newdata=AssociatedPress[21:30,])
# print(res)
# 
# lda_fit %>%
#   summary()
# 
# fit_summary <- lda_fit %>%
#   extract_fit_summary()
# print(fit_summary)

#' Functions todo:
#' summary()
#' extract_fit_summary()
#'
#' Within summary object -> $orig_labels and $cluster_assignments\
#' 
#' topics()
#' terms()
#' 

#' Other todo
#' Show workflows
#' Change other paramters like alpha, beta...
#' A brief intro to latent dirichlet allocation


