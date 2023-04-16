#' Latent Dirichlet Allocation (LDA)
#'
#' @description
#'
#' `latent_dirichlet_alloc()` defines a model that fits topics to a corpus using Latent Dirichlet
#' Allocation.
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is "partition".
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. Possible engines are listed below. The default for this
#'   model is `"topicmodels"`.
#' @param num_topics The number of topics to fit.
#' @param topic_density Prior on document-topic distribution.
#' @param word_density Prior on topic-word distribution.
#''
#' @return An `lda` topic model specification.
#'
#' @examples
#' # Show all engines
#' modelenv::get_from_env("lda")
#'
#' latent_dirichlet_alloc()
#' @export
latent_dirichlet_alloc <-
  function(mode = "partition",
           engine = "topicmodels",
           num_topics = NULL,
           topic_density = NULL,
           word_density = NULL) {
    args <- list(
      num_topics = enquo(num_topics),
      topic_density = enquo(topic_density),
      word_density = enquo(word_density)
    )

    new_cluster_spec(
      "latent_dirichlet_alloc",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.latent_dirichlet_alloc <- function(x, ...) {
  cat("LDA Topic Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

#' @export
translate_tidyclust.latent_dirichlet_alloc <- function(x, engine = x$engine, ...) {
  x <- translate_tidyclust.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' @method update lda
#' @rdname tidyclust_update
#' @export
update.latent_dirichlet_alloc <- function(object,
                       parameters = NULL,
                       num_topics = NULL,
                       topic_density = NULL,
                       word_density = NULL, ...) {
  eng_args <- parsnip::update_engine_parameters(
    object$eng_args, fresh = fresh, ...
  )

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }
  args <- list(
    num_topics = enquo(num_topics),
    topic_density = enquo(topic_density),
    word_density = enquo(word_density)
  )

  args <- parsnip::update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
    object$eng_args <- eng_args
  } else {
    null_args <- map_lgl(args, null_value)
    if (any(null_args)) {
      args <- args[!null_args]
    }
    if (length(args) > 0) {
      object$args[names(args)] <- args
    }
    if (length(eng_args) > 0) {
      object$eng_args[names(eng_args)] <- eng_args
    }
  }

  new_cluster_spec(
    "latent_dirichlet_alloc",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

# ------------------------------------------------------------------------------

check_args.latent_dirichlet_alloc <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$num_topics)) && any(args$num_topics < 0)) {
    rlang::abort("The number of topics should be >= 0.")
  }

  if (all(is.numeric(args$word_density)) && any(args$word_density <= 0)) {
    rlang::abort("The word density prior must be >0")
  }

  if (all(is.numeric(args$topic_density)) && any(args$topic_density <= 0)) {
    rlang::abort("The topic density prior must be >0")
  }

  invisible(object)
}

# ------------------------------------------------------------------------------
   
#' Simple Wrapper around ...
#' Bodwin wrote this part
#' Altering output w/ column names
#' leave blank for now (raw function)


































