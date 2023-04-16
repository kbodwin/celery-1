# nocov start

make_latent_dirichlet_alloc <- function() {
	modelenv::set_new_model("latent_dirichlet_alloc")

	modelenv::set_model_mode("latent_dirichlet_alloc", "partition")

	# ----------------------------------------------------------------------------

	modelenv::set_model_engine("latent_dirichlet_alloc", "partition", "topicmodels")
	modelenv::set_dependency(
		model = "latent_dirichlet_alloc",
		mode = "partition",
		eng = "topicmodels",
		pkg = "topicmodels"
	)

	modelenv::set_fit(
		model = "latent_dirichlet_alloc",
		eng = "topicmodels",
		mode = "partition",
		value = list(
			interface = "matrix",
			protect = c("x", "centers"),
			func = c(pkg = "topicmodels", fun = "LDA"),
			defaults = list()
		)
	)

	modelenv::set_encoding(
		model = "latent_dirichlet_alloc",
		eng = "topicmodels",
		mode = "partition",
		options = list(
			# ..........
		)
	)

  modelenv::set_model_arg(
    model = "latent_dirichlet_alloc",
    eng = "topicmodels",
    exposed = "num_topics",
    original = "k",
    func = list(pkg = "dials", fun = "num_topic"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "latent_dirichlet_alloc",
    eng = "topicmodels",
    exposed = "topic_density",
    original = "", #... control alpha beta https://cran.r-project.org/web/packages/topicmodels/topicmodels.pdf page 12
    func = list(pkg = "dials", fun = "topic_density"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "latent_dirichlet_alloc",
    eng = "topicmodels",
    exposed = "word_density",
    original = "", #... control alpha beta https://cran.r-project.org/web/packages/topicmodels/topicmodels.pdf page 12
    func = list(pkg = "dials", fun = "word_density"),
    has_submodel = TRUE
  )

  modelenv::set_pred(
    model = "latent_dirichlet_alloc",
    eng = "topicmodels",
    mode = "partition",
    type = "cluster",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "topicmodels_LDA_predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          new_data = rlang::expr(new_data)
        )
    )
  )
}

# nocov end
