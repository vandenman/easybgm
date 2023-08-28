#' @name summary.easybgm
#' @title  Summary method for \code{easybgm} objects
#'
#' @description Used to create a object of easybgm results and in turn print it
#'
#' @param object easybgm object
#' @param evidence_thresh Bayes Factor which will be considered sufficient evidence for in-/exclusion, default is 10.
#' @param ... unused argument
#'
#' @export

summary.easybgm <- function(object, evidence_thresh = 10, ...) {

  dots_check(...)

  # nodes
  p <- ncol(object$parameters)

  # names for each relation
  names <- colnames(object$parameters)
  names_bycol <- matrix(rep(names, each = p), ncol = p)
  names_byrow <- matrix(rep(names, each = p), ncol = p, byrow = T)
  names_comb <- matrix(paste0(names_byrow, "-", names_bycol), ncol = p)
  mat_names <- names_comb[upper.tri(names_comb)]

  # create data frame with parameter results
  if(object$model %in% c("dgm-binary")){
    inc_probs  <- round(object$inc_probs, 3)[upper.tri(object$inc_probs)]
    BF <- round(object$BF, 3)[upper.tri(object$BF)]
    #create the category of the edge (i.e., included, excluded, inconclusive)
    category <- character(length(BF))
    category[(BF < evidence_thresh) & (BF > 1/evidence_thresh)] <- "inconclusive"
    category[BF > evidence_thresh] <- "included"
    category[BF < 1/evidence_thresh] <- "excluded"

    results <-
      data.frame(
        relation = mat_names,
        inc_probs =  inc_probs,
        BF = BF,

        category = category,
        row.names = NULL
      )
    colnames(results) <- c(
      "Relation",
      "Posterior Incl. Prob.",
      "Inclusion BF",
      "Category")
  } else {
    parameter_values <- round(object$parameters, 3)[upper.tri(object$parameters)]
    inc_probs  <- round(object$inc_probs, 3)[upper.tri(object$inc_probs)]
    BF <- round(object$BF, 3)[upper.tri(object$BF)]
    #create the category of the edge (i.e., included, excluded, inconclusive)
    category <- character(length(BF))
    category[(BF < evidence_thresh) & (BF > 1/evidence_thresh)] <- "inconclusive"
    category[BF > evidence_thresh] <- "included"
    category[BF < 1/evidence_thresh] <- "excluded"


    results <-
      data.frame(
        relation = mat_names,
        parameter_values = parameter_values,
        inc_probs =  inc_probs,
        BF = BF,
        category = category
      )
    colnames(results) <- c(
      "Relation",
      "Estimate",
      "Posterior Incl. Prob.",
      "Inclusion BF",
      "Category")
  }
  # create list with output
  out <- list()
  out$parameters <- results
  out$package <- strsplit(class(object)[1], "_")[[1]][2]
  out$model <- object$model
  out$n_nodes <- p
  out$n_possible_edges <- p*(p-1)/2
  out$n_inclu_edges <- sum(BF > evidence_thresh)
  out$n_incon_edges <- sum((BF < evidence_thresh) & (BF > 1/evidence_thresh))
  out$n_exclu_edges <- sum(BF < 1/evidence_thresh)

  # structure information
  if(all(class(object) != "package_bggm")){
    out$possible_struc <- 2^(p*(p-1)/2)
    out$n_structures <- length(object$sample_graph)
    out$max_structure_prob <- max(object$structure_probabilities)
  }

  # Save command calls
  out$fit_object <- object
  out$evidence_thresh <- evidence_thresh

  # return object
  class(out) <- class(object)
  return(out)
  print(out)
}

#' @name print.easybgm
#' @title  Print method for \code{easybgm} objects
#'
#' @description Used to print easybgm results. The nicest overview is created by first feeding it to
#' `summary()`
#'
#' @param x easybgm object
#' @param ... unused argument
#'
#' @export
#'

print.easybgm <- function(x, ...){

  dots_check(...)

  if(is.null(x$n_inclu_edges)){
    #NextMethod("print")
    print(summary.easybgm(x))
  } else if(any(class(x) == "package_bggm")){
    cat("\n BAYESIAN ANALYSIS OF NETWORKS",
        "\n Model type:", x$model,
        "\n Number of nodes:", x$n_nodes,
        "\n Fitting Package:", x$package,
        "\n---",
        "\n EDGE SPECIFIC OVERVIEW",
        "\n")
    print(x$parameters, quote = FALSE, right = TRUE, row.names=F)
    cat("\n Bayes Factors larger than", x$evidence_thresh, "were considered sufficient evidence for the categorization.",
        "\n ---",
        "\n AGGREGATED EDGE OVERVIEW",
        "\n Number of included edges:", x$n_inclu_edges,
        "\n Number of inconclusive edges:", x$n_incon_edges,
        "\n Number of excluded edges:", x$n_exclu_edges,
        "\n Number of possible edges:", x$n_possible_edges,
        "\n")
  } else {
    cat("\n BAYESIAN ANALYSIS OF NETWORKS",
        "\n Model type:", x$model,
        "\n Number of nodes:", x$n_nodes,
        "\n Fitting Package:", x$package,
        "\n---",
        "\n EDGE SPECIFIC OVERVIEW",
        "\n")
    print(x$parameters, quote = FALSE, right = TRUE, row.names=F)
    cat("\n Bayes Factors larger than", x$evidence_thresh, "were considered sufficient evidence for the categorization.",
        "\n ---",
        "\n AGGREGATED EDGE OVERVIEW",
        "\n Number of included edges:", x$n_inclu_edges,
        "\n Number of inconclusive edges:", x$n_incon_edges,
        "\n Number of excluded edges:", x$n_exclu_edges,
        "\n Number of possible edges:", x$n_possible_edges,
        "\n",
        "\n ---",
        "\n STRUCTURE OVERVIEW",
        "\n Number of visited structures:", x$n_structures,
        "\n Number of possible structures:", x$possible_struc,
        "\n Posterior probability of most likely structure:", x$max_structure_prob,
        "\n---")
  }
}

