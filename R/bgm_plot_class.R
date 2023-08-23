#' Plot Posterior Structure Probabilities
#'
#' @param output Output object from the easybgm function
#' @param as.BF if TRUE plots the y-axis as Bayes factor instead of posterior structure probability
#' @param ... Additional arguments passed onto `ggplot2`
#'
#' @export
#' @importFrom dplyr group_by summarise mutate group_modify filter
#'

plot_posteriorstructure <- function(output, as.BF = FALSE, ...) {
  if(!any(class(output) == "easybgm") | class(output) != "bgms"){
    stop("Wrong input provided. The function requires as input the output of the easybgm or bgm function.")
  }

  if(class(output)=="bgms" & (packageVersion("bgms") < "0.1.1")){
    stop("The fit of this version of bgms is not compatible with the plot. Please install the latest package version and refit the data.")
  }
  UseMethod("plot_posteriorstructure", output)

}

# ---------------------------------------------------------------------------------------------------------------

#' Plot posterior structure complexity
#'
#' @param output Output object from the easybgm function
#' @param ... Additional arguments passed onto `ggplot2`
#'
#' @export
#' @import ggplot2
#'

plot_posteriorcomplexity <- function(output, ...) {
  if(!any(class(output) == "easybgm") | class(output) != "bgms"){
    stop("Wrong input provided. The function requires as input the output of the easybgm or bgm function.")
  }

  if(class(output)=="bgms" & (packageVersion("bgms") < "0.1.1")){
    stop("The fit of this version of bgms is not compatible with the plot. Please install the latest package version and refit the data.")
  }
  UseMethod("plot_posteriorcomplexity", output)

}

# ---------------------------------------------------------------------------------------------------------------

#' Edge evidence plot
#'
#' @param output Output object from the easybgm function
#' @param evidence_thresh Bayes Factor which will be considered sufficient evidence for in-/exclusion, default is 10.
#' @param split if TRUE, plot is split in included and excluded edges
#' @param show specifies which edges should be shown, indicated by "all", "included", "inconclusive", "excluded"
#' @param ... Additional arguments passed onto `qgraph`
#'
#' @export
#'

plot_edgeevidence <- function(output, evidence_thresh = 10, split = F, show = "all", ...) {
  if(!any(class(output) == "easybgm") | class(output) != "bgms"){
    stop("Wrong input provided. The function requires as input the output of the easybgm or bgm function.")
  }

  if(class(output)=="bgms" & (packageVersion("bgms") < "0.1.1")){
    stop("The fit of this version of bgms is not compatible with the plot. Please install the latest package version and refit the data.")
  }
  UseMethod("plot_edgeevidence", output)

}

# ---------------------------------------------------------------------------------------------------------------

#' Network plot
#'
#' @param output Output object from the easybgm function
#' @param exc_prob threshold for excluding edges; all edges with a lower inclusion probability will not be shown
#' @param dashed binary parameter indicating whether edges with inconclusive evidence should be dashed
#' @param ... Additional arguments passed onto `qgraph`

#'
#' @export

plot_network <- function(output, exc_prob = .5, dashed = F, ...) {
  if(!any(class(output) == "easybgm") | class(output) != "bgms"){
    stop("Wrong input provided. The function requires as input the output of the easybgm or bgm function.")
  }

  if(class(output)=="bgms" & (packageVersion("bgms") < "0.1.1")){
    stop("The fit of this version of bgms is not compatible with the plot. Please install the latest package version and refit the data.")
  }
  UseMethod("plot_network", output)

}


# -------------------------------------------------

#' Structure plot
#'
#' @param output Output object from the easybgm function
#' @param ... Additional arguments passed onto `qgraph`
#'
#' @export
#'
#' @import qgraph
#'

plot_structure <- function(output, ...) {
  if(!any(class(output) == "easybgm") | class(output) != "bgms"){
    stop("Wrong input provided. The function requires as input the output of the easybgm or bgm function.")
  }

  if(class(output)=="bgms" & (packageVersion("bgms") < "0.1.1")){
    stop("The fit of this version of bgms is not compatible with the plot. Please install the latest package version and refit the data.")
  }
  UseMethod("plot_structure", output)

}



# ---------------------------------------------------------------------------------------------------------------

#' Plot of interaction parameters and their 95% highest density intervals
#'
#' @param output Output object from the easybgm function
#' @param ... Additional arguments passed onto `ggplot2`
#'
#' @export
#' @import ggplot2 HDInterval
#' @importFrom stats median
#'

plot_parameterHDI <- function(output, ...) {
  if(!any(class(output) == "easybgm") | class(output) != "bgms"){
    stop("Wrong input provided. The function requires as input the output of the easybgm or bgm function.")
  }

  if(class(output)=="bgms" & (packageVersion("bgms") < "0.1.1")){
    stop("The fit of this version of bgms is not compatible with the plot. Please install the latest package version and refit the data.")
  }
  UseMethod("plot_parameterHDI", output)

}

# ---------------------------------------------------------------------------------------------------------------
# Centrality plot

#' Plot centrality measures and 95% highest density interval
#'
#' @param output Output object from the easybgm function
#' @param ... Additional arguments passed onto `ggplot2`
#'
#' @importFrom dplyr arrange
#' @importFrom ggplot2 .data
#' @export
#'


plot_centrality <- function(output, ...){
  if(!any(class(output) == "easybgm") | class(output) != "bgms"){
    stop("Wrong input provided. The function requires as input the output of the easybgm or bgm function.")
  }

  if(class(output)=="bgms" & (packageVersion("bgms") < "0.1.1")){
    stop("The fit of this version of bgms is not compatible with the plot. Please install the latest package version and refit the data.")
  }
  UseMethod("plot_centrality", output)

}


