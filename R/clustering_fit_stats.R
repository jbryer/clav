#' Description of fit statistics for cluster analysis
#'
#' This data frame contains descriptions of the fit statistics used by the [optimal_clusters()]
#' function. It contains the following columns:
#'
#' \describe{
#'   \item{Fit_Statistic}{The name of the fit statistic.}
#'   \item{Reference}{Reference to first publication.}
#'   \item{Description}{A brief description of the fit statistic.}
#'   \item{Desired_Outcome}{How to interpret the statistic (i.e. what is the desired outcome).}
#'   \item{R_Function}{The R function used to get the statistic. Note that `clav` wraps most of these functions.}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name clustering_fit_stats
#' @usage data(clustering_fit_stats)
#' @format A data frame with 6 rows and 5 variables

"clustering_fit_stats"
