% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{davies_bouldin}
\alias{davies_bouldin}
\title{Calculate Davies-Bouldin score}
\usage{
davies_bouldin(df, k = 9, ...)
}
\arguments{
\item{df}{data frame to calculate the Davies-Bouldin from.}

\item{k}{number of clusters.}

\item{...}{other parameters passed to \code{\link[clusterSim:index.DB]{clusterSim::index.DB()}}}
}
\description{
Calculate Davies-Bouldin score
}
