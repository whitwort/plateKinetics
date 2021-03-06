% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiment.R
\name{platformLabels}
\alias{platformLabels}
\title{Create well labels for a multiwell plate platform}
\usage{
platformLabels(nrow, ncol, rowLabels = c(LETTERS, paste(rep(LETTERS, each =
  length(LETTERS)), LETTERS, sep = "")))
}
\arguments{
\item{nrow}{Number of rows.}

\item{ncol}{Number of columns.}

\item{rowLabels}{A character vector containing  row labes. Defaults to the
most common convention of using A..Z first, followed by two letter series: 
AA..AZ, BA..BZ, etc.}
}
\value{
A character matrix with well labels.
}
\description{
Utility function that generates standard well labels for standard multiwell 
plate formats.
}

