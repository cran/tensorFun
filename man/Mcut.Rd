% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Mcut.R
\name{Mcut}
\alias{Mcut}
\title{Matrix cutting}
\usage{
Mcut(A, B)
}
\arguments{
\item{A}{A matrix with n rows and l columns}

\item{B}{A matrix with k rows and l columns, with k no less than n}
}
\value{
A matrix with n rows and l columns
}
\description{
Cutting the bottom few rows of a matrix B so that B has the same number of rows as matrix A
}
\examples{
Mcut(matrix(1:4,nrow=2), matrix(1:6,nrow=3))

}
