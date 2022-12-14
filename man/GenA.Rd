% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/primitives.R
\name{GenA}
\alias{GenA}
\title{Generate a}
\usage{
GenA(n, q)
}
\arguments{
\item{n}{the order}

\item{q}{the ciphermod of coefficients}
}
\value{
polynomial of order x^^n with coefficients 0,..,q
}
\description{
Generate a
}
\examples{
n = 16
q =  7
GenA(n, q)
}
