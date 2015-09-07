% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/node_methods_traversal.R
\name{Do}
\alias{Do}
\title{Executes a function an a set of nodes}
\usage{
Do(nodes, fun, ...)
}
\arguments{
\item{nodes}{a set of nodes, usually obtained via \code{\link{Traverse}}}

\item{fun}{the function to execute. The function is expected to be either a Method, or to take a
Node as its first argument}

\item{...}{any additional parameters to be passed on to fun}
}
\description{
Executes a function an a set of nodes
}
