% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/node_methods_traversal.R
\name{Traverse}
\alias{Traverse}
\title{Traverse a tree or a sub-tree}
\usage{
Traverse(node, traversal = c("pre-order", "post-order", "in-order", "level",
  "ancestor"), pruneFun = NULL, filterFun = NULL)
}
\arguments{
\item{node}{the root of a tree or a sub-tree that should be traversed}

\item{traversal}{any of 'pre-order' (the default), 'post-order', 'in-order', 'level', or 'ancestor'}

\item{pruneFun}{allows providing a a prune criteria, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}.
If the pruneFun returns FALSE for a Node, then the Node and all its sub-tree will not be considered.}

\item{filterFun}{allows providing a a filter, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}.
Note that if filter returns \code{FALSE}, then the node will be excluded from the result (but not the entire subtree).}
}
\value{
a list of \code{Node}s
}
\description{
Traverse a tree or a sub-tree
}
