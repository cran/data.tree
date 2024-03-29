% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node_methods_traversal.R
\name{Traverse}
\alias{Traverse}
\title{Traverse a tree or a sub-tree}
\usage{
Traverse(
  node,
  traversal = c("pre-order", "post-order", "in-order", "level", "ancestor"),
  pruneFun = NULL,
  filterFun = NULL
)
}
\arguments{
\item{node}{the root of a tree or a sub-tree that should be traversed}

\item{traversal}{any of 'pre-order' (the default), 'post-order', 'in-order', 'level', 'ancestor', or a custom function (see details)}

\item{pruneFun}{allows providing a prune criteria, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}. 
If the pruneFun returns FALSE for a Node, then the Node and its entire sub-tree will not be considered.}

\item{filterFun}{allows providing a a filter, i.e. a function taking a \code{Node} as an input, and returning \code{TRUE} or \code{FALSE}.
Note that if filter returns \code{FALSE}, then the node will be excluded from the result (but not the entire subtree).}
}
\value{
a list of \code{Node}s
}
\description{
Traverse takes the root of a tree or a sub-tree, and "walks" the tree in a specific order. It returns a list of
\code{\link{Node}} objects, filtered and pruned by \code{filterFun} and \code{pruneFun}.
}
\details{
The traversal order is as follows. (Note that these descriptions are not precise and complete. They are meant
for quick reference only. See the data.tree vignette for a more detailed description). 
\describe{
   \item{pre-order}{Go to first child, then to its first child, etc.}
   \item{post-order}{Go to the first branch's leaf, then to its siblings, and work your way back to the root}
   \item{in-order}{Go to the first branch's leaf, then to its parent, and only then to the leaf's sibling}
   \item{level}{Collect root, then level 2, then level 3, etc.}
   \item{ancestor}{Take a node, then the node's parent, then that node's parent in turn, etc. This ignores the \code{pruneFun} }
   \item{function}{You can also provide a function, whose sole parameter is a \code{\link{Node}} object. The
   function is expected to return the node's next node, a list of the node's next nodes, or NULL.}
}
}
\seealso{
\code{\link{Node}}

\code{\link{Get}}

\code{\link{Set}}

\code{\link{Do}}
}
