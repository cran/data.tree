% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node_methods.R
\name{FindNode}
\alias{FindNode}
\title{Find a node by name in the (sub-)tree}
\usage{
FindNode(node, name)
}
\arguments{
\item{node}{The root \code{Node} of the tree or sub-tree to search}

\item{name}{The name of the \code{Node} to be returned}
}
\value{
The first \code{Node} whose name matches, or \code{NULL} if no such \code{Node} is
found.
}
\description{
Scans the entire sub-tree spanned by \code{node} and returns the first \code{\link{Node}}
having the \code{name} specified. This is mainly useful for trees whose name is unique.
If \code{\link{AreNamesUnique}} is \code{FALSE}, i.e. if there is more than one \code{Node}
called \code{name} in the tree, then it is undefined which one will be returned.
Also note that this method is not particularly fast. See examples for a faster way to
index large trees, if you need to do multiple searches. See \code{\link{Traverse}} if
you need to find multiple \code{Nodes}.
}
\examples{
data(acme)
FindNode(acme, "Outsource")

#re-usable hashed index for multiple searches:
if(!AreNamesUnique(acme)) stop("Hashed index works for unique names only!")
trav <- Traverse(acme, "level")
names(trav) <- Get(trav, "name")
nameIndex <- as.environment(trav)
#you could also use hash from package hash instead!
#nameIndex <- hash(trav)
nameIndex$Outsource
nameIndex$IT


}
\seealso{
AreNamesUnique, Traverse
}
