% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node_conversion_party.R
\name{as.Node.BinaryTree}
\alias{as.Node.BinaryTree}
\title{Convert a a \code{SplitNode} from the party package to a \code{data.tree} structure.}
\usage{
\method{as.Node}{BinaryTree}(x, ...)
}
\arguments{
\item{x}{The BinaryTree}

\item{...}{additional arguments (unused)}
}
\description{
Convert a a \code{SplitNode} from the party package to a \code{data.tree} structure.
}
\examples{
library(party)
airq <- subset(airquality, !is.na(Ozone))
airct <- ctree(Ozone ~ ., data = airq, 
               controls = ctree_control(maxsurrogate = 3))
               
tree <- as.Node(airct)
tree

print(tree, 
      "label", 
      criterion = function(x) round(x$criterion$maxcriterion, 3),
      statistic = function(x) round(max(x$criterion$statistic), 3)
      )

FindNode(tree, 6)$path


}
