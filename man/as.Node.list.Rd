% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node_conversion_list.R
\name{as.Node.list}
\alias{as.Node.list}
\alias{FromListExplicit}
\alias{FromListSimple}
\title{Convert a nested \code{list} structure to a \code{data.tree} structure}
\usage{
\method{as.Node}{list}(
  x,
  mode = c("simple", "explicit"),
  nameName = "name",
  childrenName = "children",
  nodeName = NULL,
  interpretNullAsList = FALSE,
  check = c("check", "no-warn", "no-check"),
  ...
)

FromListExplicit(
  explicitList,
  nameName = "name",
  childrenName = "children",
  nodeName = NULL,
  check = c("check", "no-warn", "no-check")
)

FromListSimple(
  simpleList,
  nameName = "name",
  nodeName = NULL,
  interpretNullAsList = FALSE,
  check = c("check", "no-warn", "no-check")
)
}
\arguments{
\item{x}{The \code{list} to be converted.}

\item{mode}{How the list is structured. "simple" (the default) will interpret any list to be a child. "explicit" 
assumes that children are in a nested list called \code{childrenName}}

\item{nameName}{The name of the element in the list that should be used as the name, can be NULL if mode = explicit and
the children lists are named, or if an automatic name (running number) should be assigned}

\item{childrenName}{The name of the element that contains the child list (applies to mode 'explicit' only).}

\item{nodeName}{A name suggestion for x, if the name cannot be deferred otherwise. This is for example the case for
the root with mode explicit and named lists.}

\item{interpretNullAsList}{If \code{TRUE}, then \code{NULL}-valued lists are interpreted as child nodes. Else, they are interpreted as attributes.
This has only an effect if \code{mode} is "simple".}

\item{check}{Either
\itemize{
 \item{\code{"check"}: if the name conformance should be checked and warnings should be printed in case of non-conformance (the default)}
 \item{\code{"no-warn"}: if the name conformance should be checked, but no warnings should be printed in case of non-conformance (if you expect non-conformance)}
 \item{\code{"no-check" or FALSE}: if the name conformance should not be checked; use this if performance is critical. However, in case of non-conformance, expect cryptic follow-up errors}
}}

\item{...}{Any other argument to be passed to generic sub implementations}

\item{explicitList}{A \code{list} in which children are in a separate nested list called \code{childrenName}.}

\item{simpleList}{A \code{list} in which children are stored as nested list alongside other attributes. Any list is
interpreted as a child \code{Node}}
}
\description{
Convert a nested \code{list} structure to a \code{data.tree} structure
}
\examples{
kingJosephs <- list(name = "Joseph I",
                    spouse = "Mary",
                    born = "1818-02-23",
                    died = "1839-08-29",
                    children = list(
                                    list(name = "Joseph II",
                                         spouse = "Kathryn",
                                         born = "1839-03-28",
                                         died = "1865-12-19"),
                                    list(name = "Helen",
                                         born = "1840-17-08",
                                         died = "1845-01-01")
                                    )
                   )
FromListExplicit(kingJosephs)

kingJosephs <- list(head = "Joseph I",
                    spouse = "Mary",
                    born = "1818-02-23",
                    died = "1839-08-29",
                    list(head = "Joseph II",
                         spouse = "Kathryn",
                         born = "1839-03-28",
                         died = "1865-12-19"),
                    list(head = "Helen",
                         born = "1840-17-08",
                         died = "1845-01-01")       
                   )
FromListSimple(kingJosephs, nameName = "head")

kingJosephs <- list(spouse = "Mary",
                    born = "1818-02-23",
                    died = "1839-08-29",
                    `Joseph II` = list(spouse = "Kathryn",
                                       born = "1839-03-28",
                                       died = "1865-12-19"),
                    Helen = list(born = "1840-17-08",
                                 died = "1845-01-01")
                                 
                   )
FromListSimple(kingJosephs, nodeName = "Joseph I")
  
}
\seealso{
Other as.Node: 
\code{\link{as.Node.data.frame}()},
\code{\link{as.Node.dendrogram}()},
\code{\link{as.Node.phylo}()},
\code{\link{as.Node.rpart}()},
\code{\link{as.Node}()}
}
\concept{as.Node}
