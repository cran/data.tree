

#' Format a Number as a Percentage
#'
#' This utility method can be used as a format function when converting trees to a \code{data.frame}
#'
#' @param x A number
#' @param digits The number of digits to print
#' @param format The format to use
#' @param ... Any other argument passed to formatC
#' @return A string corresponding to x, suitable for printing
#'
#' @examples
#' data(acme)
#' print(acme, prob = acme$Get("p", format = FormatPercent))
#'
#' @seealso formatC
#' @export
FormatPercent <- function(x, digits = 2, format = "f", ...) {
  ifelse(is.null(x) || is.na(x), "", paste(formatC(100 * x, format = format, digits = digits, ...), "%"))
}

#' Format a Number as a Decimal
#'
#' Simple function that can be used as a format function when converting trees to a \code{data.frame}
#'
#' @param x a numeric scalar or vector
#' @param digits the number of digits to print after the decimal point
#' @return A string corresponding to x, suitable for printing
#'
#' @examples
#' data(acme)
#' print(acme, prob = acme$Get("p", format = function(x) FormatFixedDecimal(x, 4)))
#'
#' @export
FormatFixedDecimal <- function(x, digits = 3) {
  ifelse(is.null(x) || is.na(x), "", sprintf(paste0("%.",digits, "f"),x))
}





#' Calculates the height of a \code{Node} given the height of the root.
#'
#' This function puts leafs at the bottom (not hanging), and makes edges equally long.
#' Useful for easy plotting with third-party packages, e.g. if you have no specific height
#' attribute, e.g. with \code{\link{as.dendrogram.Node}}, \code{\link{ToNewick}},
#' and \code{\link{as.phylo.Node}}
#'
#' @param node The node
#' @param rootHeight The height of the root
#'
#' @examples
#' data(acme)
#' dacme <- as.dendrogram(acme, heightAttribute = function(x) DefaultPlotHeight(x, 200))
#' plot(dacme, center = TRUE)
#'
#' @export
DefaultPlotHeight <- function(node, rootHeight = 100) {
  if (node$isRoot) return ( rootHeight )
  if (node$isLeaf) return ( 0 )
  h <- DefaultPlotHeight(node$parent, rootHeight) * (1 - 1 / node$height)
  return (h)
}


SetHeight2 <- function(node, rootHeight = 100) {
  Set(node$leaves, height2 = 1)
  node$Do(function(x) x$height2 <- Aggregate(x, "height2", max) + 1, traversal = "post-order", filterFun = isNotLeaf)
  node$plotHeight <- rootHeight
  node$Do(function(x) x$plotHeight <- x$parent$plotHeight * (1 - 1 / x$height2), filterFun = isNotRoot)
}


#' Create a tree for demo and testing
#'
#' @param height the number of levels
#' @param branchingFactor the number of children per node
#' @param parent the parent node (for recursion)
#'
#' @export
CreateRegularTree <- function(height = 5, branchingFactor = 3, parent = Node$new("1")) {
  if (height <= 1) return()
  for (i in 1:branchingFactor) {
    child <- parent$AddChild(paste(parent$name, i, sep = "."), check = FALSE)
    CreateRegularTree(height - 1, branchingFactor, child)
  }
  return (parent)
}





#' Create a tree for demo and testing
#'
#' @param nodes The number of nodes to create
#' @param root the previous node (for recursion, typically use default value)
#' @param id The id (for recursion)
#'
#' @export
CreateRandomTree <- function(nodes = 100, root = Node$new("1"), id = 1) {
  if (nodes == 0) return()
  dpth <- root$height
  lvl <- sample(1:dpth, 1, rep(1/dpth))
  t <- Traverse(root, filterFun = function(x) x$level == lvl)
  parent <- sample(t, 1)[[1]]
  parent$AddChild(as.character(id + 1), check = FALSE)
  CreateRandomTree(nodes - 1, root = root, id = id + 1)
  return (root)
}



PrintPruneSimple <- function(x, limit) {
  tc <- x$totalCount
  toBeCropped <- tc - limit
  if (toBeCropped < 1) {
    if(!isRoot(x)) {
      #clone s.t. x is root (for pretty level names)
      x <- Clone(x, attributes = TRUE)
      x$parent <- NULL
    }
    return (x)
  }

  x$Set(.id = 1:tc)

  x$Do(function(x) {
       x$.originalTotalCount <- ifelse(x$isLeaf,
                                       1,
                                       sum( sapply(x$children, function(x) x$.originalTotalCount)) + 1)
       x$.originalCount <- x$count
       },
       traversal = "post-order"
  )

  xc <- Clone(x, pruneFun = function(x) x$.id < limit, attributes = TRUE)

  xc$Do(function(x) {
          if(x$count < x$.originalCount) {
            nds <- x$.originalCount - x$count
            sub <- x$.originalTotalCount - x$totalCount - nds
            x$AddChild(paste0("... ", nds, " nodes w/ ", sub, " sub"))
          }
        })

  x <- xc

}





PrintPruneDist <- function(x, limit) {
  tc <- x$totalCount
  toBeCropped <- tc - limit
  if (toBeCropped < 1) {
    if(!isRoot(x)) {
      #clone s.t. x is root (for pretty level names)
      x <- Clone(x, attributes = TRUE)
      x$parent <- NULL
    }
    return (x)
  }

  t <- Traverse(x, traversal = "post-order")

  Do(t, function(x) {
    x$.height <- ifelse(x$isLeaf, 1, x$children[[1]]$.height + 1)
  })

  Do(t, function(x) {
    x$.originalTotalCount <- ifelse(x$isLeaf, 1, sum( sapply(x$children, function(x) x$.originalTotalCount)) + 1)
  })


  t <- Traverse(x)
  Set(t, .id = 1:tc)
  x$.level <- 1
  Do(t, function(x) {
    x$.originalCount <- length(x$children)
    x$.level <- ifelse(isRoot(x), 1, x$parent$.level + 1)
  })



  t <- t[order(Get(t, ".level"),
               - Get(t, ".height"),
               Get(t, function(x) x$position > 2))]

  keep <- c(rep(TRUE, limit), rep(FALSE, toBeCropped))

  Set(t, .keep = keep)
  #sapply(t, function(x) paste(x$.height, x$.level, x$name, sep = "."))
  xc <- Clone(x, pruneFun = function(x) x$.keep, attributes = TRUE)

  t <- Traverse(xc)

  Do(t, function(x) {
    if(x$count < x$.originalCount) {
      nds <- x$.originalCount - x$count
      sub <- x$.originalTotalCount - x$totalCount - nds
      x$AddChild(paste0("... ", nds, " nodes w/ ", sub, " sub"))
    }
  })

  x <- xc

}



#' @rdname ToDiagrammeRGraph
#' @export
GetDefaultTooltip <- function(node) {


  if (length(node$attributes) > 0) {
    myattributes <- node$attributes
  } else {
    myattributes <- "name"
  }
  tt <- paste(sapply(myattributes, function(x) {
    v <- node[[x]]
    if (is.function(v)) v <- "function"
    else v <- GetAttribute(node, x)
    paste0("- ", x, ": ", v)
  }), collapse = "\n")

  return (tt)
}


#' Checks whether \code{name} is a reserved word, as defined in \code{NODE_RESERVED_NAMES_CONST}.
#' 
#' @param name the name to check
#' @param check Either
#' \itemize{
#'  \item{\code{"check"}: if the name conformance should be checked and warnings should be printed in case of non-conformance (the default)}
#'  \item{\code{"no-warn"}: if the name conformance should be checked, but no warnings should be printed in case of non-conformance (if you expect non-conformance)}
#'  \item{\code{"no-check" or FALSE}: if the name conformance should not be checked; use this if performance is critical. However, in case of non-conformance, expect cryptic follow-up errors}
#' }
CheckNameReservedWord <- function(name, check = c("check", "no-warn", "no-check")) {
  check <- check[1]
  if (check == FALSE) return (name)
  if (check == "no-check") return (name)
  if (!(check == FALSE || check == "no-check")) {
    if (name %in% NODE_RESERVED_NAMES_CONST) {
      
      name2 <- paste0(name, "2")
      if (check != "no-warn") {
        warning(paste0("Name '", name, "' is a reserved word as defined in NODE_RESERVED_NAMES_CONST. Using '", name2, "' instead."))
      }
      name <- name2
      
    }
  }
  return (name)
}
    
