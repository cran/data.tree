## ------------------------------------------------------------------------
library(data.tree)
acme <- Node$new("Acme Inc.")
  accounting <- acme$AddChild("Accounting")
    software <- accounting$AddChild("New Software")
    standards <- accounting$AddChild("New Accounting Standards")
  research <- acme$AddChild("Research")
    newProductLine <- research$AddChild("New Product Line")
    newLabs <- research$AddChild("New Labs")
  it <- acme$AddChild("IT")
    outsource <- it$AddChild("Outsource")
    agile <- it$AddChild("Go agile")
    goToR <- it$AddChild("Switch to R")

print(acme)

## ------------------------------------------------------------------------
acme$isRoot

## ------------------------------------------------------------------------
software$cost <- 1000000
standards$cost <- 500000
newProductLine$cost <- 2000000
newLabs$cost <- 750000
outsource$cost <- 400000
agile$cost <- 250000
goToR$cost <- 50000


## ------------------------------------------------------------------------
software$p <- 0.5
standards$p <- 0.75
newProductLine$p <- 0.25
newLabs$p <- 0.9
outsource$p <- 0.2
agile$p <- 0.05
goToR$p <- 1


## ------------------------------------------------------------------------
acmedf <- as.data.frame(acme)

## ----, eval=FALSE--------------------------------------------------------
#  acme$ToDataFrame()

## ------------------------------------------------------------------------
acmedf$level <- acme$Get("level")
acmedf$cost <- acme$Get("cost")

## ------------------------------------------------------------------------
acme$ToDataFrame("level", "cost")

## ----, eval=FALSE--------------------------------------------------------
#  print(acme, "level", "cost")

## ------------------------------------------------------------------------

acme$ToDataFrame("level",
                  probability = acme$Get("p", format = FormatPercent)
                )
                        


## ------------------------------------------------------------------------
acme$ToDataFrame("level")

## ------------------------------------------------------------------------
data.frame(level = acme$Get('level', traversal = "post-order"))

## ------------------------------------------------------------------------

data.frame(level = agile$Get('level', traversal = "ancestor"))


## ------------------------------------------------------------------------

ExpectedCost <- function(node) {
  result <- node$cost * node$p
  if(length(result) == 0) result <- NA
  return (result)
}

data.frame(acme$Get(ExpectedCost))


## ------------------------------------------------------------------------
library(magrittr)
ExpectedCost <- function(node) {
  result <- node$cost * node$p
  if(length(result) == 0) {
    if (node$isLeaf) result <- NA
    else {
      node$children %>% sapply(ExpectedCost) %>% sum -> result
    }
  }
  return (result)
}

data.frame(ec = acme$Get(ExpectedCost))


## ------------------------------------------------------------------------

ExpectedCost <- function(node, fun = sum) {
  result <- node$cost * node$p
  if(length(result) == 0) {
    if (node$isLeaf) result <- NA
    else {
      node$children %>% sapply(function(x) ExpectedCost(x, fun = fun)) %>% fun -> result
    }
  }
  return (result)
}

data.frame(ec = acme$Get(ExpectedCost, fun = mean))


## ------------------------------------------------------------------------

acme$Get(function(x) x$p * x$cost, assign = "expectedCost")
print(acme, "p", "cost", "expectedCost")


## ------------------------------------------------------------------------

ExpectedCost <- function(node, variableName = "avgExpectedCost", fun = sum) {
  #if the "cache" is filled, I return it. This stops the recursion
  if(!is.null(node[[variableName]])) return (node[[variableName]])
  
  #otherwise, I calculate from my own properties
  result <- node$cost * node$p
  
  #if the properties are not set, I calculate the mean from my children
  if(length(result) == 0) {
    if (node$isLeaf) result <- NA
    else {
      node$children %>%
      sapply(function(x) ExpectedCost(x, variableName = variableName, fun = fun)) %>%
      fun -> result
    }
  }
  return (result)
}


## ------------------------------------------------------------------------

invisible(
  acme$Get(ExpectedCost, fun = mean, traversal = "post-order", assign = "avgExpectedCost")
)
print(acme, "cost", "p", "avgExpectedCost")


## ------------------------------------------------------------------------

PrintMoney <- function(x) {
  format(x, digits=10, nsmall=2, decimal.mark=".", big.mark="'", scientific = FALSE)
}

print(acme, cost = acme$Get("cost", format = PrintMoney))


## ------------------------------------------------------------------------
acme$Get("cost", format = PrintMoney, assign = "cost2")
print(acme, cost = acme$Get("cost2"))

## ------------------------------------------------------------------------
employees <- c(NA, 52, NA, NA, 78, NA, NA, 39, NA, NA, NA)
acme$Set(employees)
print(acme, "employees")

## ------------------------------------------------------------------------
secretaries <- c(NA, 5, NA, NA, 6, NA, NA, 2, NA, NA, NA)
acme$Set(secretaries, secPerEmployee = secretaries/employees)
print(acme, "employees", "secretaries", "secPerEmployee")



## ------------------------------------------------------------------------
ec <- acme$Get(function(x) x$p * x$cost)
acme$Set(expectedCost = ec)
print(acme, "p", "cost", "expectedCost")


## ------------------------------------------------------------------------
acme$Set(avgExpectedCost = NULL)

## ------------------------------------------------------------------------
acme$newAttribute

## ------------------------------------------------------------------------
acme$Set(avgExpectedCost = NULL)$Set(expectedCost = NA)
print(acme, "avgExpectedCost", "expectedCost")

## ------------------------------------------------------------------------
acme$Set(avgExpectedCost =NULL, expectedCost = NA)

## ------------------------------------------------------------------------
acme$avgExpectedCost
acme$expectedCost

## ------------------------------------------------------------------------

acme$Aggregate("cost", sum)


## ------------------------------------------------------------------------
acme$Get("Aggregate", "cost", sum)

## ------------------------------------------------------------------------

GetCost <- function(node) {
  result <- node$cost
  if(length(result) == 0) {
    if (node$isLeaf) stop(paste("Cost for ", node$name, " not available!"))
    else {
      node$children %>% sapply(GetCost) %>% sum -> result
    }
  }
  return (result)
}

acme$Get(GetCost)


## ------------------------------------------------------------------------
acme$Get(ExpectedCost, assign = "expectedCost")
acme$Sort("expectedCost", decreasing = TRUE)
print(acme, "expectedCost")

## ------------------------------------------------------------------------
library(R6)
MyNode <- R6Class("MyNode",
                    inherit = Node,
                    lock = FALSE,
                    
                    #public fields and function
                    public = list(
                        
                        p = NULL, 
                        
                        cost = NULL,
                        
                        AddChild = function(name) {
                          child <- MyNode$new(name)
                          invisible (self$AddChildNode(child))
                        }
                        
                    ),
                    
                    #active
                    active = list(
                      
                      expectedCost = function() {
                        if ( is.null(self$p) || is.null(self$cost)) return (NULL)
                        self$p * self$cost                    
                      }
                      
                    )
                )


