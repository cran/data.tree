context("tree conversion")

data(acme)

test_that("as.list.Node explicit", {
  data(acme)
  
  l <- as.list(acme, mode = "explicit")
    
  expect_equal("list", class(l))
  expect_equal(2, length(l))
  expect_equal(c('name', 'children'), names(l))
  expect_equal(c('children'), names(l$children$Research))
  expect_equal(0.9, l$children$Research$children$`New Labs`$p)
 
})


test_that("as.list.Node explicit nameName=name", {
  data(acme)
  
  l <- as.list(acme, mode = "explicit", nameName = 'name')
  
  expect_equal(class(l), "list")
  expect_equal(length(l), 2)
  expect_equal(names(l), c('name', 'children'))
  expect_equal(names(l$children$Research), c('name', 'children'))
  expect_equal(l$children$Research$children$`New Labs`$p, 0.9)
  
})

test_that("as.list.Node explicit nameName=id", {
  data(acme)
  
  l <- as.list(acme, mode = "explicit", nameName = 'id')
  
  expect_equal(class(l), "list")
  expect_equal(length(l), 2)
  expect_equal(names(l), c('id', 'children'))
  expect_equal(names(l$children$Research), c('id', 'children'))
  expect_equal(l$children$Research$children$`New Labs`$p, 0.9)
  
})


test_that("as.list.Node simple", {
  
  data(acme)
  l <- as.list(acme)
  
  expect_equal("list", class(l))
  expect_equal(length(l), 4)
  expect_equal(names(l), c("name", "Accounting", "Research", "IT"))
  expect_equal(names(l$Research), c("New Product Line", "New Labs" ))
  expect_equal(0.9, l$Research$`New Labs`$p)
  
})

test_that("as.list.Node simple unname no effect", {
  
  data(acme)
  l <- as.list(acme)
  
  expect_equal("list", class(l))
  expect_equal(length(l), 4)
  expect_equal(names(l), c("name", "Accounting", "Research", "IT"))
  expect_equal(names(l$Research), c("New Product Line", "New Labs" ))
  expect_equal(0.9, l$Research$`New Labs`$p)
  
})


test_that("as.list.Node simple nameName=name", {
  
  data(acme)
  l <- as.list(acme, nameName = 'name')
  
  expect_equal("list", class(l))
  expect_equal(length(l), 4)
  expect_equal(names(l), c('name', "Accounting", "Research", "IT"))
  expect_equal(names(l$Research), c("name", "New Product Line", "New Labs" ))
  expect_equal(0.9, l$Research$`New Labs`$p)
  
})

test_that("as.list.Node simple keepOnly=p", {
  
  data(acme)
  l <- as.list(acme, keepOnly = 'p')
  
  expect_equal("list", class(l))
  expect_equal(length(l), 4)
  expect_equal(names(l), c('name', "Accounting", "Research", "IT"))
  expect_equal(names(l$Research), c("New Product Line", "New Labs" ))
  expect_equal(0.9, l$Research$`New Labs`$p)
  expect_null(l$Research$`New Labs`$cost)
  
})

test_that("as.list.Node explicit nameName=id", {
  
  
  l <- as.list(acme, nameName = 'id')
  
  expect_equal("list", class(l))
  expect_equal(length(l), 4)
  expect_equal(names(l), c('id', "Accounting", "Research", "IT"))
  expect_equal(names(l$Research), c("id", "New Product Line", "New Labs" ))
  expect_equal(0.9, l$Research$`New Labs`$p)
  
})


test_that("as.Node.list", {
  data(acme)
  n <- as.Node(as.list(acme))
  
  expect_equal("Acme Inc.", n$name)
  expect_equal(3, n$count)
  expect_equal(11, n$totalCount)
  expect_equal(0.05, n$Climb("IT", "Go agile")$p)
  
})


test_that("as.list.Node unname", {
  data(acme)
  l <- as.list(acme, mode = "explicit", unname = TRUE, nameName = 'id', childrenName = 'sub')
  
  expect_equal("list", class(l))
  expect_equal(2, length(l))
  expect_equal(c('id', 'sub'), names(l))
  expect_equal(0.9, l$sub[[2]]$sub[[2]]$p)
  expect_equal('New Product Line', l$sub[[2]]$sub[[1]]$id)
  
})

test_that("as.Node.list unname with mode = simple", {
  
  l <- as.list(acme, unname = TRUE, nameName = 'id', childrenName = 'sub')
  n <- as.Node(l, nameName = 'id', childrenName = 'sub')
  
  expect_equal("Acme Inc.", n$name)
  expect_equal(3, n$count)
  expect_equal(11, n$totalCount)
  expect_equal(0.05, n$Climb("IT", "Go agile")$p)
  
})


test_that("as.Node.list auto", {
  
  lol <- list(type = "Root", list(type = "Rule", value = 1), list(type = "Rule", value = 2))
  tree <- FromListSimple(lol, nameName = NULL, nodeName = 1)
  
  expect_equal(tree$totalCount, 3)
  expect_equal(unname(tree$Get("name")), as.character(c(1, 1, 2)))
  expect_equal(tree$children[[1]]$type, "Rule")
  
})


test_that("as.Node.list warning", {
  
  lol <- list(type = "Root", list(type = "Rule", count = 1), list(type = "Rule", count = 2))
  #tree <- FromListSimple(lol, nameName = NULL, nodeName = 1)
  tree <- NULL
  expect_warning(FromListSimple(lol, nameName = NULL, nodeName = 1, check = "no-warn"), NA)
  expect_that(tree <- FromListSimple(lol, nameName = NULL, nodeName = 1), gives_warning())
  
  expect_equal(tree$totalCount, 3)
  expect_equal(unname(tree$Get("name")), as.character(c(1, 1, 2)))
  
  expect_equal(unname(tree$Get("count")), c(2,0,0))
  expect_equal(unname(tree$Get("count2")), c(NA, 1, 2))
  
  expect_warning(FromListSimple(lol, nameName = NULL, nodeName = 1, check = "no-check"), NA)
  expect_that(tree <- FromListSimple(lol, nameName = NULL, nodeName = 1), gives_warning())
  
  
})



test_that("as.Node.list string", {
  
  
  
  
  yaml <- "
children:
  CR:
    description: Currencies
    type: Currency
    children:
      CR_CHF: market
      CR_EUR: market
      CR_USD: market
"
  
  lol <- yaml::yaml.load(yaml)
  
  tree <- FromListExplicit(lol)
  expect_equal(tree$totalCount, 5)
  expect_equal(tree$height, 3)
  expect_equal(tree$CR$CR_CHF$name, "CR_CHF")
  
  #market is numbered (1) instead of lost:
  expect_equal(tree$attributesAll, c("description", "type", "1"))
  
  
  
})



test_that("as.Node.list string 2", {
  

  yaml <- "
children:
  CR:
    description: Currencies
    type: Currency
    children:
      - CR_CHF
      - CR_EUR
      - CR_USD
"
  
  lol <- yaml::yaml.load(yaml)
  
  tree <- FromListExplicit(lol)
  expect_equal(tree$totalCount, 5)
  expect_equal(tree$height, 3)
  expect_equal(tree$CR$CR_CHF$name, "CR_CHF")
  
  
  
})


test_that("as.Node.list string NULL", {
  
  
  
  
  yaml <- "
name: OS Students 2014/15
OS X:
  Yosemite:
  Leopard:
Linux:
  Debian:
    version: 9
  Ubuntu:
Windows:
  W7:
  W8:
  W10:
"
  
  lol <- yaml::yaml.load(yaml)
  
  tree <- FromListSimple(lol, interpretNullAsList = TRUE)
  expect_equal(tree$totalCount, 11)
  expect_equal(tree$height, 3)
  expect_equal(tree$Linux$Debian$name, "Debian")
  expect_equal(tree$Linux$Debian$version, 9)


})


test_that("as.Node.list empty list()", {
  lol = list(a = list(aa=1), b="hello", c = list())
  tree = FromListSimple(lol)
  expect_equal(tree$totalCount, 3)
  expect_equal(tree$height, 2)
  expect_equal(tree$c$count, 0)
  expect_equal(tree$c$attributes, character(0))
})


test_that("as.Node.list with empty attributes", {
  lol = list(a = list(aa=1), "hello", c = list(), 1, d=2, list(e=4))
  tree = FromListSimple(lol)
  expect_equal(length(tree$attributes), 3)
  expect_equal(tree$count, 3)
  expect_true(all(c(1, 2) %in% tree$attributes))
})




test_that("as.list.Node prune", {
  data(acme)
  
  l <- ToListExplicit(acme, pruneFun = function(node) node$name != 'Research')
  
  expect_equal("list", class(l))
  expect_equal(2, length(l$children))
  expect_equal(c('Accounting', 'IT'), names(l$children))

})

test_that("as.list.Node prune", {
  data(acme)
  
  l <- as.list(acme, pruneFun = function(node) node$name != 'Outsource')
  
  expect_equal("list", class(l))
  expect_equal(c('Go agile', 'Switch to R'), names(l$IT))
  
})
