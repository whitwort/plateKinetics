context("Loading experimental design & data")

test_that("well syntax parses correctly", {
  
  t1 <- "A1, A2, B1, B2"
  t2 <- "A1->B2"
  t3 <- "A1 -> B2"
  t4 <- "A1->A2, B1, B2"
  
  a  <- c("A1","A2","B1","B2")
  
  for (pName in names(platforms)) {
    for (t in c(t1, t2, t3, t4)) {

      r <- expandWells(t, platforms[[pName]])
      expect_is(r, 'character', info = paste("test:", t, "platform:", pName))
      expect_equal(r, a, info = paste("test:", t, "platform:", pName))
      
    }
  }
  
})

test_that("newDesign writes a readable default design file", {
  s <- newDesign()
  
  expect_true(file.exists("design.yaml"))
  expect_is(loadDesign(), 'list')
  
})

test_that("loadDesign loads good test designs", {
  expect_is(loadDesign(), 'list')
  expect_is(loadDesign(file = "design.yaml"), 'list')
  expect_is(loadDesign(file = "magellan.design.yaml"), 'list')
  
  expect_message(loadDesign(file = "missingdefaults.yaml"))
  expect_is(loadDesign(file = "missingdefaults.yaml"), 'list')
  
})

test_that("loadDesign fails on invalid designs", {
  no.files  <- list.files(pattern = "no-*")
  bad.files <- list.files(pattern = "bad-*")
  
  for (file in c(no.files, bad.files)) {
    expect_error(loadDesign(file = file), info = file)
  } 
  
})

test_that("loadData produces identical results across optional input structures", {
  sources <- list.files(pattern = "^source-*")
  
  d   <- loadDesign(file = "design.yaml")
  ref <- loadData("data1.txt", read.table, d)
  
  for (file in sources) {
    test <- loadData(file, read.table, d)
    expect_identical(ref, test, info = file)
  }
  
})

test_that("loadExperiment works on good tests end-to-end", {
  designs <- c("design.yaml", "magellan.design.yaml")
  
  for (file in designs) {
    d <- loadDesign(file = file)
    expect_is(loadExperiment(design = d), 'list')
  }
  
})


