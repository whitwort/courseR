context("Loading experimental design & data")

test_that("well syntax parses correctly", {
  
  tests <- list( "A1, A2, B1, B2"
               , "A1->B2"
               , "A1 -> B2"
               , "A1->A2, B1, B2"
               , c("A1","A2","B1->B2")
               )
  
  a  <- c("A1","A2","B1","B2")
  
  for (pName in names(platforms)) {
    for (t in tests) {
      
      r <- expandWells(t, platforms[[pName]])
      expect_identical(r, a)
      
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
  
  good <- list.files(pattern = "good-.*")
  for (file in good) {
    expect_is(loadDesign(file = file), 'list', info = file)
  }
  
  expect_message(loadDesign(file = "good-missingdefaults.yaml"))
  
})

test_that("loadDesign fails on invalid designs", {
  no.files  <- list.files(pattern = "^no-.*")
  bad.files <- list.files(pattern = "^bad-.*")
  
  for (file in c(no.files, bad.files)) {
    expect_error(loadDesign(file = file), info = file)
  }
  
})

test_that("loadDesigns gives a message when factor names are being coerced", {
  expect_message(loadDesign(file = "good-renamed-factors.yaml"))
})

test_that("loadData produces identical results across input structures", {
  sources <- list.files(pattern = "^source-*")
  
  d   <- loadDesign(file = "design.yaml")
  ref <- loadData("data1.txt", read.table, d)
  
  for (file in sources) {
    test <- loadData(file, read.table, d)
    expect_identical(ref, test, info = file)
  }
  
})

test_that("loadData correctly appends data when loading from multiple files", {
  
  s  <- loadDesign(file = "good-sequential.yaml")
  ns <- loadDesign(file = "good-nonsequential.yaml")
  
  ds  <- appendData(s$channels$channel, read.table, s)
  dns <- appendData(ns$channels$channel, read.table, ns)
  
  expect_equivalent(ds[order(ds$well),], dns[order(dns$well),])
  
  d <- loadDesign(file = "nonsequential-nooffset.yaml")
  expect_warning(appendData(d$channels$channel, read.table, d))
  
})

test_that("loadExperiment works on good tests end-to-end", {
  good <- list.files(pattern = "^good-.*")
  
  for (file in good) {
    d <- loadDesign(file = file)
    expect_is(loadExperiment(design = d), 'list', info = file)
  }
  
})


