context("end-to-end tests")

testPath <- normalizePath("temp")

unlink(testPath, recursive = TRUE)
dir.create(testPath)
test_that("fresh init without errors", {
  
  expect_equal(courseR::init(path = testPath), testPath)
  
})

test_that("fresh build without errors", {
  
  expect_true(courseR::build(path = testPath))
  
})


test_that("package loads", {
  devtools::install(file.path(testPath, "dist", "temp"))
})

test_that("startAssignment works", {
  studentPath <- file.path(testPath, "student")
  
  unlink(studentPath, recursive = TRUE)
  dir.create(studentPath)
  expect_type(temp::startAssignment('03-assignment', path = studentPath), type = "character")
  expect_true(file.exists(file.path(studentPath, "03-assignment.Rmd")))
})
