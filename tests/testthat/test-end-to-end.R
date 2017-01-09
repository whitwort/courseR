context("end-to-end tests")

dir.create("temp")
test_that("fresh init and build run without errors", {
  
  expect_equal(courseR::init(path = "temp"), "temp")
  expect_true(courseR::build(path = "temp"))
  
})

test_that("package loads", {
  devtools::install("temp/dist/temp/")
})

test_that("startAssignment works", {
  dir.create("temp/student")
#  expect_type(temp::startAssignment('03-assignment.Rmd', path = student), type = "character")
  expect_type(temp::startAssignment('03-assignment', path = "temp/student"), type = "character")
  expect_true(file.exists("temp/student/03-assignment.Rmd"))
  unlink("temp/student")
  
})

# unlink("temp")
