context("end-to-end")

path <- tempdir()
print(path)

test_that("init creates a project", {
  expect_success({ init(path = path) })
})

test_that("update succeeds", {
  expect_success({ update(path = path) })
})

test_that("newContent succeeds", {
  expect_success({ newContent(file = "test-content.rmd", title = "Test content", path = path) })
})

test_that("newAssignment succeeds", {
  expect_success({ newAssignment(file = "test-assignment.rmd", title = "Test assignment", path = path) })
})

test_that("build succeeds", {
  expect_success({ build(path = path) })
})
