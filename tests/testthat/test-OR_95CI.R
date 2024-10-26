test_that("Basic functionality", {
  expect_equal(OR_95CI(0.5, 0.1, 0.05, 2), "1.65 (1.36, 2.01)")
  expect_equal(OR_95CI(1.0, 0.2, 0.05, 2), "2.72 (1.84, 4.02)")
})

test_that("Rounding works correctly", {
  expect_equal(OR_95CI(0.555, 0.123, 0.05, 3), "1.742 (1.369, 2.217)")
  expect_equal(OR_95CI(1.111, 0.111, 0.01, 1), "3.0 (2.3, 4.0)")
})

test_that("Edge cases are handled", {
  expect_equal(OR_95CI(0, 0.1, 0.05, 2), "1.00 (0.82, 1.22)")
  expect_equal(OR_95CI(10, 0.5, 0.05, 2), "22026.47 (8266.93, 58687.50)")
})

test_that("Invalid inputs return errors", {
  expect_error(OR_95CI("a", 0.1, 0.05, 2), "All arguments must be numeric")
  expect_error(OR_95CI(0.5, -0.1, 0.05, 2), "Standard error must be positive")
  expect_error(OR_95CI(0.5, 0.1, 1.5, 2), "Significance level must be between 0 and 1")
})
