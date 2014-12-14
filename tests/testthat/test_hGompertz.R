library(eha2)
context("Gamma-Gompertz")

x <- 2
res1 <- hGgompertz(x)
res2 <- exp(x)

test_that("hGgompertz does the right thing", {
  expect_equal(res1, res2)
})
