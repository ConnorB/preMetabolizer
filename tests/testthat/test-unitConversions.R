test_that("cfs_to_cms() converts discharge from CFS to CMS", {
  expect_equal(cfs_to_cms(2.34), 0.06626142198)
})
