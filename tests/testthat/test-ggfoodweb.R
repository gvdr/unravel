# just a minimal test to start with
test_that("rejects non tbl_graph", {
  expect_error(ggfoodweb(1))
})
