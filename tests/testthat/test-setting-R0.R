test_that("sets R0 according to pick_uadj", {
  params <- params9(date_start="2020-01-01", date_end="2020-01-02")
  params_ref <- copy(params)

  params <- with_R0(params, 2.5, scenario_id = 1)

  u_adj <- pick_uadj(2.5, scenario_id = 1)

  # build a matrix
  for (ipop in seq_along(params$pop)) {
    u_new <- params_ref$pop[[ipop]]$u * u_adj$u_adj
    params_ref$pop[[ipop]]$u <- u_new

    params_ref$pop[[ipop]]$y <- u_adj$covy

    # This is added just to keep track, not used by simulation
    params_ref$pop[[ipop]]$info$R0 <- 2.5
  }

  expect_equal(params_ref, params)
})
