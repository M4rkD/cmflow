test_that("sweep over set of parameters returning result", {
  vars <- data.frame(R0 = seq(2.0, 3.0, 0.1))

  result <- params9(date_start="2020-01-01", date_end="2020-01-02") %>%
    with_vars_run(vars, function(params, R0) {
      with_R0(params, R0)
    },
    action = function(result) {
      # for each result, return the unique R0
      # over all populations
      unique(unlist(map(result$pop, ~ .x$info$R0)))
    }) %>%
    unlist()

  expect_equal(result, vars$R0)
})

test_that("sweep vars calls run_and_return_safely action", {
  vars <- data.frame(R0 = seq(2.0, 3.0, 0.1))

  with_mock(
    run_and_return_safely = function(params) { "run-and-return-safely" },

    result <- params9(date_start="2020-01-01", date_end="2020-01-02") %>%
      with_vars_run(vars, function(params, R0) {
        with_R0(params, R0)
      }))

  expect_equal("run-and-return-safely", unique(unlist(result)))
})

test_that("sweep vars with no .func", {
  vars <- data.frame(R0 = seq(2.0, 3.0, 0.1))

  with_mock(
    run_and_return_safely = function(params) { "run-and-return-safely" },

    result <- params9(date_start="2020-01-01", date_end="2020-01-02") %>%
      with_vars_run(vars,
                    # action just returns R0 in sweep_vars
                    action = function(params) {
                      params$info$sweep_vars$R0
                    }))

  expect_equal(unlist(result), vars$R0)
})
