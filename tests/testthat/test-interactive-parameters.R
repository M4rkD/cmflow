library(lubridate)

test_that("can build UK params from dates", {
  date_start = "2020-01-01"
  date_end = "2020-01-02"
  params = covidmflow::params9(date_start = date_start,
                               date_end = date_end)
  expect_equal(params$date0, date_start)
  expect_equal(params$time1, date_end)

  # should be UK unitary authorities by default
  expect_equal(length(params$pop), 186)
})

test_that("refuses to build params when end date before start date", {
  date_start = "2020-01-01"
  date_end = "2020-01-02"
  expect_error(params9(date_start = date_end,
                       date_end = date_start))
})

test_that("can build correctly named single UA with parameter", {
  date_start = "2020-01-01"
  date_end = "2020-01-02"
  params = covidmflow::params9(date_start = date_start,
                               date_end = date_end,
                               UAuth$Swansea)

  # should have correct name
  expect_equal(params$pop[[1]]$name, "UK | Swansea")
})

test_that("can build correctly named double UA with parameter", {
  date_start = "2020-01-01"
  date_end = "2020-01-02"
  params = covidmflow::params9(date_start = date_start,
                               date_end = date_end,
                               c(UAuth$Swansea, UAuth$Cardiff))

  # should have correct names
  expect_equal(params$pop[[1]]$name, "UK | Swansea")
  expect_equal(params$pop[[2]]$name, "UK | Cardiff")
})

build_default_params <- function() {
  date_start = "2020-01-01"
  date_end = "2021-01-01"

  params = covidmflow::params9(date_start = date_start,
                               date_end = date_end)
}

test_that("sets standard school terms", {
  # Do pipeline
  params <- build_default_params() %>%
    with_standard_school_terms

  # Check pipeline
  terms <- get_school_terms()

  # duration of simulation time in days
  duration <- as.numeric(ymd(params$time1) - ymd(params$date0))

  # get schedule
  schedule <- params$pop[[1]]$schedule

  # timesteps where scaling set to 1 for all matrices
  no_iv <- map_dbl(keep(schedule, ~ all(.x$contact == rep(1, 9))), "t")

  # timesteps with closed schools (total scaling == 7)
  school_iv <- map_dbl(keep(schedule, ~ all(.x$contact == c(1,1,0,1,1,1,0,1,1))), "t")

  # indices of open and close
  idx_of_close <- as.numeric(sort(ymd(c(terms$close)) - ymd(params$date0)))
  idx_of_reopen <- as.numeric(sort(ymd(c(terms$reopen)) - ymd(params$date0)))

  # indices of open and close
  idx_of_close <- idx_of_close[idx_of_close <= duration]
  idx_of_reopen <- idx_of_reopen[idx_of_reopen <= duration]

  # make sure I'm testing something...
  # we should have at least two points for each
  expect_gt(length(idx_of_close), 2)
  expect_gt(length(idx_of_reopen), 2)

  # check opening closing times are equal
  expect_equal(idx_of_close, school_iv)

  # check opening times are equal
  # note: in this case, reopening happens the day after the
  # specified date, and day zero is also classed as "opening"
  # regardless of school dates
  expect_equal(c(0, idx_of_reopen + 1), no_iv)
})

