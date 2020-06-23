#' @export
with_intervention <- function(params, start_date, end_date, ...) {
  # check no schedule
  ymd_start <- ymd(start_date)
  ymd_end <- ymd(end_date)

  ipops <- 1:length(params$pop)

  params$pop[ipops] <-
    map(ipops, function(ipop) {
      pop <- params$pop[[ipop]]

      # if it hasn't had an intervention yet, initialise
      # intervention schedule data. Otherwise, use
      # previous data.
      if (is.null(pop$iv)) {
        iv <- cm_iv_build(params)
      } else {
        iv <- pop$iv
        stop("doubling up not tested")
      }

      cm_iv_set(iv, ymd_start, ymd_end, ...)

      pop$iv <- iv

      return(pop)
    })

  return(params)
}

.apply_schedule <- function(params) {
  for (ipop in seq_along(params$pop)) {
    iv <- params$pop[[ipop]]$iv

    if (!is.null(iv)) {
      params$pop[[ipop]]$iv <- NULL

      params <- cm_iv_apply(params, iv, ipop)
    }
  }

  return(params)
}

get_school_terms_uk <- function() {
  list(
    close = c("2020-2-16", "2020-4-05", "2020-5-24", "2020-7-22", "2020-10-25", "2020-12-20", "2021-02-14", "2021-04-01", "2021-05-30", "2021-07-25"),
    reopen = c("2020-2-22", "2020-4-18", "2020-5-30", "2020-9-01", "2020-10-31", "2021-01-02", "2021-02-20", "2021-04-17", "2021-06-05", "2021-09-01")
  )
}

#' Classification by region of UK locations
#'
#' @param params params simulation configuration data
#'
#' @return params, with school terms applied
#
#' @examples
#' params9() %>% with_uk_school_terms %>% run_simulation
#'
#' @export
with_uk_school_terms <- function(params) {
  school_terms <- get_school_terms_uk()
  school_contact <- c(1, 1, 0, 1, 1, 1, 0, 1, 1)
  with_intervention(params,
                    school_terms$close,
                    school_terms$reopen,
                    contact = school_contact,
                    trace_school = 2)

  return(params)
}
