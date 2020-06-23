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

      cm_iv_set(iv, ymd_start, ymd_end, contact = contact)
      cm_iv_set(iv, ymd_start, ymd_end, trace_intervention = 2)

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
