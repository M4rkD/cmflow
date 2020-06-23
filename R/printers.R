#' @export
print.cm.run <- function(cm_run) {
  npops <- length(cm_run$base_parameters$pop)
  nts <- length(unique(cm_run$dynamics$t))

  str <- sprintf(
    "cm.run:
     %d populations
     %d timesteps
     %.2fs runtime",
    npops,
    nts,
    cm_run$runtime_seconds
  )

  writeLines(str)
}
