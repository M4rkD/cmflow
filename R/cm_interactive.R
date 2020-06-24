# future::plan(future::multicore)
# future::plan(future::sequential)

# Function to compute the adjustment to R
# TODO - untested. note that tests for R0 depend on this.
pick_uadj <- function(R0, scenario_id = NULL) {
  parametersUK1 <- cm_parameters_SEI3R(cm_uk_locations("UK", 0),
    dE = cm_delay_gamma(4.0, 4.0, t_max = 60, t_step = 0.25)$p,
    dIp = cm_delay_gamma(1.5, 4.0, t_max = 60, t_step = 0.25)$p,
    dIs = cm_delay_gamma(3.5, 4.0, t_max = 60, t_step = 0.25)$p,
    dIa = cm_delay_gamma(5.0, 4.0, t_max = 60, t_step = 0.25)$p,
    deterministic = F
  )

  covid_scenario <- covidm:::cm_age_vary_symp_rate__symp_fit_fIa0.5

  # if scenario_id is NULL, then reset this
  if (is.null(scenario_id)) {
    scenario_id <- sample.int(nrow(covid_scenario), 1)
  }

  # 1. Pick age-varying symptomatic rate
  covy <- unname(unlist(covid_scenario[scenario_id, f_00:f_70]))
  covy <- rep(covy, each = 2)

  # 2. Calculate R0 adjustment needed
  parametersUK1$pop[[1]]$y <- covy
  u_adj <- R0 / cm_calc_R0(parametersUK1, 1)

  list(covy = covy, u_adj = u_adj, r = R0)
}

with_u_adjustment <- function(params, adj) {
  for (i in seq_along(params$pop)) {
    params$pop[[i]]$u <- params$pop[[i]]$u * adj$u_adj

    params$pop[[i]]$y <- adj$covy
  }

  return(params)
}

#' Apply selected R0 to parameters
#'
#' @param params params simulation configuration data
#' @param R0 numeric value of R0
#' @param scenario_id the covid scneario to select. A random one will be selected if ommitted or NULL.
#'
#' @return params, with modified $u and $y for each population. The value of R0 is also added to $info for each population
#' @examples
#' params9() %>% with_R0(2.5) %>% simulate()
#'
#' @export
with_R0 <- function(params, R0, scenario_id = NULL) {
  u_adj <- pick_uadj(R0, scenario_id)

  for (i in seq_along(params$pop)) {
    params$pop[[i]]$info$R0 <- R0
  }

  with_u_adjustment(params, u_adj)
}

calc_population_sizes <- function(populations) {
  # compute population sizes
  map_int(1:length(populations), ~ as.integer(sum(populations[[.x]]$size)))
}

get_population_sizes <- function(level) {
  "Function to compute population sizes.
Note: I expect this could be done in a far simpler way."
  locations <- covidm::cm_uk_locations("UK", 3)
  parameters <- covidm::cm_parameters_SEI3R(locations,
    dE = cm_delay_gamma(4.0, 4.0, t_max = 60, t_step = 0.25)$p, # 6.5 day serial interval.
    dIp = cm_delay_gamma(1.5, 4.0, t_max = 60, t_step = 0.25)$p, # 1.5 days w/o symptoms
    dIs = cm_delay_gamma(3.5, 4.0, t_max = 60, t_step = 0.25)$p, # 5 days total of infectiousness
    dIa = cm_delay_gamma(5.0, 4.0, t_max = 60, t_step = 0.25)$p, # 5 days total of infectiousness here as well.
    deterministic = F
  )

  # compute population sizes
  map_int(1:186, ~ as.integer(sum(parameters$pop[[.x]]$size)))
}

#' Classification by region of UK locations
#'
#' @param locations list of location, as returned by cm_uk_locations. All of the UK at the UA level by default.
#'
#' @return list of logical vectors, each of which selectes the UAs in that region
#
#' @examples
#' classify_regions_uk()$wales
#' classify_regions_uk()$london
#'
#' @export
classify_regions_uk <- function(locations = NULL) {

  if(is.null(locations)) {
    locations <- covidm::cm_uk_locations("UK", 3)
  }

  list(
    london = covidm:::cm_structure_UK[match(str_sub(locations, 6), Name), Geography1 %like% "London"],
    england = covidm:::cm_structure_UK[match(str_sub(locations, 6), Name), Code %like% "^E" & !(Geography1 %like% "London")],
    wales = covidm:::cm_structure_UK[match(str_sub(locations, 6), Name), Code %like% "^W"],
    scotland = covidm:::cm_structure_UK[match(str_sub(locations, 6), Name), Code %like% "^S"],
    nireland = covidm:::cm_structure_UK[match(str_sub(locations, 6), Name), Code %like% "^N"],
    westmid = covidm:::cm_structure_UK[match(str_sub(locations, 6), Name), Name == "West Midlands (Met County)"],
    cumbria = covidm:::cm_structure_UK[match(str_sub(locations, 6), Name), Name == "Cumbria"]
  )
}

# finally, I want to convert to seed day index (e.g. 1,1,1,1,2,2,2,...)
save_run_result <- function(R, ipop, run) {
  save_as <- paste0(store, "run-R=", R, "-pop=", ipop, ".rds")
  qsave(run, save_as)
}

# TODO - missing tests
create_gran_matrix <- function(pop) {
  "Create a grandparent matrix, by combining other matrices."

  if (length(pop$matrices) != 8) {
    stop("Current matrices must be of size 8")
  }

  # Recover home/other contact matrix
  mat_ref <- pop$matrices[[1]] + pop$matrices[[4]] +
    pop$matrices[[5]] + pop$matrices[[8]]

  gran <- 5 / 7 # adjustment for weekdays only.
  N <- nrow(mat_ref)
  popsize <- pop$size
  mat <- matrix(0, ncol = N, nrow = N)

  # Add child-grandparent contacts: under 15s to 55+s
  for (a in 1:3) {
    dist <- c(rep(0, 10 + a), mat_ref[a, (11 + a):N])
    dist <- dist / sum(dist)
    mat[a, ] <- mat[a, ] + gran * dist
    mat[, a] <- mat[, a] + (gran * dist) * (popsize[a] / popsize)
  }

  mat
}

#' Build default parameters from contact matrices
#'
#' @param date_start simulation start date
#' @param date_end simulation end date
#' @param locations list of location to simlulate, whole of UK at UA level if NULL or omitted.
#'
#' @return default params
#
#' @examples
#' params() %>% ...
#'
#' @export
params <- function(date_start, date_end, locations = NULL) {
  if (is.null(locations)) {
    locations <- covidm::cm_uk_locations("UK", 3)
  }

  cm_parameters_SEI3R(locations,
    date_start = date_start, date_end = date_end,
    dE = cm_delay_gamma(4.0, 4.0, t_max = 60, t_step = 0.25)$p, # 6.5 day serial interval.
    dIp = cm_delay_gamma(1.5, 4.0, t_max = 60, t_step = 0.25)$p, # 1.5 days w/o symptoms
    dIs = cm_delay_gamma(3.5, 4.0, t_max = 60, t_step = 0.25)$p, # 5 days total of infectiousness
    dIa = cm_delay_gamma(5.0, 4.0, t_max = 60, t_step = 0.25)$p, # 5 days total of infectiousness here as well.
    deterministic = F
  )
}

run_simulation <- function(params, run = 1, model_seed = 0) {
  "Runs the simulation, setting the runtime."
  start_time <- Sys.time()

  # cm_backend_simulate will cache results by default
  # I get around this by always setting n=1
  # and setting run to the value it should be
  # outside of cm_simulate
  result <- cm_simulate(params, n = 1, model_seed)

  result$dynamics$run <- run

  result$runtime_seconds <- Sys.time() - start_time

  return(result)
}

#' Build 9-matrix default parameters from contact matrices
#'
#' Additional matrices are added for elderly shielding and child-grandparent contact.
#'
#' @param date_start simulation start date
#' @param date_end simulation end date
#' @param locations list of location to simlulate, whole of UK at UA level if NULL or omitted.
#'
#' @return default params (with additional matrices)
#
#' @examples
#' params9() %>% ...
#'
#' @export
params9 <- function(date_start, date_end, locations = NULL) {
  "Build parameters and split into 9 matrices"

  if(date_end < date_start) {
    stop("Attempt to build parameters with date_end before date_start")
  }

  if (is.null(locations)) {
    locations <- covidm::cm_uk_locations("UK", 3)
  }

  parameters <- params(date_start, date_end, locations)

  # Split off the elderly (70+, age groups 15 and 16) so their contact matrices can be manipulated separately
  parameters <- cm_split_matrices_ex_in(parameters, 15)

  # Create additional matrix for child-elderly contacts
  parameters$pop <- map(parameters$pop, function(pop) {
    pop$matrices$gran <- create_gran_matrix(pop)
    pop
  })

  # add zero contact for gran matrix
  # grandparent matrix has no contact by default
  parameters$pop <- map(parameters$pop, function(p) {
    p$contact <- c(p$contact, 0)
    p
  })

  parameters
}

params_adj_base_with_seeds <- function(r, u_adj, seed_matrix, params, info = NULL) {
  "Run the base case with the specified seed matrix"

  params$pop <- seed_params(params$pop, seed_matrix, 4)

  # for some reason, this didn't work when replacing lapply with purr
  params$pop <- lapply(params$pop, function(p) {
    p$y <- u_adj$covy
    p
  })

  # apply covid scenario and R adjustment
  params$pop <- map(params$pop, function(p) {
    p$u <- p$u * u_adj$u_adj
    p
  })

  params
}

pop_set <- function(params, ...) {
  "Sets value in parameters for populations and returns the results.
Note, this does not mutate the input parameters."
  values <<- list(...)

  if (is.null(pop_idxs)) {
    pop_idxs <- 1:length(params$pop)
  }

  for (name in names(values)) {
    if (!name %in% names(params$pop[[1]])) {
      stop(paste0("Name ", name, " not found in params"))
    }

    for (i in pop_idxs) {
      params$pop[[i]][name] <- values[[name]]
    }
  }
  params
}

fetch_in_pop <- function(params, var, func) {
  map(params$pop, var)
}

modify_in_pop <- function(params, var, func) {
  params$pop <- map(params$pop, function(p) p[var] <- func(p[var]))

  return(params)
}

with_validate <- function(params) {

  covidm::cm_check_parameters(params)

  if (length(map(params$pop, ~ .x$seed_times)) != length(params$pop)) {
    stop("Validation failed: Check the values for params$pop[[i]]$seed_times")
  }
  if (unique(dim(params$travel)) != length(params$pop)) {
    stop("Validation failed: Check the travel matrix size")
  }

  return(params)
}

#' Apply seeding
#'
#' Applies a seeding to params, based on a number of seeding options
#'
#' @param params params simulation configuration data
#' @param seed_matrices one of the allowed values of seeding. If a numeric vector,
#' then the numeric vector is applied to each population as a seeding vector.
#' This is a vector of the length number of day to seed, and each value is the number
#' of seeds for that day.
#' If seed_matrices is a matrix, then each row is treated as a seeding vector for the
#' corresponding population at that index.
#' If a function, then the function will be called with two arguments,
#' params and population index, and should return the seeding vector for the
#' populatoin in the second parameter.
#' @param translation a function used to translate the seeding vector to the
#' format used by the covidm package. Set this to unity to set the seeding directly in
#' the format used by covidm. This is a numeric vector of one entry per seed,
#' with the value specifying the day on which that seed occured.
#' @param dist_ages distribution of age seeds to use for all populations. If null, everyone
#' in a fixed age range are seeded equally.

#' @param params params simulation configuration data
#'
#' @return default params (with seeds set)
#
#' @examples
#' params9() %>% with_seeding(c(1,2,3))
#' params9() %>% with_seeding(function(params, ipop) { c(1,2,3) })
#' params9() %>% with_seeding(seed_matrix)
#'
#' @export
with_seeding <- function(params, seed_matrices, translation = trans_seed_format, dist_ages = NULL) {

  if (is.null(dist_ages)) {
    age_min <- 25
    age_max <- 50
    age_bounds <- 5 * 0:16

    dist_ages <- cm_age_coefficients(age_min, age_max, age_bounds)
  }

  params$pop <- lapply(params$pop, `[[<-`, "dist_seed_ages", dist_ages)

  ith <- function(ipop) {
    if (is.matrix(seed_matrices)) {
      seed_matrices[, ipop]
    } else if (is.function(seed_matrices)) {
      seed_matrices(params, ipop)
    } else if (is.vector(seed_matrices) & is.numeric(seed_matrices)) {
      seed_matrices
    } else {
      stop("Unknown type for seed matrix")
    }
  }

  for (ipop in seq_along(params$pop)) {
    params$pop[[ipop]]$seed_times <- translation(ith(ipop))
  }

  return(params)
}

with_store <- function(params) {
  .debug_params <<- params
}

with_multiply_field <- function(params, fields, factor) {
  with_map_fields(params, fields, function(x) x * factor)
}

with_map_fields <- function(params, fields, func) {
  for (field in fields) {
    params <- with_map_field(params, field, func)
  }

  return(params)
}

with_map_field <- function(params, field, func) {
  params$pop <- map(params$pop, ~ modify_in(.x, field, func))

  return(params)
}

filter_pop <- function(params, pop_idxs) {
  params$pop <- params$pop[pop_idxs]
  params$travel <- params$travel[pop_idxs, pop_idxs]
}



#' Apply default health info
#'
#' @param params params simulation configuration data
#'
#' @return params, with applied defauly health info
#' @examples
#' params9() %>% with_default_health_info
#'
#' @export
with_default_health_info <- function(params) {
  probs <- fread(
    "Age,Prop_symptomatic,IFR,Prop_inf_hosp,Prop_inf_critical,Prop_critical_fatal,Prop_noncritical_fatal,Prop_symp_hospitalised,Prop_hospitalised_critical
10,0.66,8.59E-05,0.002361009,6.44E-05,0.5,0,0,0.3
20,0.66,0.000122561,0.003370421,9.19E-05,0.5,9.47E-04,0.007615301,0.3
30,0.66,0.000382331,0.010514103,0.000286748,0.5,0.001005803,0.008086654,0.3
40,0.66,0.000851765,0.023423527,0.000638823,0.5,0.001231579,0.009901895,0.3
50,0.66,0.001489873,0.0394717,0.001117404,0.5,0.002305449,0.018535807,0.3
60,0.66,0.006933589,0.098113786,0.005200192,0.5,0.006754596,0.054306954,0.3
70,0.66,0.022120421,0.224965092,0.016590316,0.5,0.018720727,0.150514645,0.3
80,0.66,0.059223786,0.362002579,0.04441784,0.5,0.041408882,0.332927412,0.3
100,0.66,0.087585558,0.437927788,0.065689168,0.5,0.076818182,0.617618182,0.3"
  )

  reformat <- function(P) {
    # 70-74,3388.488  75-79,2442.147  80-84,1736.567  85-89,1077.555  90-94,490.577  95-99,130.083  100+,15.834
    x <- c(P[1:7], weighted.mean(c(P[8], P[9]), c(3388.488 + 2442.147, 1736.567 + 1077.555 + 490.577 + 130.083 + 15.834)))
    return(rep(x, each = 2))
  }

  P.icu_symp <- reformat(probs[, Prop_symp_hospitalised * Prop_hospitalised_critical])
  P.nonicu_symp <- reformat(probs[, Prop_symp_hospitalised * (1 - Prop_hospitalised_critical)])
  P.death_icu <- reformat(probs[, Prop_critical_fatal])
  P.death_nonicu <- reformat(probs[, Prop_noncritical_fatal])
  hfr <- probs[, Prop_noncritical_fatal / Prop_symp_hospitalised]

  params$processes <- list(
    list(
      source = "Ip", type = "multinomial", names = c("to_icu", "to_nonicu", "null"), report = c("", "", ""),
      prob = matrix(c(P.icu_symp, P.nonicu_symp, 1 - P.icu_symp - P.nonicu_symp), nrow = 3, ncol = 16, byrow = T),
      delays = matrix(c(cm_delay_gamma(7, 7, 60, 0.25)$p, cm_delay_gamma(7, 7, 60, 0.25)$p, cm_delay_skip(60, 0.25)$p), nrow = 3, byrow = T)
    ),

    list(
      source = "to_icu", type = "multinomial", names = "icu", report = "p",
      prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
      delays = matrix(cm_delay_gamma(10, 10, 60, 0.25)$p, nrow = 1, byrow = T)
    ),

    list(
      source = "to_nonicu", type = "multinomial", names = "nonicu", report = "p",
      prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
      delays = matrix(cm_delay_gamma(8, 8, 60, 0.25)$p, nrow = 1, byrow = T)
    ),

    list(
      source = "Ip", type = "multinomial", names = c("death", "null"), report = c("o", ""),
      prob = matrix(c(P.death_nonicu, 1 - P.death_nonicu), nrow = 2, ncol = 16, byrow = T),
      delays = matrix(c(cm_delay_gamma(22, 22, 60, 0.25)$p, cm_delay_skip(60, 0.25)$p), nrow = 2, byrow = T)
    )
  )

  return(params)
}

with_dates <- function(params, start_date, end_date = NULL, days = NULL) {
  params$date0 <- start_date

  if (is.null(end_date)) {
    end_date <- ymd(start_date) + days
  }

  if (!is.null(end_date) & !is.null(days)) {
    stop("Error: shouldn't provide both end_date and days")
  }

  params$time1 <- end_date

  return(params)
}

#------------------------------------------------------------------------------
# Parameter sweeps and collecting results
#------------------------------------------------------------------------------
with_run <- function(irun, .vars, .func) {
  "Run a parameter run for the given combo."
  with_sweep(.vars[irun, ], func)
}

#' Sweep over parameters
#'
#' Given a data frame of parameters sets (.vars) and a function (.func),
#' call the function once per data frame row, passing the field
#' values as named arguments to .func.
#' Func should return a set of params for one simulation.
#' Run the simulation with these params, calling action with the params and result each time.
#' Must be the last entry function in the chain
#'
#' @param .vars data frame of values to run
#' @param .func function of params, and the values of fields in
#' the data frame. Should return a set of params for the run.
#' @param action an action to perform on each run, should
#' return the value for with_sweep to return
#'
#' @return list of result of action function on the result
#' of the simulation. This will be the simulation results
#' and error information by default.
#
#' @examples
#' params9() %>% with_vars(df, function(params, R0) {
#'                                        with_R0(paras, R0)
#'                                      })
#'
#' @export
with_simulate <- function(params,
                          output_dir = NULL,
                          .vars = NULL,
                          .func = function(params) params,
                          overwrite = FALSE) {

  # params is used from this environment
  # of the runner function and the default
  # .func function
  params <- .apply_schedule(params)
  force(params)

  runner <- function(run, ...) {
    # Compute params by calling .func
    params <- .func(params, ...)

    # Set sweep vars
    vars <- list(...)

    # put run back into vars
    vars$run <- run

    params$info$vars <- vars

    # every call to runner should have a run column
    # seed every run with the index of the run
    result <- run_simulation(params, run, model_seed = run)

    # note: output_dir = NULL is only allowed for
    # when .vars it NULL (i.e. individual runs)
    if(!is.null(output_dir)) {
      .save_file_to_uuid_location(output_dir, result, vars)
    } else {
      result
    }
  }

  if(is.null(.vars)) {
    runner(run = 1)
  } else {
    if(is.null(output_dir)) {
      stop("Please provide output_dir when .vars is not NULL")
    }

    # add_column requires a data frame, and vars is a function
    # in default R
    if(!is.data.frame(.vars)) {
      stop("Please ensure that .vars is a data frame")
    }

    # ensure that every dataframe
    # has a run column. The runner function
    # depends on this. In particular, the run ID
    # is also used to seed simulation runs.
    .vars <- add_column(.vars,
                        run=1:dim(.vars)[[1]],
                        .before=names(.vars)[1])

    future_pmap(.vars,
                runner,
                .progress = TRUE) %>%
      bind_rows()
  }
}

.save_file_to_uuid_location <- function(output_dir, result, vars) {
  full_dir <- file.path(getwd(), output_dir, "results")

  ifelse(!dir.exists(full_dir), dir.create(full_dir, recursive = TRUE), FALSE)

  result_filepath <- file.path(full_dir, paste0(vars$run, ".rds"))

  qsave(result, result_filepath)

  tibble(!!!vars, file = result_filepath)
}

# ac_collect_dynamics <- function(vars, results) {
#   "Bind a list of simulation results into a single tibble"
#   i <- 0
#   reduce(results, function(agg, res) {
#     i <<- i + 1
#     res$dynamics$run <- i
#     rbind(agg, res$dynamics)
#   }, .init = tibble())
# }
#
# with_sweep_collecting <- function(...) {
#   "Run a sweep, and collect/bind the dynamics."
#   with_collect_dynamics(with_sweep(...))
# }
