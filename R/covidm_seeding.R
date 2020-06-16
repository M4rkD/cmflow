plot_seed_matrix <- function(matrix, location_names = NULL, plot = T) {
  "Plot a single seed matrix"
  df <- melt(matrix)

  names(df) <- c("time", "pop", "seeds")

  if (!is.null(location_names)) {
    df$pop <- factor(location_names)
  }

  if (plot) {
    ggplot(df, aes(x = time / 4, y = seeds)) +
      geom_line(color = "blue") +
      facet_wrap(. ~ pop)
  } else {
    return(df)
  }
}

trans_seed_format <- function(seeds) {
  "Translate a row of seeds (one row of the seed matrix) to the format expected by the C++ code.

seeds is a list of numbers, one per day, designating the number of new cases for that day."
  idx <- which(seeds > 0)
  rep <- seeds[idx]
  unlist(mapply(function(i, n) rep(i, n), idx, rep))
}

plot_compare_seed_matrices_wales <- function(matrix1, matrix2) {
  "Plot two seeding matrices, both picking population labels for Wales"
  df <- merge(melt(matrix1), melt(matrix2), by = c("Var1", "Var2"))

  names(df) <- c("day", "population", "explicit expression", "calculated")

  ggdata <- melt(df, id.vars = c("day", "population"))

  welsh_locations <- locations[wales]
  ggdata$population <- factor(welsh_locations[ggdata$population])

  plt <- ggplot(ggdata, aes(x = day, y = value, color = variable)) +
    geom_line() +
    facet_wrap(. ~ population)
}


#------------------------------------------------------------------
# Plot seeding
#------------------------------------------------------------------

extract_seed_times <- function(pops) {
  "Extract the number of seeds in each region as a data frame"
  df <- reduce(pops, function(agg, pop) {
    seed_times <- pop$seed_times
    df <- data.table(aggregate(data.frame(seeds = seed_times), list(t = seed_times), length))
    df$population <- pop$name
    rbind(agg, df)
  }, .init = data.table())

  # add cumulative sum per region
  df$region_csum <- ave(df$seeds, df$population, FUN = cumsum)

  return(df)
}

combined_seed_and_E <- function(result) {
  df_seed <- extract_seed_times(result$base_parameters$pop)
  df_dyn <- data.table(result$dynamics)[compartment == "E", .(total = sum(value)), by = .(t, population)]
  df <- merge(df_dyn, df_seed)

  # make total an integer (not double)
  df$total <- as.integer(df$total)

  return(df)
}


plot_seeding_vs_exposed <- function(result, ...) {
  "Plot the number of seeded (from the input data) vs the number of exposed.

Particularly useful when setting u and rates to zero."
  df <- combined_seed_and_E(result) %>%
    melt(id.vars = c("population", "t"))

  # set the order of the factor
  df$variable <- factor(df$variable, c("total", "region_csum", "seeds"))


  ggplot(df[...], aes(x = t, y = value, color = variable)) +
    geom_line() +
    facet_grid(population ~ variable)
}

print_max_exposed_minus_cum_seeded <- function(result, ...) {
  "Plot the difference between seeded (from the input data) vs the number of exposed.

Note, this is usually a fairly useless plot.

Particularly useful when setting u and rates to zero."
  df <- combined_seed_and_E(result)

  df$diff <- df$total - df$region_csum

  df$max <- ave(df$diff, df$population, FUN = sum)

  # return a summary
  summary <- df[, .(max = min(max)), by = .(population)]
  format_population_df(summary)
}

#--------------------------------------------
# Seeders
#--------------------------------------------

# Exponential growth curve for seeding
#' @export
seeder_source_exp_growth_curve <- function(init = 10, total = 100, expgrowth = 1.05, p_ht = 0.5, ht_day = NULL, ndays = 67) {
  weights <- rep(0, ndays)
  weights[init] <- 1
  for (i in seq(init + 1, ndays)) {
    weights[i] <- weights[i - 1] * expgrowth
  }
  curve <- weights * total * (1 - p_ht) / sum(weights)

  if(!is.null(ht_day)) {
    if(ht_day > ndays) {
      stop("Half term pulse can't happen after the end of seeding")
    }

    curve[ht_day] <- curve[ht_day] + total * p_ht
  }

  return(curve)
}

#' @export
seeder_weight_by_population <- function(curve, pop, ipop) {
  "Return seeding matrix."
  pop_sizes <- calc_population_sizes(pop)
  fractional_pop <- pop_sizes / sum(pop_sizes)

  curve * fractional_pop[[ipop]]
}

seeder_sample_poisson__functional <- function() {
  last_seed <- NULL

  # prevents repeat seeds, as these could cause issues if same seed used for
  # every population
  function(curve, seed = NULL, prevent_repeat_seeds=TRUE) {
    if(!is.null(seed)) {
      # check not called repeatedly with the same seed
      if(!is.null(last_seed) && last_seed == seed && prevent_repeat_seeds) {
        stop("Repeatedly using the same sampling for poisson seeds. Are you sure you're not using the same seeding for every population in a simulation? This could cause issues.")
      }
      last_seed <<- seed

      set.seed(seed)
    }

    rpois(length(curve), curve)
  }
}

#' This is a closure in order to preserve the last seed, so that errors can be thrown if repeated
#' @export
seeder_sample_poisson <- seeder_sample_poisson__functional()

#' @export
seeder_default_exp <- function(params, ipop, init = 10, total = 100, expgrowth = 1.05, p_ht = 0.5, ht_day = 48, ndays = 67, seed = NULL) {
  seeder_source_exp_growth_curve(init = 10, total = 100, expgrowth = 1.05, p_ht = 0.5, ht_day = 48, ndays = 67) %>%
  seeder_weight_by_population(params$pop, ipop) %>%
  seeder_sample_poisson(seed)
}
