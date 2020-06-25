store_dynamics <- function(conn, dynamics, run_id = NULL) {
  if (!is.null(run_id)) {
    dynamics$run <- run_id
  }

  dbWriteTable(conn, "dynamics", dynamics, overwrite = FALSE, append = TRUE)

  # create an index for all columns if it doesn't exist
  columns <- names(dynamics)
  for(col in columns) {
    done <- has_index_SQLite(conn, col)
    if(!done) {
      query <- sprintf("CREATE INDEX index_%s ON dynamics ('%s')", col, col)
      dbExecute(conn, query)
    }
  }

  return(TRUE)
}

#' @export
with_tbl <- function(dbname) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname)

  dplyr::tbl(conn, "dynamics")
}

get_store_directory <- function(files) {
  dirs <- unique(sub("/results/[0-9]*.rds", "", files))
  if (length(dirs) != 1) {
    stop("unable to determine base directory")
  }

  dirs
}

#' @export
with_database <- function(output_dir, dbname = NULL, overwrite = FALSE) {

  if (is.null(dbname)) {
    dbname <- file.path(output_dir, "db.sqlite")
  } else if (file.exists(dbname) && !overwrite) {
    stop("Database exists")
  }

  files = Sys.glob(file.path(output_dir, "results", "*"))

  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname)

  out <- imap(files,
    function(file, index, conn) {
      cat(sprintf("%d : %s\n", index, file))

      results <- qread(file)

      dynamics <- results$dynamics
      dynamics$file <- file

      # add vars to dynamics, first removing run
      args <- results$base_parameters$info$vars

      if(!unique(args$run) == unique(dynamics$run)) {
        stop("Discrepancy with run IDs")
      }

      dynamics$run <- NULL

      dynamics <- add_column(dynamics, !!!args, .before = 1)

      # write to database
      store_dynamics(conn, dynamics)
    },
    conn = conn
  )

  dbDisconnect(conn)

  return(dbname)
}

has_index_SQLite <- function(conn, index_name) {
  col_name <- str_c("index_", index_name)

  dplyr::tbl(conn, "sqlite_master") %>%
    filter(name %like% col_name) %>%
    count() %>%
    collect() %>%
    .[[1]] > 0
}
