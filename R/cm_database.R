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

  tbl(conn, "dynamics")
}

get_store_directory <- function(files) {
  dirs <- unique(sub("/results/[0-9]*.rds", "", files))
  if (length(dirs) != 1) {
    stop("unable to determine base directory")
  }

  dirs
}

#' @export
with_database <- function(runs, dbname = NULL, overwrite = FALSE) {
  if (is.null(dbname)) {
    db_dir <- get_store_directory(runs$file)
    dbname <- file.path(db_dir, "db.sqlite")
  } else if (file.exists(dbname) && !overwrite) {
    stop("Database exists")
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname)

  out <- pmap(runs,
    function(run, file, ..., conn) {
      results <- qread(file)

      dynamics <- results$dynamics
      dynamics$file <- file

      args <- list(...)

      dynamics <- add_column(dynamics, !!!args, .before = 1)

      store_dynamics(conn, dynamics)
    },
    conn = conn
  )

  dbDisconnect(conn)

  return(dbname)
}

has_index_SQLite <- function(conn, index_name) {
  col_name <- str_c("index_", index_name)

  tbl(conn, "sqlite_master") %>%
    filter(name %like% col_name) %>%
    count() %>%
    collect() %>%
    .[[1]] > 0
}
