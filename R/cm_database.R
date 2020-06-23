store_file_name <- function(output_dir, file_name, create = TRUE, recreate = FALSE) {

  path <- file.path(output_dir, file_name)
  exists <- file.exists(path)

  if(exists && create) {
    stop("File exists and create is set")
  } else if(exists && create && !recreate) {
    stop("File exists, create is set but recreate is not set")
  } else if(exists && create && recreate) {
    unlink(path)
  }

  return(path)
}

store_db_name <- function(output_dir, create = FALSE, recreate = FALSE) {
  store_file_name(output_dir, "db.sqlite", create, recreate)
}

store_conn <- function(output_dir, create = FALSE, rerun = FALSE) {
  DBI::dbConnect(RSQLite::SQLite(), dbname = store_db_name(output_dir, create = create, recreate = rerun))
}

store_dynamics <- function(conn, dynamics, run_id = NULL) {
  if(!is.null(run_id))
    dynamics$run <- run_id

  dbWriteTable(conn, "dynamics", dynamics, overwrite = FALSE, append = TRUE)
}

store_tbl <- function(output_dir) {
  tbl(store_conn(output_dir), "dynamics")
}

create_database <- function(results, output_dir) {
  conn <- store_conn(output_dir, create = TRUE, rerun = TRUE)

  out <- pmap(results %>% select(run, file),
              function(run, file, conn) {
                results <- qread(file)
                dynamics <- results$dynamics
                dynamics$file <- file
                store_dynamics(conn, dynamics)
              }, conn = conn)

  dbDisconnect(conn)

}
