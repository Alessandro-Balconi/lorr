#' Execute a query in MySQL
#'
#' @param query The query to execute (either the path to a .sql file, or a character string)
#' @param print_text Print query text? default: false
#' @param conn connection to the db; if not provided, create one and close it after the query
#' @param bigint Class to use for numeric values (one of integer", "numeric", "character", "integer64")
#' @param ... Additional parameters to use for parametric queries
#'
#' @export
execute_db_query <- function(
  query,
  print_text = FALSE,
  conn = NULL,
  bigint = "integer64",
  ...
) {

  # create connection with db (if not provided)
  # also if we create it, we'll close it after the query
  if(is.null(conn)){
    close_conn_after <- TRUE
    conn <- lorr::create_db_con(bigint = bigint)
  } else {
    close_conn_after <- FALSE
  }

  # get query statement
  query <- lorr::get_query_statement(query)

  # replace query params if any
  query <- glue::glue(query, ..., .envir = parent.frame())

  # print query if needed
  if(print_text){ cat(query) }

  # execute query
  DBI::dbExecute(conn = conn, statement = query)

  # close connection if we opened it
  if(close_conn_after){ DBI::dbDisconnect(conn) }

}
