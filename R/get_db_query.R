#' Read query from a SQL file (or from a charcter string)
#'
#' @param query The query to read (either a file path, or a character string)
#'
#' @return A character string, ready to be read by R
#' @export
get_query_statement <- function(query){

  if(file.exists(query)){

    query <- readLines(con = query)

    query <- gsub(pattern = "--.*", replacement = "", x = query)

    query <- paste(strwrap(x = query, width = .Machine$integer.max), collapse = " ")

  }

  return(query)

}


#' Get data from a query in MySQL
#'
#' @param query The query to execute (either the path to a .sql file, or a character string)
#' @param limit Maximum number of rows to retrive default: unbounded
#' @param print_text Print query text? default: false
#' @param print_df Print first rows of df? default: false
#' @param conn connection to the db; if not provided, create one and close it after the query
#' @param bigint Class to use for numeric values (one of integer", "numeric", "character", "integer64")
#' @param ... Additional parameters to use for parametric queries
#'
#'
#' @return A dataframe
#' @export
get_db_query <- function(
  query,
  limit = -1,
  print_text = FALSE,
  print_df = FALSE,
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

  # get data
  data <- DBI::dbGetQuery(
    conn = conn,
    statement = query,
    n = limit
  )

  # print first rows of df if needed
  if(print_df){ utils::head(data) }

  # close connection if we opened it
  if(close_conn_after){ DBI::dbDisconnect(conn) }

  # return data
  return(data)

}
