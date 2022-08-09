#' Get data from a query in MySQL
#'
#' @param query The query to execute
#' @param limit Maximum number of rows to retrive default: unbounded
#' @param print_text Print query text? default: false
#' @param print_df Print first rows of df? default: false
#' @param conn connection to the db; if not provided, create one and close it after the query
#'
#' @return A dataframe
#' @export
#'
get_db_query <- function(
  query,
  limit = -1,
  print_text = FALSE,
  print_df = FALSE,
  conn = NULL
  ) {

  # create connection with db (if not provided)
  # also if we create it, we'll close it after the query
  if(is.null(conn)){
    close_conn_after <- TRUE
    conn <- lorr::create_db_con()
  } else {
      close_conn_after <- FALSE
    }

  # print query if needed
  if(print_text){ cat(glue::glue(query)) }

  # get data
  data <- DBI::dbGetQuery(
    conn = conn,
    statement = glue::glue(query),
    n = limit)

  # print first rows of df if needed
  if(print_df){ utils::head(data) }

  # close connection if we opened it
  if(close_conn_after){ DBI::dbDisconnect(conn) }

  # return data
  return(data)

}
