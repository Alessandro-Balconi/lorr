#' Connect to MySQL database
#'
#' @return A connection object
#' @export
#'
#' @examples
#' create_db_con()
create_db_con <- function() {

  # load mysql db credentials
  db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

  # create connection to MySQL database
  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    db_host  = "127.0.0.1",
    user     = db_creds$uid,
    password = db_creds$pwd,
    dbname   = db_creds$dbs
  )

  # return connection
  return(con)

}
