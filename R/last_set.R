#' Get latest released LoR set number
#'
#' @return A numeric integer
#' @export
#'
#' @examples
#' last_set()
last_set <- function() {

  # perform GET request
  req <- httr::GET("https://dd.b.pvp.net/latest/core/en_us/data/globals-en_us.json")

  # extract content from the GET
  content <- httr::content(req, encoding = "UTF-8")

  # extract sets name from JSON content
  sets <- jsonlite::fromJSON(content)[["sets"]]$nameRef

  # extract number from sets and convert to numeric
  sets <- as.numeric(stringi::stri_extract(sets, regex = '[0-9]+'))

  # get maximum number (it's the latest set released)
  last_set <- max(sets, na.rm = TRUE)

  # return last_set
  return(last_set)

}
