#' Get all data about LoR regions
#'
#' @param select A character vector with the fields to keep (return all fields if NULL)
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' get_regions_data()
get_regions_data <- function(select = NULL) {

  # perform GET request
  req <- httr::GET(
    "https://dd.b.pvp.net/latest/core/en_us/data/globals-en_us.json"
  )

  # extract content from the GET
  content <- httr::content(req, encoding = "UTF-8")

  # extract data from JSON content
  data <- jsonlite::fromJSON(content)[['regions']]

  # if needed, keep only some columns
  if(!is.null(select)){ data <- data[select] }

  # return last_set
  return(data)

}
