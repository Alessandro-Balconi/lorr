#' Get latest released LoR set number
#'
#' @return A numeric integer
#' @export
get_last_set <- function() {

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

#' Get all data about cards from a specific set
#'
#' @param set The number of the set
#' @param select A character vector with the fields to keep (return all fields if NULL)
#'
#' @return A dataframe
#' @export
get_set_cards_data <- function(
  set,
  select = NULL
  ) {

  # perform GET request
  req <- httr::GET(
    sprintf(
      "https://dd.b.pvp.net/latest/set%1$s/en_us/data/set%1$s-en_us.json",
      set
    )
  )

  # extract content from the GET
  content <- httr::content(req, encoding = "UTF-8")

  # extract data from JSON content
  data <- jsonlite::fromJSON(content)

  # if needed, keep only some columns
  if(!is.null(select)){ data <- data[select] }

  # return last_set
  return(data)

}


#' Get all data about cards
#'
#' @param select A character vector with the fields to keep (return all fields if NULL)
#'
#' @return A dataframe
#' @export
get_cards_data <- function(select = NULL) {

  # pull data for all sets and bind them
  data <- purrr::map_dfr(
    .x = 1:(lorr::get_last_set()),
    .f = ~get_set_cards_data(., select = select),
    .id = "set"
  )

  # set is a special column so needs to be removed in a special way
  if(!is.null(select) & !"set" %in% select){
    data <- data[setdiff(colnames(data), 'set')]
  }

  # return last_set
  return(data)

}
