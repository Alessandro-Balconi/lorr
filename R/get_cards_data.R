#' Get all data about cards from a specific set
#'
#' @param set The number of the set
#' @param select A character vector with the fields to keep (return all fields if NULL)
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' get_set_cards_data(set = 1)
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
#'
#' @examples
#' get_cards_data()
get_cards_data <- function(select = NULL) {

  # pull data for all sets and bind them
  data <- purrr::map_dfr(
    .x = 1:(lorr::last_set()),
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
