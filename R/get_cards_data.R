#' Get all sets released
#'
#' @param trim boolean; shold the "Set" prefix be removed? (default: true)
#'
#' @return A vector with all released sets
#' @export
get_sets <- function(trim = TRUE) {

  # perform GET request
  req <- httr::GET("https://dd.b.pvp.net/latest/core/en_us/data/globals-en_us.json")

  # extract content from the GET
  content <- httr::content(req, encoding = "UTF-8")

  # extract sets name from JSON content
  sets <- jsonlite::fromJSON(content)[["sets"]]$nameRef

  # if needed, remove the "Set" prefix
  if(trim){ sets <- gsub(pattern = "Set", replacement = "", x = sets) }

  # return last_set
  return(sets)

}

#' Get all data about cards from a specific set
#'
#' @param set The number of the set
#' @param select A character vector with the fields to keep (return all fields if NULL)
#' @param use_latest Should the "latest" JSON be used? or the current version one? (default: TRUE)
#'
#' @return A dataframe
#' @export
get_set_cards_data <- function(
  set,
  select = NULL,
  use_latest = TRUE
  ) {

  patch <- ifelse(
    use_latest,
    "latest",
    paste0(gsub('\\.', '_', lorr::get_current_patch(since_last_change = FALSE)), "_0")
  )

  # perform GET request
  req <- httr::GET(
    sprintf(
      "https://dd.b.pvp.net/%2$s/set%1$s/en_us/data/set%1$s-en_us.json",
      set,
      patch
    )
  )

  # extract content from the GET
  content <- httr::content(req, encoding = "UTF-8")

  # extract data from JSON content
  data <- jsonlite::fromJSON(content)

  # # FOR SET6CDE, RIOT SPLIT CARDS INTO 2 SETS >.>
  # if(set == "6cde"){
  #   req2 <- httr::GET("https://dd.b.pvp.net/3_16_0/set6cde/en_us/data/set6cde-en_us.json")
  #   content2 <- httr::content(req2, encoding = "UTF-8")
  #   data2 <- jsonlite::fromJSON(content2)
  #   data <- rbind(data, data2)
  # }

  # if needed, keep only some columns
  if(!is.null(select)){ data <- data[select] }

  # return last_set
  return(data)

}


#' Get all data about cards
#'
#' @param select A character vector with the fields to keep (return all fields if NULL)
#' @param use_latest Should the "latest" JSON be used? or the current version one? (default: TRUE)
#'
#' @return A dataframe
#' @export
get_cards_data <- function(select = NULL, use_latest = TRUE) {

  # Get list of all sets released on LoR
  # "Event" is a special set that has no cards
  sets <- setdiff(lorr::get_sets(), 'Event')

  # pull data for all sets and bind them
  data <- purrr::map_dfr(
    .x = purrr::set_names(sets, sets),
    .f = ~lorr::get_set_cards_data(., select = select, use_latest = use_latest),
    .id = "set"
  )

  # set is a special column so needs to be removed in a special way
  if(!is.null(select) & !"set" %in% select){
    data <- data[setdiff(colnames(data), 'set')]
  }

  # return last_set
  return(data)

}
