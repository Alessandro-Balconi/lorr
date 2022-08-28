#' Get an archetype name from a deck code
#'
#' @param deck_code A character string with the code of the deck
#' @param engine One of "r" or "python" (default: "r")
#'
#' @return A character string
#' @export
#'
get_archetype_from_deck_code <- function(deck_code, engine = "r") {

  if(engine == "r") {

    cards <- lordecks::get_decklist_from_code(deck_code, format = "simple")

  } else if(engine == 'python') {

    lor_deckcodes <- reticulate::import("lor_deckcodes")
    cards <- lor_deckcodes$decode$decode_deck(deck_code)

  } else {

    stop('engine must be one of: "r", "python"')

  }

  # convert from vector to single string
  cards <- paste0(cards, collapse = " ")

  # return cards (still needs fixing)
  return(cards)

}
