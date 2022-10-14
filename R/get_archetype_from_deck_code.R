#' Extract region from mono-region champions
#'
#' @param champs A string with a list of champion codes
#' @param data_champs Champions data from LoR Data Dragon (if NULL, the default, it will be fetched within the function)
#'
#' @return A character string with the list of regions from monoregion champs
#' @export
#'
get_monoregion <- function(
  champs,
  data_champs = NULL
  ){

  # if not provided, fetch champs data
  if(is.null(data_champs)){

    # champions names / codes / regions from set JSONs
    data_champs <- lorr::get_cards_data(
      select = c('name', 'cardCode', 'regionRefs', 'rarity')
    )

    # keep only champions & remove rarity column
    data_champs <- data_champs[
      data_champs$rarity == 'Champion' & nchar(data_champs$cardCode) <= 8,
      c('name', 'cardCode', 'regionRefs')
    ]

  }

  # split champions string
  champs <- unlist(stringr::str_split(champs, pattern = " "))

  # get data of these champs
  data_champs <- data_champs[data_champs$cardCode %in% champs, ]

  # add number of regions to data
  data_champs$n_regions <- purrr::map_int(data_champs$regionRefs, length)

  # keep only monoregion champs
  data_champs <- data_champs[data_champs$n_regions == 1, ]

  # convert to string
  monoregions <- gsub(
    x = paste0(data_champs$cardCode, collapse = " "),
    pattern = "[0-9]",
    replacement = ''
  )

  # return result
  return(monoregions)

}

#' Get an archetype name from a deck code
#'
#' @param deck_code A character string with the code of the deck
#' @param engine One of "r" or "python" (default: "r")
#' @param data_champs Champions data from LoR Data Dragon (if NULL, the default, it will be fetched within the function)
#'
#' @return A character string
#' @export
#'
get_archetype_from_deck_code <- function(
  deck_code,
  engine = "r",
  data_champs = NULL
  ) {

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

  # if not provided, fetch champs data
  if(is.null(data_champs)){

    # champions names / codes / regions from set JSONs
    data_champs <- lorr::get_cards_data(
      select = c('name', 'cardCode', 'regionRefs', 'rarity')
    )

    # keep only champions & remove rarity column
    data_champs <- data_champs[
      data_champs$rarity == 'Champion' & nchar(data_champs$cardCode) <= 8,
      c('name', 'cardCode', 'regionRefs')
    ]

  }

  # extract champions from cards list
  champs <- stringr::str_extract_all(
    cards,
    pattern = paste0('[1-3]:', data_champs$cardCode, collapse = "|")
  )

  # convert to a single string
  champs <- paste0(champs[[1]], collapse = " ")

  # remove champ from the list if it's played only as 1x
  champs <- gsub(
    x = champs,
    pattern = paste0('[2-3]:|', paste0('1:', data_champs$cardCode, collapse = "|")),
    replacement = ''
  )

  # trim the new name (in case we removed something)
  champs <- stringr::str_squish(champs)

  # get faction of mono region champs
  champs_factions <- lorr::get_monoregion(champs = champs, data_champs = data_champs)

  # return result (still needs fixing)
  return(champs)

}
