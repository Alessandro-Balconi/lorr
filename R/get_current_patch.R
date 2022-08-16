#' Get info on the most recent patch released
#'
#' @param what One of "patch", "patch_regex" (default: "patch")
#' @param since_last_change If TRUE, returns all patches since the last one with changes; otherwise returns only the very last one (default: TRUE)
#' @param collapse If since_last_change = TRUE, should patches be collapsed into a single character string? (default: TRUE)
#' @param sep separator to use for collapsing when collapse = TRUE (default: "|")
#'
#' @return A character string (if since_last_change = FALSE or collapse = TRUE); otherwise character vector
#' @export
get_current_patch <- function(
  what = 'patch',
  since_last_change = TRUE,
  collapse = since_last_change,
  sep = '|'
) {

  # extract needed info
  res <- lorr::get_db_query(
    query = "
    WITH lagged_change AS (
      SELECT
        {what},
        release_date,
        LAG(`change`) OVER (
          ORDER BY release_date DESC
          ROWS UNBOUNDED PRECEDING
        ) AS `lag_change`
      FROM
        utils_patch_history
      {if(!since_last_change) 'LIMIT 1' else '' }
    ),
    main_db AS (
      SELECT
        {what},
        SUM(COALESCE(`lag_change`, 0)) OVER (
          ORDER BY release_date DESC
          ROWS UNBOUNDED PRECEDING
        ) AS cum_change
      FROM
        lagged_change
    )

    SELECT
      {what}
    FROM
      main_db
    WHERE
      cum_change = (SELECT MIN(cum_change) FROM main_db)
    ",
    what = what,
    since_last_change = since_last_change
  )

  # convert from dataframe to array
  res <- res[[what]]

  # if needed collapse into a single string
  if(collapse){
    res <- paste0(res, collapse = sep)
  }

  # return result
  return(res)

}

#' Get info on the release date of a patch
#'
#' @param patch The patch to check, in the format "live_X_XX" (if NULL, uses the latest patch since changes)
#' @param on_live If TRUE, adds 1 day to return the date when the patch actually went live (default: TRUE)
#'
#' @return A date
#' @export
get_patch_release_date <- function(
  patch = NULL,
  on_live = TRUE
) {

  # if patch = NULL, fetch most recent patch
  if(is.null(patch)){
    patch <- lorr::get_current_patch(what = 'patch_regex')
  }

  # get first occurence of patch in the data
  release_date <- lorr::get_db_query(
    query = "
    SELECT
      MIN(game_start_time_utc)
    FROM
      ranked_match_metadata_30d
    WHERE
      game_version REGEXP '{patch}'
    ",
    patch = patch
  )

  release_date <- release_date[[1]]

  # if needed, add 1 day to get the actual patch release date
  if(on_live){
    release_date <- release_date + 86400
  }

  # return result
  return(release_date)

}
