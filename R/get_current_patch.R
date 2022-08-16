#' Get info on the most recent patch released
#'
#' @param what One of "patch", "patch_regex", "release_date" (default: "patch")
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
