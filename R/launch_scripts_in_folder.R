#' Run all scripts inside a folder, sending a discord message if they fail
#'
#' @param folder Path to the folder with the scripts to run
#' @param log_time boolean, should we keep track of the time needed to run the script? (default: TRUE)
#' @param log_message Message to send alongside the elapsed time in logs
#' @param send_error_to_discord boolean, should a discord message be sent if the scripts fail? (default: TRUE)
#' @param discord_username The username that will be shown as sender on Discord
#' @param discord_message The content of the message sent on Discord
#'
#' @return Nothing, just runs the scripts
#' @export
launch_scripts_in_folder <- function(
  folder,
  log_time = TRUE,
  log_message = 'End: ',
  send_error_to_discord = TRUE,
  discord_username,
  discord_message = "Error. ",
  verbosity = "defult"
){

  # list all scripts in the folder
  scripts <- list.files(folder, full.names = TRUE, pattern = "\\.R$")

  if(log_time) { tictoc::tic() }

  func <- switch(
    verbosity,
    "debug" = function(x){
      cat(paste(x, "start:", Sys.time(), sep = " "))
      source(x)
      cat(paste(x, "end:", Sys.time(), sep = " "))
    },
    function(x) source(x) # default function, just source
  )

  # safely run the scripts, returning an error message if something goes wrong
  tryCatch(
    expr = purrr::walk(.x = scripts, .f = func),
    error = function(e) {
      if(send_error_to_discord){
        lorr::send_discord_message(
          username = discord_username,
          message = sprintf("%s(%s)", discord_message, e$message)
        )
      }
      print(e)
    }
  )

  if(log_time) { cat(log_message); tictoc::toc() }

}
