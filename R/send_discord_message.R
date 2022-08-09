#' Send message to discord
#'
#' @param username The username that will be shown as sender on Discord
#' @param message The content of the message
#'
#' @export
#'
send_discord_message <- function(username, message) {

  # webhook to create a discord connection (fixed but could be parametric)
  webhook <- paste(
    "https://discord.com/api",
    "webhooks",
    "940930457070096444",
    "qBSYJH0KETu992oDrdJBH20H1j4yPbBMZm2T3KNKZA5AU1LhRypZshQ0uKly9N_7jeGy",
    sep = "/"
  )

  # create connection
  conn <- discordr::create_discord_connection(
    webhook = webhook,
    username = username
  )

  # send message
  discordr::send_webhook_message(
      message = message,
      conn = conn
    )

}
