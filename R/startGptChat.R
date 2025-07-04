#' Start a GPT Chat
#'
#' Creates a messages list containing the initial message, formatted for use with the GPT API.
#'
#' @param content Character. The content of the initial message.
#' @param role Character. The role of the sender (e.g., \code{"user"}, \code{"assistant"}, or \code{"system"}). Default is \code{"user"}.
#'
#' @return A list containing the initial message, formatted for use as the \code{messages} parameter in \code{getGptResponse()}.
#'
#' @examples
#' startGptChat("Hello!")
#'
#' @export
startGptChat <- function(content, role = "user") {
  return(list(list(role = role, content = content)))
}
