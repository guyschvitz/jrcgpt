#' Add a Message to a GPT Chat
#'
#' Appends a new message to an existing GPT messages list.
#'
#' @param messages List. An existing list of GPT messages.
#' @param content Character. The content of the new message.
#' @param role Character. The role of the sender of the new message (e.g., \code{"user"}, \code{"assistant"}, or \code{"system"}). Default is \code{"user"}.
#'
#' @return A list containing the updated sequence of messages.
#'
#' @examples
#' msgs <- startGptChat("Hello!")
#' msgs <- addGptMessage(msgs, "Hi, how can I help you?", "assistant")
#'
#' @export
addGptMessage <- function(messages, content, role = "user") {
  new.message <- list(role = role, content = content)
  return(append(messages, list(new.message)))
}
