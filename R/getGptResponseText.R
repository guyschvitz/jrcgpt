#' Extract Text Content from a GPT API Response
#'
#' Retrieves the generated text content from a GPT API response object.
#'
#' @param gpt.response List. The response object returned by \code{getGptResponse()}.
#'
#' @return Character. The text content of the first choice in the response.
#'
#' @examples
#' \dontrun{
#' response <- getGptResponse(token, base.url, model, messages)
#' text <- getGptResponseText(response)
#' }
#'
#' @export
getGptResponseText <-  function(gpt.response) {
  texts <- character()
  # Guard against missing resp$output
  if (!is.list(gpt.response) || is.null(gpt.response$output)) return(texts)
  for (out in gpt.response$output) {
    # each out$content should be a list of messages
    if (is.list(out$content)) {
      for (msg in out$content) {
        if (!is.null(msg$text)) {
          texts <- c(texts, msg$text)
        }
      }
    }
  }
  return(texts)
}

