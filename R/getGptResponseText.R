#' Extract Text Content from a GPT API Response
#'
#' Retrieves the generated text content from a GPT API response object.
#' Supports both "responses" and "completions" API endpoint formats.
#'
#' @param gpt.response List. The response object returned by \code{getGptResponse()}.
#' @param endpoint.type Character. Either "responses" (default) or "completions"
#'   to specify which API endpoint format to parse.
#'
#' @return Character. The text content from the response. For "responses" endpoint,
#'   returns a vector of all text content found. For "completions" endpoint,
#'   returns the content of the first choice.
#'
#' @examples
#' \dontrun{
#' # For responses API
#' response <- getGptResponse(token, base.url, model, messages)
#' text <- getGptResponseText(response, "responses")
#'
#' # For completions API
#' completion <- getGptCompletion(token, base.url, model, prompt)
#' text <- getGptResponseText(completion, "completions")
#' }
#'
#' @export
getGptResponseText <- function(gpt.response, endpoint.type = "responses") {

  # Validate inputs
  if (!is.list(gpt.response)) {
    warning("gpt.response must be a list")
    return(character())
  }

  endpoint.type <- match.arg(endpoint.type, c("responses", "completions"))

  if (endpoint.type == "completions") {
    # Handle completions API format: choices[[1]]$message$content
    if (is.null(gpt.response$choices) || length(gpt.response$choices) == 0) {
      warning("No choices found in completions response")
      return(character())
    }

    first.choice <- gpt.response$choices[[1]]
    if (is.null(first.choice$message) || is.null(first.choice$message$content)) {
      warning("No message content found in first choice")
      return(character())
    }

    return(first.choice$message$content)

  } else {
    # Handle responses API format (original logic)
    texts <- character()

    # Guard against missing resp$output
    if (is.null(gpt.response$output)) {
      warning("No output found in responses response")
      return(texts)
    }

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
}
