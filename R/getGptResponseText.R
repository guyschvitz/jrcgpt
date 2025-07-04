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
getGptResponseText <- function(gpt.response) {
  return(gpt.response$choices[[1]]$message$content)
}
