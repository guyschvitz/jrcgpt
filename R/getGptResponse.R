#' Request a GPT API Chat Completion
#'
#' Sends a chat completion request to the GPT API and returns the parsed response.
#'
#' @param token Character. API token for authentication (required).
#' @param base.url Character. Base URL of the API endpoint (e.g., \code{"https://api.openai.com"}).
#' @param endpoint.path Character. The API endpoint path appended to \code{base.url}. Default is \code{"/v1/chat/completions"}.
#' @param model Character. Model identifier to use (e.g., \code{"gpt-35-turbo-0613"}).
#' @param messages List. List of messages forming the conversation history. Each message should be a named list with \code{role} and \code{content}.
#' @param max.tokens Integer. Maximum number of tokens to generate in the completion. Default is 1024.
#' @param temperature Numeric. Sampling temperature to control randomness. Must be between 0 and 2. Default is 1.
#' @param stream Logical. Whether to stream partial responses as they become available. Default is \code{FALSE}.
#' @param top.p Numeric. Nucleus sampling parameter to control diversity (0 to 1). Default is 1.
#' @param frequency.penalty Numeric. Penalty for repeating tokens (-2 to 2). Default is 0.
#' @param presence.penalty Numeric. Penalty for introducing new tokens (-2 to 2). Default is 0.
#' @param stop Character vector. Optional stop sequences that, when encountered, end the generation.
#' @param timeout Numeric. Request timeout in seconds. Default is 60.
#'
#' @return A list containing the parsed API response.
#'
#' @examples
#' \dontrun{
#' response <- getGptResponse(
#'   token = "YOUR_API_TOKEN",
#'   base.url = "https://api.openai.com",
#'   model = "gpt-35-turbo-0613",
#'   messages = list(
#'     list(role = "system", content = "You are a helpful assistant."),
#'     list(role = "user", content = "Hello!")
#'   )
#' )
#' }
#' @export
getGptResponse <- function(token,
                           base.url,
                           endpoint.path = "/v1/chat/completions",
                           model,
                           messages,
                           max.tokens = 1024,
                           temperature = 1,
                           stream = FALSE,
                           top.p = 1,
                           frequency.penalty = 0,
                           presence.penalty = 0,
                           stop = NULL,
                           timeout = 60) {

  # Validate inputs
  if (missing(token) || is.null(token) || token == "") {
    stop("API token is required")
  }

  if (missing(base.url) || is.null(base.url) || base.url == "") {
    stop("Base URL is required")
  }

  if (missing(model) || is.null(model) || model == "") {
    stop("Model is required")
  }

  if (missing(messages) || length(messages) == 0) {
    stop("Messages are required")
  }

  # Construct the full URL
  url <- paste0(gsub("\\/$", "", base.url), endpoint.path)

  # Prepare the request body
  request.body <- list(
    model = model,
    messages = messages,
    max_tokens = max.tokens,
    temperature = temperature,
    stream = stream,
    top_p = top.p,
    frequency_penalty = frequency.penalty,
    presence_penalty = presence.penalty
  )

  # Add stop sequences if provided
  if (!is.null(stop)) {
    request.body$stop <- stop
  }

  # Prepare headers
  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", token),
    "Content-Type" = "application/json"
  )

  # Make the API call
  tryCatch({
    response <- httr::POST(
      url = url,
      body = jsonlite::toJSON(request.body, auto_unbox = TRUE),
      headers,
      httr::timeout(timeout)
    )

    # Check for HTTP errors
    if (httr::http_error(response)) {
      error.content <- httr::content(response, "text", encoding = "UTF-8")
      stop(paste("API request failed with status", httr::status_code(response), ":", error.content))
    }

    # Parse the response
    result <- httr::content(response, "parsed", encoding = "UTF-8")

    return(result)

  }, error = function(e) {
    stop(paste("Error calling GPT API:", e$message))
  })
}
