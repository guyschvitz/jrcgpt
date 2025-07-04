#' Request a GPT API Chat Completion or Responses API result
#'
#' Sends a request to the specified OpenAI API (Chat Completions or Responses API) and returns the parsed response.
#'
#' @param token Character. API token for authentication (required).
#' @param base.url Character. Base URL of the API endpoint (e.g., "https://api.openai.com").
#' @param model Character. Model identifier to use (e.g., "gpt-4o").
#' @param api.type Character. API type to use: "completions" or "responses".
#' @param messages List. Conversation history for completions API (list of lists with role/content).
#' @param prompt Character. Prompt text for responses API.
#' @param prompt.id Character. Unique ID for the prompt (responses API).
#' @param tools Character vector. Tools to enable for responses API (e.g., c("web_search")).
#' @param max.tokens Integer. Max tokens to generate (default: 1024).
#' @param temperature Numeric. Sampling temperature (default: 1 for completions, 0.7 for responses).
#' @param stream Logical. Stream partial responses (default: FALSE, completions only).
#' @param top.p Numeric. Nucleus sampling parameter (default: 1).
#' @param frequency.penalty Numeric. Frequency penalty (default: 0).
#' @param presence.penalty Numeric. Presence penalty (default: 0).
#' @param stop Character vector. Stop sequences for completions API.
#' @param timeout Numeric. Request timeout in seconds (default: 60).
#'
#' @return Parsed API response as list.
#' @export
getGptApiResponse <- function(token,
                              base.url,
                              model,
                              api.type = c("completions", "responses"),
                              messages = NULL,
                              prompt = NULL,
                              prompt.id = NULL,
                              tools = NULL,
                              max.tokens = 1024,
                              temperature = NULL,
                              stream = FALSE,
                              top.p = 1,
                              frequency.penalty = 0,
                              presence.penalty = 0,
                              stop = NULL,
                              timeout = 60) {

  api.type <- match.arg(api.type)

  if (missing(token) || token == "") {
    stop("API token is required")
  }

  if (missing(base.url) || base.url == "") {
    stop("Base URL is required")
  }

  if (missing(model) || model == "") {
    stop("Model is required")
  }

  endpoint.path <- if (api.type == "completions") {
    "/v1/chat/completions"
  } else {
    "/v1/responses"
  }

  url <- paste0(gsub("\\/$", "", base.url), endpoint.path)

  if (api.type == "completions") {

    if (is.null(messages) || length(messages) == 0) {
      stop("Messages are required for completions API")
    }

    body.list <- list(
      model = model,
      messages = messages,
      max_tokens = max.tokens,
      temperature = if (!is.null(temperature)) { temperature } else { 1 },
      stream = stream,
      top_p = top.p,
      frequency_penalty = frequency.penalty,
      presence_penalty = presence.penalty
    )

    if (!is.null(stop)) {
      body.list$stop <- stop
    }

  } else { # responses

    if (is.null(prompt) || prompt == "") {
      stop("Prompt text is required for responses API")
    }

    # Create the body list with proper structure for responses API
    body.list <- list(
      model = model,
      messages = list(
        list(
          role = "user",
          content = prompt
        )
      ),
      temperature = if (!is.null(temperature)) { temperature } else { 0.7 }
    )

    # Add tools if specified
    if (!is.null(tools)) {
      # Convert tools to proper format - responses API expects tool objects
      tool_objects <- lapply(tools, function(tool) {
        if (tool == "web_search") {
          list(type = "web_search")
        } else {
          list(type = tool)
        }
      })
      body.list$tools <- tool_objects
    }
  }

  # Convert to JSON with proper formatting
  json_body <- jsonlite::toJSON(body.list, auto_unbox = TRUE, pretty = FALSE)

  # Debug: Print the JSON body to see what's being sent
  cat("Sending JSON body:\n", json_body, "\n")

  response <- httr::POST(
    url = url,
    body = json_body,
    httr::add_headers(
      "Authorization" = paste("Bearer", token),
      "Content-Type" = "application/json"
    ),
    httr::timeout(timeout)
  )

  if (httr::http_error(response)) {
    error.content <- httr::content(response, "text", encoding = "UTF-8")
    stop(paste("API request failed with status", httr::status_code(response), ":", error.content))
  }

  return(httr::content(response, "parsed", encoding = "UTF-8"))
}

# Your example usage (updated to remove prompt.id):
response2 <- getGptApiResponse(
  token = openai.api.key$key,
  base.url = "https://api.openai.com",
  model = "gpt-4o",
  api.type = "responses",
  prompt = "What is the current inflation rate in the EU?",
  tools = c("web_search")
)
