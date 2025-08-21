#' Request a GPT API Chat Completion or Responses API result
#'
#' Sends a request to the specified OpenAI API (Chat Completions or Responses API) and returns the parsed response.
#'
#' @param token Character. API token for authentication (required).
#' @param base.url Character. Base URL of the API endpoint (e.g., "https://api.openai.com").
#' @param model Character. Model identifier to use (e.g., "gpt-4o", "o1-deep-research").
#' @param api.type Character. API type to use: "completions" or "responses".
#' @param is.deep.research Logical. Whether this is a deep research model (default: FALSE).
#' @param is.reasoning Logical. Whether this is a reasoning model (default: FALSE).
#' @param messages List. Conversation history for completions API (list of lists with role/content).
#' @param prompt Character or List. Prompt text or message list for responses API.
#' @param prompt.id Character. Unique ID for the prompt (responses API).
#' @param instructions Character. Text for the system prompt (responses API).
#' @param tools Character vector. Tools to enable (e.g., c("web_search_preview")).
#' @param max.tokens Integer. Max tokens to generate (default: 1024).
#' @param temperature Numeric. Sampling temperature (default: 1 for completions, 0.7 for responses).
#' @param stream Logical. Stream partial responses (default: FALSE, completions only).
#' @param top.p Numeric. Nucleus sampling parameter (default: 1).
#' @param frequency.penalty Numeric. Frequency penalty (default: 0).
#' @param presence.penalty Numeric. Presence penalty (default: 0).
#' @param stop Character vector. Stop sequences for completions API.
#' @param timeout Numeric. Request timeout in seconds (default: 60).
#' @param max.retries Integer. Maximum number of retry attempts (default: 3).
#' @param retry.delay Numeric. Base delay between retries in seconds (default: 1).
#' @param retry.backoff Logical. Whether to use exponential backoff for retries (default: TRUE).
#'
#' @return Parsed API response as list.
#' @importFrom httr2 request req_headers req_body_json req_timeout req_retry req_perform resp_is_error resp_status resp_body_json
#' @export
getGptApiResponse <- function(token,
                              base.url,
                              model,
                              api.type = c("completions", "responses"),
                              is.deep.research = FALSE,
                              is.reasoning = FALSE,
                              messages = NULL,
                              prompt = NULL,
                              prompt.id = NULL,
                              instructions = NULL,
                              tools = NULL,
                              max.tokens = 1024,
                              temperature = NULL,
                              stream = FALSE,
                              top.p = 1,
                              frequency.penalty = 0,
                              presence.penalty = 0,
                              stop = NULL,
                              timeout = 60,
                              max.retries = 3,
                              retry.delay = 1,
                              retry.backoff = TRUE) {

  # Basic validation
  api.type <- match.arg(api.type)

  if (missing(token) || token == "") stop("API token is required")
  if (missing(base.url) || base.url == "") stop("Base URL is required")
  if (missing(model) || model == "") stop("Model is required")

  # Build URL
  endpoint.path <- if (api.type == "completions") "/v1/chat/completions" else "/v1/responses"
  url <- paste0(gsub("\\/$", "", base.url), endpoint.path)

  # Build request body
  if (api.type == "completions") {

    if (is.null(messages) || length(messages) == 0) {
      stop("Messages are required for completions API")
    }

    body.list <- list(model = model, messages = messages)

    # Add parameters based on model type
    if (!is.deep.research && !is.reasoning) {
      body.list$max_tokens <- max.tokens
      body.list$temperature <- if (!is.null(temperature)) temperature else 1
      body.list$stream <- stream
      body.list$top_p <- top.p
      body.list$frequency_penalty <- frequency.penalty
      body.list$presence_penalty <- presence.penalty
      if (!is.null(stop)) body.list$stop <- stop
    }

    if (!is.null(tools)) {
      body.list$tools <- lapply(tools, function(t) list(type = t))
    }

  } else {
    # Responses API

    if (is.null(prompt) || length(prompt) == 0) {
      stop("At least one user prompt is required for responses API")
    }

    body.list <- list(model = model)

    # Add temperature if specified and not restricted
    if (!is.null(temperature) && !is.deep.research && !is.reasoning) {
      body.list$temperature <- temperature
    } else if (!is.deep.research && !is.reasoning) {
      body.list$temperature <- 0.7
    }

    # Add instructions
    if (!is.null(instructions)) {
      if (length(instructions) > 1) stop("'instructions' must be a single string")
      body.list$instructions <- instructions
    }

    # Handle prompt input
    if (is.character(prompt)) {
      body.list$input <- list(list(role = "user", content = prompt))
    } else if (is.list(prompt)) {
      body.list$input <- prompt
    } else {
      stop("'prompt' must be a character string or a list of messages")
    }

    # Add tools
    if (!is.null(tools)) {
      body.list$tools <- lapply(tools, function(t) list(type = t))
    }

    # Add prompt ID
    if (!is.null(prompt.id)) body.list$prompt_id <- prompt.id
  }

  # Build httr2 request with retry logic
  req <- httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body.list) |>
    httr2::req_timeout(timeout)

  # Add retry logic with user feedback
  if (max.retries > 0) {
    # Define which status codes should trigger a retry
    transient_errors <- c(429, 500, 502, 503, 504)

    # Set up backoff strategy with logging
    if (retry.backoff) {
      # Exponential backoff: 1s, 2s, 4s, 8s...
      backoff_fn <- function(i) {
        delay <- retry.delay * (2 ^ (i - 1))
        message(sprintf("API request failed (attempt %d/%d). Retrying in %s seconds...",
                        i, max.retries + 1, delay))
        return(delay)
      }
    } else {
      # Fixed delay
      backoff_fn <- function(i) {
        message(sprintf("API request failed (attempt %d/%d). Retrying in %s seconds...",
                        i, max.retries + 1, retry.delay))
        return(retry.delay)
      }
    }

    req <- req |>
      httr2::req_retry(
        max_tries = max.retries + 1,  # +1 because httr2 counts initial attempt
        backoff = backoff_fn,
        is_transient = function(resp) {
          status <- httr2::resp_status(resp)
          if (status %in% transient_errors) {
            message(sprintf("API returned status %d, will retry...", status))
            return(TRUE)
          }
          return(FALSE)
        }
      )
  }

  # Perform the request with progress feedback
  message("Sending API request...")
  tryCatch({
    response <- httr2::req_perform(req)
  }, error = function(e) {
    if (grepl("Timeout", e$message)) {
      stop(paste("API request timed out after", timeout, "seconds. Consider increasing the timeout parameter or checking network connectivity."))
    } else {
      stop(paste("API request failed after", max.retries + 1, "attempts:", e$message))
    }
  })

  # Check for errors
  if (httr2::resp_is_error(response)) {
    error.content <- httr2::resp_body_string(response)
    stop(paste("API request failed with status", httr2::resp_status(response), ":", error.content))
  }

  # Return parsed response
  return(httr2::resp_body_json(response))
}
