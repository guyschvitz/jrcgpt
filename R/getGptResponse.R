#' Request a GPT API Chat Completion or Responses API result
#'
#' Sends a request to the specified OpenAI API (Chat Completions or Responses API) and returns the parsed response.
#'
#' @param token Character. API token for authentication (required).
#' @param base.url Character. Base URL of the API endpoint (e.g., "https://api.openai.com").
#' @param model Character. Model identifier to use (e.g., "gpt-4o", "o1-deep-research").
#' @param api.type Character. API type to use: "completions" or "responses".
#' @param messages List. Conversation history for completions API (list of lists with role/content).
#' @param prompt Character. Prompt text for responses API.
#' @param prompt.id Character. Unique ID for the prompt (responses API).
#' @param instructions Character. Text for the system prompt (responses API).
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
                              instructions = NULL,
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

  # Check if this is a deep research model that doesn't support certain parameters
  is_deep_research <- grepl("deep-research|o1-deep-research", model, ignore.case = TRUE)

  if (api.type == "completions") {

    if (is.null(messages) || length(messages) == 0) {
      stop("Messages are required for completions API")
    }

    body.list <- list(
      model = model,
      messages = messages
    )

    # Only add parameters that are supported by the model
    if (!is_deep_research) {
      body.list$max_tokens <- max.tokens
      body.list$temperature <- if (!is.null(temperature)) { temperature } else { 1 }
      body.list$stream <- stream
      body.list$top_p <- top.p
      body.list$frequency_penalty <- frequency.penalty
      body.list$presence_penalty <- presence.penalty

      if (!is.null(stop)) {
        body.list$stop <- stop
      }
    } else {
      # For deep research models, only include basic parameters
      if (!is.null(temperature)) {
        warning("Temperature parameter is not supported for deep research models and will be ignored.")
      }

      # Deep research models require specific tools
      if (is.null(tools) || length(tools) == 0) {
        # Default to web_search_preview if no tools specified
        body.list$tools <- list(list(type = "web_search_preview"))
        cat("Deep research model detected: automatically enabling web_search_preview tool\n")
      } else {
        # Validate that required tools are present
        valid_tools <- c("web_search_preview", "mcp")
        if (!any(tools %in% valid_tools)) {
          stop("Deep research models require at least one of 'web_search_preview' or 'mcp' tools.")
        }
        body.list$tools <- lapply(tools, \(t) list(type = t))
      }
    }

  } else { # responses -------------------------------------------------------

    ## --- basic checks ------------------------------------------------------
    if (is.null(prompt) || length(prompt) == 0)
      stop("At least one user prompt is required for responses API.")

    ## --- build the body ----------------------------------------------------
    body.list <- list(
      model = model
    )

    # Only add temperature if not a deep research model
    if (!is_deep_research) {
      body.list$temperature <- if (!is.null(temperature)) temperature else 0.7
    } else if (!is.null(temperature)) {
      warning("Temperature parameter is not supported for deep research models and will be ignored.")
    }

    ## (a) system / instructions --------------------------------------------
    if (!is.null(instructions)) {
      if (length(instructions) > 1)
        stop("'instructions' must be a single string.")
      body.list$instructions <- instructions
    }

    ## (b) user input --------------------------------------------------------
    # Accept EITHER a single string OR a pre-built list of role/content pairs.
    if (is.character(prompt)) {
      body.list$input <- list(list(role = "user", content = prompt))
    } else if (is.list(prompt)) {
      body.list$input <- prompt              # assume user supplied full spec
    } else {
      stop("'prompt' must be a character string or a list of messages.")
    }

    ## (c) tools -------------------------------------------------------------
    if (!is_deep_research) {
      # Regular models: add tools if specified
      if (!is.null(tools)) {
        body.list$tools <- lapply(tools, \(t) list(type = t))
      }
    } else {
      # Deep research models: tools are required
      if (is.null(tools) || length(tools) == 0) {
        # Default to web_search_preview if no tools specified
        body.list$tools <- list(list(type = "web_search_preview"))
        cat("Deep research model detected: automatically enabling web_search_preview tool\n")
      } else {
        # Validate that required tools are present
        valid_tools <- c("web_search_preview", "mcp")
        if (!any(tools %in% valid_tools)) {
          stop("Deep research models require at least one of 'web_search_preview' or 'mcp' tools.")
        }
        body.list$tools <- lapply(tools, \(t) list(type = t))
      }
    }

    ## (d) optional prompt.id -----------------------------------------------
    if (!is.null(prompt.id)) body.list$prompt_id <- prompt.id
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
