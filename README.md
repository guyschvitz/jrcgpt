# jrcgpt

An R client for interacting with the GPT API infrastructure of the Joint Research Centre (JRC).

The package provides helper functions to:

-   Start and update chat messages
-   Send requests to the GPT API
-   Extract text content from GPT responses

## Installation

``` r
remotes::install_github("guyschvitz/jrcgpt")
```

## Example

``` r
# Start a chat
messages <- startGptChat("Hello!")

# Add more messages
messages <- addGptMessage(messages, "What is the capital of France?")

# Send request
response <- getGptResponse(
  token = "YOUR_TOKEN",
  base.url = "https://your.jrc.gpt.endpoint",
  model = "gpt-35-turbo-0613",
  messages = messages
)

# Extract response text
text <- getGptResponseText(response)
print(text)
```

## License
MIT License.
