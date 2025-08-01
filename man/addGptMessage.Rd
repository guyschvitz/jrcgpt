% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addGptMessage.R
\name{addGptMessage}
\alias{addGptMessage}
\title{Add a Message to a GPT Chat}
\usage{
addGptMessage(messages, content, role = "user")
}
\arguments{
\item{messages}{List. An existing list of GPT messages.}

\item{content}{Character. The content of the new message.}

\item{role}{Character. The role of the sender of the new message (e.g., \code{"user"}, \code{"assistant"}, or \code{"system"}). Default is \code{"user"}.}
}
\value{
A list containing the updated sequence of messages.
}
\description{
Appends a new message to an existing GPT messages list.
}
\examples{
msgs <- startGptChat("Hello!")
msgs <- addGptMessage(msgs, "Hi, how can I help you?", "assistant")

}
