library(httr)
library(jsonlite)

#see https://platform.openai.com/docs/guides/gpt for documentation and keys

#replace this key with your own
api_key <- "YOURAPIKEY"

#there are different endpoints, we will use the chat completion endpoint
endpoint <- "https://api.openai.com/v1/engines/davinci/completions"

#create an HTTP post
response <- POST(
  url = endpoint,
  add_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  ),
  body = '{"prompt": "What is R?","max_tokens": 150}', 
  encode = "json"
)
content <- content(response, "parsed")

#POST returns a list
class(content)

#access the return text from within the list
content$choices[[1]]$text


