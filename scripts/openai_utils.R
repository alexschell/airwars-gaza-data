library(assertthat)
library(openai)
library(jsonlite)

# openai::create_chat_completion with support for 'response_format' request parameter
get_completion = function(model = "gpt-4o-mini",
                          messages = NULL,
                          response_format = NULL,
                          openai_api_key = Sys.getenv("OPENAI_API_KEY")) {
  
  task <- "chat/completions"
  base_url <- glue::glue("https://api.openai.com/v1/{task}")
  headers <- c(Authorization = paste("Bearer", openai_api_key), 
               `Content-Type` = "application/json")
  
  body <- list()
  body[["model"]] <- model
  body[["messages"]] <- messages
  if (!is.null(response_format)) body[["response_format"]] <- response_format

  response <- httr::POST(url = base_url, httr::add_headers(.headers = headers), 
                         body = body, encode = "json")
  openai:::verify_mime_type(response)
  parsed <- response %>% httr::content(as = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(flatten = TRUE)
  if (httr::http_error(response)) {
    paste0("OpenAI API request failed [", httr::status_code(response), 
           "]:\n\n", parsed$error$message) %>% stop(call. = FALSE)
  }
  parsed
}

get_completion_typed = function(type,
                                user_prompt, 
                                system_prompt = "You are a helpful assistant.",
                                model = "gpt-4o-mini") {
  
  assertthat::assert_that(type %in% c("character", "numeric", "integer", "logical"))
  assertthat::is.scalar(system_prompt)
  assertthat::is.scalar(model)
  
  if (type == "character") {
    response_format = get_response_format(response = "string")
    map_fn = purrr::map_chr
  } else if (type == "numeric") {
    response_format = get_response_format(response = "number")
    map_fn = purrr::map_dbl
  } else if (type == "integer") {
    response_format = get_response_format(response = "integer")
    map_fn = purrr::map_int
  } else if (type == "logical") {
    response_format = get_response_format(response = "boolean")
    map_fn = purrr::map_lgl
  }
  
  user_prompt |>
    map(\(x) {
      get_completion(model = model,
                     messages = list(
                       list(role = "system", content = system_prompt),
                       list(role = "user", content = x)
                     ),
                     response_format = response_format)
    }) |>
    map(\(x) x$choices$message.content[[1]]) |>
    map(jsonlite::fromJSON) |>
    map_fn(\(x) x$response)
  
}

llm = function(user_prompt, system_prompt = "You are a helpful assistant.", model = "gpt-4o-mini", ...) {
  assertthat::is.scalar(system_prompt)
  assertthat::is.scalar(model)
  user_prompt |>
    map(\(x) {
      get_completion(model = model,
                     messages = list(
                       list(role = "system", content = system_prompt),
                       list(role = "user", content = x)
                     ), ...)
    }) |>
    map_chr(\(x) x$choices$message.content[[1]])
}

llm_lgl = function(...) get_completion_typed(type = "logical", ...)
llm_dbl = function(...) get_completion_typed(type = "numeric", ...)
llm_int = function(...) get_completion_typed(type = "integer", ...)
llm_chr = function(...) get_completion_typed(type = "character", ...)

# x = 1:12
# prompts = glue::glue("Is {x} divisible by 4?")
# tmp = llm_lgl(user_prompt = prompts, model = "gpt-4o-mini")
# cbind(x, as.numeric(tmp))
# 
# prompts = glue::glue("Return {x} / 4, rounded to two decimal places.")
# tmp = llm_dbl(user_prompt = prompts, model = "gpt-4o-mini")
# cbind(x, tmp)

  
# 
# llm_call = function(prompt, system_prompt = "You are a helpful assistant.", model = "gpt-4o-mini") {
#   assertthat::is.scalar(system_prompt)
#   assertthat::is.scalar(model)
#   prompt |>
#     map_chr(\(x) {
#       tmp =
#         openai::create_chat_completion(
#           model = model,
#           messages = list(
#             list(role = "system", content = system_prompt),
#             list(role = "user", content = x)
#           )
#         )
#       tmp$choices$message.content[[1]]
#     })
# }




# fmt2 = list(
#   type = "json_schema",
#   json_schema = list(
#     strict = TRUE,
#     name = "isw_summary",
#     schema = list(
#       type = "object",
#       properties = list(
#         report_date = list(type = "string"),
#         engagements = list(
#           type = "array",
#           items = list(
#             `$ref` = "#/$defs/engagement"
#           )
#         ),
#       ),
#       `$defs` = list(
#         engagement = list(
#           type = "object",
#           properties = list(
#             killed_summary = list(type = "string"),
#             cumulative = list(type = "boolean"),
#             killed_count_description = list(type = c("string", "null")),
#             killed_count = list(type = "number")
#           ),
#           required = c("killed_summary", "cumulative", "killed_count_description", "killed_count"),
#           additionalProperties = FALSE
#         )
#       ),
#       required = c("report_date", "engagements"),
#       additionalProperties = FALSE
#     )
#   )
# )


get_response_format = function(array = FALSE, ...) {
  args = list(...)
  properties = args |> map(\(x) list(type = x))
  
  if (array) {
    list(
      type = "json_schema",
      json_schema = list(
        strict = TRUE,
        name = "custom_schema",
        schema = list(
          type = "object",
          properties = list(
            response = list(
              type = "array",
              items = list(
                type = "object",
                properties = properties,
                required = if (length(properties) == 1) list(names(properties)) else names(properties),
                additionalProperties = FALSE
              )
            )
          ),
          required = list("response"),
          additionalProperties = FALSE
        )
      )
    )
  } else {
    list(
      type = "json_schema",
      json_schema = list(
        strict = TRUE,
        name = "custom_schema",
        schema = list(
          type = "object",
          properties = properties,
          required = if (length(properties) == 1) list(names(properties)) else names(properties),
          additionalProperties = FALSE
        )
      )
    )
  }
}

