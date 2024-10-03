library(stringr)
library(lubridate)
library(rvest)
library(xml2)
library(dplyr)
library(jsonlite)
library(openai)
library(purrr)
library(glue)

source("scripts/utils.R")
source("scripts/openai_utils.R")


tmp = readRDS("data/raw_scrape.rds")
lst = tmp |> map(xml2::as_xml_document)

has_belligerents = 
  lst |>
  map_lgl(\(x) {
    tmp = x |>
      html_nodes(".info-main-block [class='meta-list summary']") |>
      html_children() |> 
      html_text() |>
      str_subset("Belligerents") |>
      length()
    tmp > 0
  })

civ_victims = 
  lst[has_belligerents] |>
  map(parse_incident) |>
  map(select, any_of(c("incident_id", "name_en", "age", "gender", "notes"))) |>
  map_chr(jsonlite::toJSON)

summaries = 
  lst[has_belligerents] |>
  map_chr(\(x) {
    x |>
      html_nodes(".info-main-block [class='summary']") |>
      html_text() |>
      str_trim("both") |>
      str_replace_all("\\n\\t+", "\n")
  })

incident_codes = 
  lst[has_belligerents] |>
  map_chr(\(x) x |> html_node(".code > span") |> html_text() |> tolower())

user_prompt = glue(
  "Below is a summary of an incident of alleged civilian harm due to Israeli airstrikes in the context of the 2023 Israel-Hamas war, followed by a list of identified presumed-civilian casualties. Please return information on all identified casualties mentioned in the summary that are alleged and/or confirmed combatants. These may be included in the presumed-civilian list (if militant status was not independently confirmed) or may be excluded (for confirmed militants). Return the information in JSON format similar to the civilian casualties list, with each element of the array containing the following fields:
- incident_id: string; request identifier: {incident_codes}
- name: string; Latinized name
- age: number or null; age in years if available, otherwise null
- notes: string; information from the summary as to why this person is an alleged and/or confirmed militant
- killed_or_injured: string; killed or injured
If there are no casualties described in the summary that are either alleged or confirmed militants, return an empty array. Do not include any individuals that were mentioned but were not among the casualties of the specific incident described.

----
The summary is given below:

{summaries}

----
The list of civilian casualties is given below:

{civ_victims}")

response_format = get_response_format(
  array = TRUE, 
  incident_id = "string",
  name = "string",
  age = c("string", "null"),
  notes = "string",
  killed_or_injured = "string"
)

responses = llm(user_prompt, response_format = response_format, model = "gpt-4o-mini")

df_combatants = 
  responses |>
  map(fromJSON) |> 
  map(\(x) x$response) |>
  bind_rows()




# 5 replications to check consistency
responses_rep5 = replicate(
  5, llm(user_prompt, response_format = response_format, model = "gpt-4o-mini"),
  simplify = FALSE
)

idx_consistent =
  responses_rep5 |> 
  map(\(x) map(x, \(u) fromJSON(u)$response)) |>
  purrr::transpose() |>
  map(\(x) unlist(map(x, nrow))) |>
  map_lgl(\(x) length(unique(x)) == 1)

