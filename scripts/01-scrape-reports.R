library(magrittr)
library(stringr)
library(lubridate)
library(xml2)
library(dplyr)

source("scripts/utils.R")



# 1. Scrape Reports ---------------------------------------------------

urls = readLines("data/incident_urls_20240816.txt")

# Read HTML
lst = list()
for (i in seq_along(urls)) {
  print(urls[i])
  lst[[i]] = safe_read_html(urls[i])
}

# Parse HTML
victims_by_incident = lst %>% lapply(function(x) parse_incident(x$result))

df_victims = 
  bind_rows(victims_by_incident) %>% 
  arrange(incident_date, incident_id, victim_id) %>%
  mutate_if(is.character, function(x) str_replace_all(x, ',', '..')) %>%
  mutate_if(is.character, function(x) str_replace_all(x, '[“”"]', '`'))



# 2. Cleaning ---------------------------------------------------------

df_victims = 
  df_victims %>% 
  mutate(
    
    .name_en = 
      name_en %>% 
      str_remove_all("[\u0600-\u06ff]") %>% 
      str_remove_all("^[ .,]+|[ .,]+$") %>% 
      str_squish,
    
    .name_ar = 
      name_ar %>% 
      str_remove_all("[A-Za-z]") %>% 
      str_remove_all("^[ .,]+|[ .,]+$") %>%
      str_squish,

    .age_years = as.numeric(str_extract(age, "[0-9.]+(?=[^0-9]*years old)")),
    .age_group = case_when(
      age == "Adult" ~ "adult",
      age == "Child" ~ "child",
      .age_years >= 18 ~ "adult",
      .age_years < 18 ~ "child",
      TRUE ~ NA_character_
    ),
    
    .gender = case_when(
      gender == "male" ~ "m",
      gender == "female" ~ "f",
      TRUE ~ NA_character_
    ),
    
    .pregnant = case_when(
      pregnant == "pregnant" ~ 1L,
      TRUE ~ 0L
    ),
    
    .killed_or_injured = case_when(
      killed_or_injured == "killed" ~ "k",
      killed_or_injured == "injured" ~ "i",
      TRUE ~ NA_character_
    ),
    
    .moh_id = str_extract(moh_id, "(?<=Matched to MoH ID [^0-9]{0,20})[0-9]{8,9}"),
    .moh_id = case_when(
      is.na(.moh_id) & str_detect(notes, "[0-9]{8,9}") ~ str_extract(notes, "[0-9]{8,9}"),
      TRUE ~ .moh_id
    ),
    
  )

out = 
  df_victims %>%
  select(
    incident_id,
    incident_date,
    family_id,
    victim_id,
    name_en = .name_en,
    name_ar = .name_ar,
    age_years = .age_years,
    age_group = .age_group,
    gender = .gender,
    pregnant = .pregnant,
    killed_or_injured = .killed_or_injured,
    moh_id = .moh_id
  )
  


# 3. Output -----------------------------------------------------------

write.csv(out, "data/victims.csv", row.names = FALSE, na = "", quote = FALSE)


# Check

tmp = read.csv(
  "data/victims.csv", na = "",
  colClasses = { x = rep("character", 12); x[2] = "Date"; x[c(3,4,10)] = "integer"; x[7] = "numeric"; x }
)

all.equal(out, tmp)
