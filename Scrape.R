library(rvest)
library(tidyverse)

url <- "https://broncosports.com/sports/football/roster"


# Get Roster -------------------------------------------------------------

tables <- read_html(url) |> html_elements("table") |> html_table()

roster <- tables[[1]] |>
  filter(`Full Name` != "Skip Ad") |>
  rename("Number" = `#`) |>
  mutate(Day = row_number() - 1, Number = as.numeric(Number)) |>
  select(Day, everything())


# All possible days 0-100
all_days <- 0:100

# Identify duplicated numbers (second+ occurrences)
roster_assigned <- roster |>
  arrange(Number) |>
  mutate(is_dup = duplicated(Number))

# Days already claimed by non-duplicates
claimed <- roster_assigned |>
  filter(!is_dup) |>
  pull(Number)

# Available days (not claimed by any unique number)
available <- sort(setdiff(all_days, claimed))

# Assign days to duplicates in order of their Number (lowest first)
dup_rows <- which(roster_assigned$is_dup)
roster_assigned$Day[dup_rows] <- available[seq_along(dup_rows)]

# Assign Day = Number for non-duplicates, clean up
roster <- roster_assigned |>
  mutate(Day = if_else(!is_dup, Number, Day)) |>
  select(-is_dup) |>
  arrange(Day)


# Get Player Links -------------------------------------------------------

page <- read_html(url)

# Extract all player links from the roster table
player_links <- page |>
  html_element("table") |>
  html_elements("a") |>
  (\(x) {
    tibble(
      name = html_text2(x),
      href = html_attr(x, "href")
    )
  })() |>
  filter(str_detect(href, "/sports/football/roster/")) |>
  mutate(url = paste0("https://broncosports.com", href)) |>
  select(name, url)


roster <- roster |>
  left_join(player_links, join_by(`Full Name` == name))


# Add HTML Hyperlinks ----------------------------------------------------

roster <- roster |>
  mutate(
    `Full Name` = paste0('=HYPERLINK("', url, '","', `Full Name`, '")'),
    Twitter = if_else(
      Twitter != "" & !is.na(Twitter),
      paste0('=HYPERLINK("https://twitter.com/', Twitter, '","@', Twitter, '")'),
      Twitter
    ),
    Instagram = if_else(
      Instagram != "" & !is.na(Instagram),
      paste0('=HYPERLINK("https://instagram.com/', Instagram, '","@', Instagram, '")'),
      Instagram
    )
  ) |> 
  select(-url)



# Export Sheet -----------------------------------------------------------

write.csv(roster, "roster2026.csv")
