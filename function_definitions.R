### Function Definitions File

# Define a function to map text language levels to integers
map_language_levels <- function(x) {
  case_when(
    x == "A1" ~ 1,
    x == "A2" ~ 2,
    x == "B1" ~ 3,
    x == "B2" ~ 4,
    x == "C1" ~ 5,
    x == "C2" ~ 6,
    TRUE ~ NA_integer_ ## CAREFUL, this one does not handle "non A2" cases!
  )
}

# Define a function to translate languages
map_language_name <- function(x) {
  case_when(
    x == "INGLESE" ~ "English",
    x == "TEDESCO" ~ "German",
    x == "FRANCESE" ~ "French",
    x == "SPAGNOLO" ~ "Spanish",
    x == "PORTOGHESE" ~ "Portuguese",
    x == "" ~ "",
    TRUE ~ NA 
  )
}