### Function Definitions File

# Data cleaning and validation functions
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

# Matching and assignment functions
# Function to check and update posti_disponibili
assign_find_match <- function(df, choice) {
  print(choice)
  if (choice %in% df$nome_accordo) {
    row <- df %>% filter(nome_accordo == choice) %>% distinct()
    print(row)
    if (row$posti_disponibili > 0) {
      df <- 
        df %>%
        mutate(posti_disponibili = ifelse(nome_accordo == choice, posti_disponibili - 1, posti_disponibili))
      print(df %>% filter(nome_accordo == choice))
      return(list(udf = df, a = choice))  # Return the updated df_locations_slots dataframe and the assigned value of "choice"
    }
  }
  return(list(udf = df, a = NULL))  # Return no match
}