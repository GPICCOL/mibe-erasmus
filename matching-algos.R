# Import libraries


# Read needed data objects
df_locations_validated %>% glimpse()

# Prepare data for the matching algo
df_student_ranked <- 
  df_locations_validated %>%
  select(cognome:matricola, punteggio_normalizzato_a_100) %>% 
  arrange(desc(punteggio_normalizzato_a_100)) %>% 
  distinct()

df_student_choices <- 
  df_locations_validated %>% 
  select(matricola, nomeaccordo, choice_number) %>% 
  arrange(matricola, choice_number)

df_locations_slots <- 
  df_locations_available_clean %>% 
  select(nome_accordo, n_posti) %>% 
  mutate(posti_disponibili = n_posti)

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

student_assignment <- NULL
updated_df <- df_locations_slots
df_assignments <- tibble(matricola = numeric(), assigned_locations = character())
#ranked_students <-c(512786, 518361, 512799, 512787, 515499)
ranked_students <- 
  df_student_ranked %>% 
  select(matricola) %>% 
  pull() %>% 
  as.numeric()

# Loop through students in order of their rank
for (m in ranked_students) {
  
  # Extract the preference of the given student
  choices <- df_locations_validated %>% 
    filter(matricola == m) %>% 
    pull(nomeaccordo)
  
  # Loop through each choice until a valid one is found
  for (choice in choices) {
    assignment_results <- assign_find_match(updated_df, choice)
    student_assignment <- assignment_results$a
    updated_df <- assignment_results$udf
    if (!is.null(student_assignment)) {
      break  # Stop execution if a valid choice is found
    }
    if (is.null(student_assignment)) {
      student_assignment <- "All valid choices were taken - no assignment possible at this time"
  }
  }
  # Append the results to the tibble
  df_assignments <- bind_rows(df_assignments , tibble(matricola = m, assigned_locations = student_assignment))
}


assign_students(512786)

df_student_choices %>% filter(matricola == 516028)


assign_students <- function(location_df, students_df, st_matricola) {
  #Initialize variables
  student_assignment <- NULL
}




processing_week <- function(raw_week) {
  processed_week <- raw_week %>% clean_names() %>% 
    mutate(total_hours = observation_hours + participation_hours + teaching_hours) %>% 
    bind_rows()
  
  processed_week <- left_join(df_static, processed_week, by = c("email" = "username")) %>% 
    mutate(responded_survey = ifelse(!is.na(observation_hours), 1, 0))
  
  return(processed_week)
}

df_weeks_complete <- lapply(df_weeks, processing_week)

df <- map2(df_weeks_complete, seq_along(df_weeks_complete), ~mutate(.x, week = .y -1)) %>% 
  map(select, week, everything()) %>% 
  bind_rows() %>%
  rename(`Last Name` = last_name.x,
         `First Name` = first_name.x,
         Program = program.x,
         Observations = observation_hours,
         Participation = participation_hours,
         Teaching = teaching_hours,
         Supervisor = last_name.y,
         `Family Engagement` = family_engagement) %>% 
  mutate(Supervisor_name = paste(Supervisor, first_name.y, sep = ", ")) %>% 
  left_join(., week_data, by = "week") %>% # attach the date
  left_join(., certification_teaching_hours, by = c("Program" = "program")) # attach the program

# Save the df objects as an RDS file
saveRDS(df, file = "df_weeks_complete.rds")

#call reports programmatically
#




