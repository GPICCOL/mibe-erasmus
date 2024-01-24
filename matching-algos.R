# Import libraries
source("function_definitions.R")

### Read needed data objects

### Here we generate the list of student and valid locations by eliminating all the destinations
### students selected but they do not qualify for
df_locations_validated <- 
  df_locations_validated_all %>% 
  filter(language_requirement == "Language requirement met successfully" & 
           level_requirement == "Level requirement met successfully" &
           isced_requirement == "ISCED requirement met successfully")


# Prepare data for the matching algo
df_student_ranked <- 
  df_locations_validated %>%
  select(cognome:matricola, punteggio_normalizzato_a_100) %>% 
  arrange(desc(punteggio_normalizzato_a_100)) %>% 
  distinct() %>% 
  mutate(posizione_graduatoria = row_number())

df_student_choices <- 
  df_locations_validated %>% 
  select(matricola, nomeaccordo, choice_number) %>% 
  arrange(matricola, choice_number)

df_locations_slots <- 
  df_locations_available_clean %>% 
  select(nome_accordo, n_posti) %>% 
  mutate(posti_disponibili = n_posti)


# Assignment loop going through students by rank and destinations by order of preference
student_assignment <- NULL
updated_df <- df_locations_slots %>% 
  filter(!(nome_accordo == "D  DRESDEN02_0410_5months" & n_posti == 2))
df_assignment <- tibble(matricola = numeric(), assigned_locations = character())
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
  df_assignment <- bind_rows(df_assignment , tibble(matricola = m, assigned_locations = student_assignment))
}

# Creation of dataframes to pass for output
df_assignment <- df_assignment %>%
  mutate(matricola = as.character(matricola))

df_assignment <- 
  df_student_ranked %>% 
  select(matricola, posizione_graduatoria) %>% 
  left_join(., df_assignment, by = join_by(matricola), keep = FALSE)

df_locations_remaining <- 
  df_locations_available %>% 
  select(paese, sede_ospitante, isced, nome_accordo) %>% 
  left_join(., updated_df, by = c("nome_accordo" = "nome_accordo"), keep = FALSE, relationship = "many-to-many")

### Output Generation Call
source("output_generation.R")


# assign_students(512786)
# 
# df_student_choices %>% filter(matricola == 516028)
# 
# 
# assign_students <- function(location_df, students_df, st_matricola) {
#   #Initialize variables
#   student_assignment <- NULL
# }
# 

