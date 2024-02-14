# Import libraries
source("function_definitions.R")


### Read needed data objects
# At this stage I don't have data objects, I just keep the dfs in memory

### Here we generate the list of student and valid locations by eliminating all the destinations
### students selected but they do not qualify for. This includes language requirements, 
### level and isced requirements, as well as Erasmus destinations not available to Double
### Degree students who need to go to the assigned Double Degree location
df_locations_validated <- 
  df_locations_validated_all %>% 
  filter(language_requirement == "Language requirement met successfully" &
           level_requirement == "Level requirement met successfully" &
           isced_requirement == "ISCED requirement met successfully") %>%
  filter(!double_degree_notes == "Student already assigned to different Double Degree destination" | is.na(double_degree_notes))

### Here we override the "discrezionale" if needed 
### Read a file with matricola and discrezionale and replace values where needed, all others stay at zero


### Data preparation for the matching algo
### First rank students and make a ranked students vector to use later
### Students are first ranked by total points, ties go to the lowest matricola (seniority rule)
# ranked_students <-c(512786, 518361, 512799, 512787, 515499) ## Used for testing a subset
df_student_ranked <- 
  df_locations_validated %>%
  select(cognome:matricola, punteggio_normalizzato_a_100, punteggio_motivazione:discrezionale) %>% 
  mutate(matricola = as.integer(matricola)) %>% 
  mutate(punteggio_totale = punteggio_normalizzato_a_100 + punteggio_motivazione + discrezionale) %>% 
  arrange(desc(punteggio_totale), matricola) %>% 
  distinct() %>% 
  mutate(matricola = as.character(matricola)) %>% 
  mutate(posizione_graduatoria = row_number())
  
ranked_students <- 
  df_student_ranked %>% 
  select(matricola) %>% 
  pull() %>% 
  as.numeric()

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
updated_df <- df_locations_slots
df_assignment <- tibble(matricola = numeric(), assigned_locations = character())

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
df_student_personal_for_output <- df_student_ranked %>% 
  select(matricola, punteggio_motivazione, discrezionale, punteggio_totale, posizione_graduatoria) %>% 
  left_join(df_student_personal, ., by = join_by(matricola), keep = FALSE)
  
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
# Remove unnecessary objects
objects_to_keep <- c("df_locations_validated_all", "df_dd_assignment_notes", "df_locations_remaining", 
                     "df_assignment", "df_student_personal_for_output", "df_locations_available",
                     "df_dd_assignment_notes")
all_objects <- ls()
rm(list = setdiff(all_objects, objects_to_keep))
rm(all_objects)

source("output_generation.R")

