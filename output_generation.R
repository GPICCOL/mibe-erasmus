# Import libraries
library(writexl)

source("function_definitions.R")

# Creation of transitory data frames
x <- 
  df_locations_available %>% 
  select(paese, sede_ospitante, isced, nome_accordo) %>% 
  left_join(df_assignment,., by = c("assigned_locations" = "nome_accordo"), keep = TRUE, relationship = "many-to-many")
  

# Create df_esito_selezioni
df_esito_selezioni <- 
  df_student_personal_for_output %>% 
  select(cognome, nome, matricola, corso_di_studi, tipo_corso_di_studi, punteggio_normalizzato_a_100, 
         punteggio_motivazione, punteggio_totale, posizione_graduatoria, tipologia_assegnazione) %>% 
  left_join(., x, by = join_by(matricola), keep = FALSE)

# Write output files
## Assigned locations file - Esiti worksheet
## File with all notes on why students did not get assigned a location - Note worksheet
x <- df_locations_validated_all %>% 
  select(cognome, nome, matricola, choice_number, sede_ospitante, nome_accordo, language_requirement:double_degree_notes)
worksheets <- list(esiti = df_esito_selezioni, note = x)

## Add a sheet for students who did not enter the ranking for any reason, with notes about them
nr <- df_esito_selezioni %>% 
  filter(is.na(posizione_graduatoria) | posizione_graduatoria == "") %>% 
  select(matricola) %>% 
  pull()
x <- df_locations_validated_all %>% 
  select(cognome, nome, matricola, sede_ospitante, nome_accordo, language_requirement:double_degree_notes) %>% 
  filter(matricola %in% nr)

worksheets <- list_modify(worksheets, `students not ranked` = x)

## Write file with esiti, note and list of non-ranked students
write_xlsx(worksheets, path = "output/esito_selezioni.xlsx", )

## Remaining locations file for those that have not been completely filled out
write_xlsx(df_locations_remaining, path = "output/locations_availability.xlsx")

## Notes about double degree students
write_xlsx(df_dd_assignment_notes, path = "output/double_degree_notes.xlsx")

print("All done, no errors")
##
# df_locations_remaining %>% glimpse()
# df_esito_selezioni %>% glimpse()
# x %>% glimpse()

