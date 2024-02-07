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
         punteggio_motivazione, discrezionale, punteggio_totale) %>% 
  left_join(., x, by = join_by(matricola), keep = FALSE)

# Write output files
## Assigned locations file - Esiti worksheet
## File with all notes on why students did not get assigned a location - Note worksheet
x <- df_locations_validated_all %>% 
  select(cognome, nome, matricola, sede_ospitante, nome_accordo, language_requirement:isced_requirement)
worksheets <- list(esiti = df_esito_selezioni, note = x)
write_xlsx(worksheets, path = "output/esito_selezioni.xlsx", )

## Remaining locations file for those that have not been completely filled out
write_xlsx(df_locations_remaining, path = "output/locations_availability.xlsx")


##
df_locations_remaining %>% glimpse()
df_locations_remaining %>% View()
df_esito_selezioni %>% glimpse()
df_esito_selezioni %>% View()
df_locations_validated_all %>% glimpse()
df_dd_assignment_notes %>% glimpse()





# COGNOME	NOME	MATRICOLA	CORSO DI STUDI	TIPO CORSO DI STUDI	
# PUNTEGGIO NORMALIZZATO A 100	PUNTEGGIO MOTIVAZIONE (max 50 punti)	
# PUNTEGGIO TOTALE	
# POSIZIONE GRADUATORIA	
# Paese dell'istituzione scelta	Istituzione scelta	Codice Erasmus Istituzione	Nome dell'accordo	
# NOTE

