# Import libraries
library(writexl)

source("function_definitions.R")

# Read needed data objects
df_student_personal %>% glimpse()
df_assignment %>% glimpse()
df_locations_validated %>% glimpse()
df_locations_available %>% glimpse()
df_locations_remaining %>% glimpse() 


# Creation of transitory data frames
x <- 
  df_locations_available %>% 
  select(paese, sede_ospitante, isced, nome_accordo) %>% 
  left_join(df_assignment,., by = c("assigned_locations" = "nome_accordo"), keep = TRUE, relationship = "many-to-many")
  

# Create df_esito_selezioni
df_esito_selezioni <- 
  df_student_personal %>% 
  select(cognome, nome, matricola, corso_di_studi, tipo_corso_di_studi, punteggio_normalizzato_a_100) %>% 
  left_join(., x, by = join_by(matricola), keep = FALSE, )

# Write output files
df_locations_remaining %>% glimpse()
df_locations_remaining %>% View()
df_esito_selezioni %>% glimpse()
df_esito_selezioni %>% View()

# COGNOME	NOME	MATRICOLA	CORSO DI STUDI	TIPO CORSO DI STUDI	
# PUNTEGGIO NORMALIZZATO A 100	PUNTEGGIO MOTIVAZIONE (max 50 punti)	
# PUNTEGGIO TOTALE	
# POSIZIONE GRADUATORIA	
# Paese dell'istituzione scelta	Istituzione scelta	Codice Erasmus Istituzione	Nome dell'accordo	
# NOTE

