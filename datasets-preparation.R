# Import libraries
library(tidyverse)
library(janitor)
library(readxl)
library(dplyr)
library(gtools)

source("function_definitions.R")

### Data Ingestion
### Read files in and change all variable names
df_student_double_degree <- read_xlsx(path = "./data/dd-candidati.xlsx", sheet = "Sheet1", range = cell_cols("A:E")) %>% 
  rename(matricola = MATRICOLA, cognome = Last_Name, nome = First_Name, 
         master = Master, codice_erasmus_sedi = CODICE_ERASMUS_SEDI)

df_locations_available <- read_xlsx(path = "./data/sedi ERASTU 23-24 - Scienze Economiche e Aziendali-1.xlsx", 
                 sheet = "vereinbarungen", range = cell_cols("A:P")) %>% 
  rename(area_erasmus = `AREA DI STUDI ERASMUS`, nome_accordo = `NOME DELL'ACCORDO`,
  sede_ospitante = `SEDE OSPITANTE (codice Erasmus e nome)`, paese = PAESE, isced = ISCED,
  n_posti = `N. POSTI`, durata = `DURATA INDICATIVA PERIODO (mesi)`,
  livelli_di_studio = `LIVELLI DI STUDIO AMMISSIBILI`, corsi_di_studio = `CORSI DI STUDIO AMMISSIBILI`,
  note = NOTE, lingua_1 = `LINGUA DI ISTRUZIONE 1`, livello_lingua_1 = `LIVELLO LINGUA DI ISTRUZIONE 1`,
  lingua_2 = `LINGUA DI ISTRUZIONE 2`, livello_lingua_2 = `LIVELLO LINGUA DI ISTRUZIONE 2`,
  certificazione_linguistica = `CERTIFICAZIONE LINGUISTICA`, 
  scadenza_urgente = `EVENTUALE SCADENZA APPLICATION URGENTE 1° SEMESTRE`)

df_student_personal <- read_xlsx(path = "./data/selezioni ERASTUDIO 23-24 - SCIENZE ECONOMICHE E AZIENDALI.xlsx",
                 sheet = "Dati personali e carriera", range = cell_cols("A:N")) %>% 
  rename(cognome = COGNOME, nome = NOME, matricola = MATRICOLA, email_di_ateneo = `EMAIL DI ATENEO`, 
         corso_di_studi = `CORSO DI STUDI`, tipo_corso_di_studi = `TIPO CORSO DI STUDI`, 
         anno_di_corso = `ANNO DI CORSO`, tipo_di_iscrizione = `TIPO DI ISCRIZIONE`, 
         voto_laurea_precedente = `VOTO LAUREA PRECEDENTE`, cfu_conseguiti = `CFU CONSEGUITI`, 
         media_pesata = `MEDIA PESATA`, punteggio_di_merito = `PUNTEGGIO DI MERITO`, 
         punteggio_normalizzato_a_100 = `PUNTEGGIO NORMALIZZATO A 100`, note_personal = NOTE)

df_student_destinations <- read_xlsx(path = "./data/selezioni ERASTUDIO 23-24 - SCIENZE ECONOMICHE E AZIENDALI.xlsx",
                               sheet = "Destinazioni", range = cell_cols("A:V")) %>% 
  rename(cognome = COGNOME, nome = NOME, matricola = MATRICOLA, corso_di_studi = `CORSO DI STUDI`, 
         paese_1 = `Paese dell'istituzione scelta (1)`, istituzione_1 = `Istituzione scelta (1)`, 
         codiceerasmus_1 = `Codice Erasmus Istituzione (1)`, nomeaccordo_1 = `Nome dell'accordo (1)`, 
         numeroposti_1 = `Numero di posti per l'accordo (1)`, numeromesi_1 = `Numero di mesi (1)`, 
         paese_2 = `Paese dell'istituzione scelta (2)`, istituzione_2 = `Istituzione scelta (2)`, 
         codiceerasmus_2 = `Codice Erasmus Istituzione (2)`, nomeaccordo_2 = `Nome dell'accordo (2)`, 
         numeroposti_2 = `Numero di posti per l'accordo (2)`, numeromesi_2 = `Numero di mesi (2)`, 
         paese_3 = `Paese dell'istituzione scelta (3)`, istituzione_3 = `Istituzione scelta (3)`, 
         codiceerasmus_3 = `Codice Erasmus Istituzione (3)`, nomeaccordo_3 = `Nome dell'accordo (3)`, 
         numeroposti_3 = `Numero di posti per l'accordo (3)`, numeromesi_3 = `Numero di mesi (3)`)

df_student_language <- read_xlsx(path = "./data/selezioni ERASTUDIO 23-24 - SCIENZE ECONOMICHE E AZIENDALI.xlsx",
                                     sheet = "Competenze linguistiche", range = cell_cols("A:H")) %>% 
  rename(cognome = COGNOME, nome = NOME, matricola = MATRICOLA, corso_studi = `CORSO DI STUDI`, 
         tipo_corso_studi = `TIPO CORSO DI STUDI`, lingua = LINGUA, livello = LIVELLO, note_lingua = NOTE) %>% 
  select(-cognome, -nome, - corso_studi, -tipo_corso_studi)

### Data Preparation
### Create file for analyzing location and program validation
df_student_destination_clean <- 
  df_student_destinations %>% 
  select(-numeroposti_1, -numeromesi_1, -numeroposti_2, -numeromesi_2, -numeroposti_3, -numeromesi_3) %>% 
  pivot_longer(
    cols = starts_with(c("paese", "istituzione", "codiceerasmus", "nomeaccordo")),
    names_to = c(".value", "choice_number"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>% 
  left_join(., df_student_language %>% 
              pivot_wider(names_from = lingua, values_from = c(livello, note_lingua)),
            by = join_by(matricola))

df_locations_available_clean <- 
  df_locations_available %>% select(-area_erasmus, -paese)
  
df_locations_complete <- left_join(df_student_destination_clean, df_locations_available_clean, 
            by = c("nomeaccordo" = "nome_accordo"), keep = TRUE, relationship = "many-to-many") %>% 
  select(-cognome, -nome, -corso_di_studi) %>% 
  left_join(df_student_personal, ., by = join_by(matricola), keep = FALSE, relationship = "many-to-many")


### Validate Language Requirements and provide appropriate notes to df_locations_complete
### Create appropriate file with language mappings and scores on language levels
l <-
  df_locations_complete %>% 
  select(cognome, nome, matricola, nomeaccordo, nome_accordo, lingua_1, livello_lingua_1, lingua_2, livello_lingua_2) %>% 
  left_join(., df_student_language, by = join_by(matricola), relationship = "many-to-many") %>% 
  mutate(language = map_language_name(lingua)) %>%
  mutate_at(vars(starts_with("livello")), ~map_language_levels(.)) # Replace the values of livello_LANGUAGE variables
  
### Evaluate language requierments for each student and retunr list of student/destination they qualify for
l <- 
  l %>%   
  mutate(language_requirement = if_else(
    lingua_1 == language & livello_lingua_1 <= livello |
    lingua_2 == language & livello_lingua_2 <= livello, 
    "Language requirement met successfully", "Language requirement not met")) %>% 
  distinct(across(-lingua_1:-language)) %>%
  filter(language_requirement == "Language requirement met successfully") %>% 
  select(matricola, nomeaccordo, language_requirement)

### Evaluate program requirements at the destination
#d <-
  df_locations_complete %>%
    select(cognome:tipo_di_iscrizione, choice_number:nomeaccordo, nome_accordo:corsi_di_studio) ### WORKING ON THIS ONE!

### Create file with student-location selections completely validate
### and appropriate notes for failing each test of validation
df_locations_validated <- df_locations_complete

### Merge language requirement results back into df_locations_validated
df_locations_validated <- left_join(df_locations_validated, l, 
                                   by = c("matricola" = "matricola", "nomeaccordo" = "nomeaccordo")) %>% 
  mutate(language_requirement = replace(language_requirement, is.na(language_requirement), "Language requirement not met"))

### Merge program requirement results back into df_locations_validated


### Matching Algorithm Call
source("matching-algos.R")

