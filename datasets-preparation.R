# Import libraries
library(tidyverse)
library(janitor)
library(readxl)
library(dplyr)
library(gtools)

source("function_definitions.R")

### Data Ingestion
### Read files in and change all variable names

# ISCED Code Conversion Table
df_isced <- read_xlsx(path = "./data/tabella_conversione-ISCED_CODE.xlsx", sheet = "Sheet1", range = cell_cols("A1:B8")) %>% 
  rename(isced_tabella_conversione = isced)
  
# List of students who have won the double degree assignment
df_student_double_degree <- read_xlsx(path = "./data/dd-candidati.xlsx", sheet = "Sheet1", range = cell_cols("A:E")) %>% 
  rename(matricola = MATRICOLA, cognome = Last_Name, nome = First_Name, 
         master = Master, codice_erasmus_sedi = CODICE_ERASMUS_SEDI) %>% 
  mutate(matricola = as.character(matricola))

# List of available locations and their characteristics
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

# List of participating students and their personal date
df_student_personal <- read_xlsx(path = "./data/selezioni ERASTUDIO 23-24 - SCIENZE ECONOMICHE E AZIENDALI.xlsx",
                 sheet = "Dati personali e carriera", range = cell_cols("A:N")) %>% 
  rename(cognome = COGNOME, nome = NOME, matricola = MATRICOLA, email_di_ateneo = `EMAIL DI ATENEO`, 
         corso_di_studi = `CORSO DI STUDI`, tipo_corso_di_studi = `TIPO CORSO DI STUDI`, 
         anno_di_corso = `ANNO DI CORSO`, tipo_di_iscrizione = `TIPO DI ISCRIZIONE`, 
         voto_laurea_precedente = `VOTO LAUREA PRECEDENTE`, cfu_conseguiti = `CFU CONSEGUITI`, 
         media_pesata = `MEDIA PESATA`, punteggio_di_merito = `PUNTEGGIO DI MERITO`, 
         punteggio_normalizzato_a_100 = `PUNTEGGIO NORMALIZZATO A 100`, note_personal = NOTE)

# List of students destination selection
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

# List of students language competence
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

### Evaluate Double Degree students to eliminate those who are going to destinations *not erasmus*
### To eliminate the destinations that are not congruent with the assigned double degree destination
### and produce appropriate messages
### Generate list of students in double degree
unique_erasmus_locations <- 
  df_locations_available_clean %>% 
  mutate(unique_erasmus_locations = str_extract(sede_ospitante, "^[^-]+")) %>% 
  mutate(unique_erasmus_locations = str_trim(unique_erasmus_locations)) %>% 
  select(unique_erasmus_locations) %>% 
  pull()
  

df_dd_assignment_notes <- 
  df_student_double_degree %>% 
  mutate(double_degree_notes = if_else(codice_erasmus_sedi %in% unique_erasmus_locations, 
                                       paste("Double Degree Student, Erasmus Location: ", codice_erasmus_sedi),
                                       "Student assigned to location not in Erasmus list")) %>%
  mutate(double_degree_notes = if_else(codice_erasmus_sedi == "no admitted", "Student not admitted to Double Degree Program", double_degree_notes)) 

dd_students_not_admitted <-
  df_dd_assignment_notes %>% 
  filter(double_degree_notes == "Student not admitted to Double Degree Program") %>% 
  select(matricola) %>% 
  pull()

dd_students_admitted <-
  df_dd_assignment_notes %>% 
  filter(str_starts(double_degree_notes, "Double Degree Student, Erasmus Location")) %>% 
  select(matricola) %>% 
  pull()
  
dd <- 
  df_dd_assignment_notes %>% 
  select(matricola, master, codice_erasmus_sedi, double_degree_notes) %>% 
  left_join(df_locations_complete, ., by = c("matricola" = "matricola", "codiceerasmus" = "codice_erasmus_sedi")) %>% 
  select(cognome:matricola, choice_number:nomeaccordo, master:double_degree_notes) %>% 
  mutate(double_degree_notes = if_else(is.na(double_degree_notes) & matricola %in% dd_students_not_admitted, 
                                       "Double Degree student not admitted to double degree",
                                       double_degree_notes)) %>% 
  mutate(double_degree_notes = if_else(is.na(double_degree_notes) & matricola %in% dd_students_admitted, 
                                       "Student already assigned to different Double Degree destination",
                                       double_degree_notes)) %>% 
  select(matricola, nomeaccordo, double_degree_notes)

### Validate Language Requirements and provide appropriate notes to df_locations_complete
### Create appropriate file with language mappings and scores on language levels
l <-
  df_locations_complete %>% 
  select(cognome, nome, matricola, nomeaccordo, nome_accordo, lingua_1, livello_lingua_1, lingua_2, livello_lingua_2) %>% 
  left_join(., df_student_language, by = join_by(matricola), relationship = "many-to-many") %>% 
  mutate(language = map_language_name(lingua)) %>%
  mutate_at(vars(starts_with("livello")), ~map_language_levels(.)) # Replace the values of livello_LANGUAGE variables
  
### Evaluate language requirements for each student and return list of student/destination
### A student must meet or exeed the minimum language requirement set by the destination
### on at least one of the two languages required
l <- 
  l %>%   
  mutate(language_requirement = if_else(
    lingua_1 == language & livello_lingua_1 <= livello |
    lingua_2 == language & livello_lingua_2 <= livello, 
    "Language requirement met successfully", "Language requirement not met")) %>% 
  distinct(across(-lingua_1:-language)) %>%
    filter(language_requirement == "Language requirement met successfully") %>% 
  select(matricola, nomeaccordo, language_requirement)


### This code implements the following rules
### 1. A student must have chosen a location that offers schooling at the level they are at: triennale or magistrale
### 2. A student must have chosen a location where the ISCED of their "corso di studi" is in the set of ISCEDs the location meets
### 3. The above rule is superseded for students who are in the third year. For those students ISCED congurence is not required.
# Validate if they have the correct livello di studio for each location and the correct ISCED
d <-
  df_locations_complete %>%
    select(cognome:tipo_di_iscrizione, choice_number:nomeaccordo, nome_accordo:isced, livelli_di_studio, corsi_di_studio) %>%
  left_join(., df_isced, by = join_by(corso_di_studi)) %>%
#  left_join(., d, by = join_by(matricola == matricola, codiceerasmus == codice_erasmus_sedi), keep = FALSE, relationship = "many-to-many") %>% 
  mutate(livelli_di_studio_corrected = map_livelli_studio(livelli_di_studio)) %>% 
  mutate(level_requirement = if_else(str_detect(livelli_di_studio_corrected, tipo_corso_di_studi), "Level requirement met successfully", "Level requirement not met")) %>% 
  mutate(isced_clean = str_sub(isced, -4)) %>% 
  mutate(isced_requirement = if_else(str_detect(isced_tabella_conversione, isced_clean), "ISCED requirement met successfully", "ISCED requirement not met")) %>% 
  mutate(isced_requirement = if_else(anno_di_corso == 3, "ISCED requirement met successfully becasue student in third year", isced_requirement)) %>% 
  select(matricola, nomeaccordo, level_requirement, isced_requirement)
  

### Create file with student-location selections validated and appropriate notes for failing each test of validation
### Merge language requirement and ISCED requirements results back into df_locations_complete
### This file will list all students and all the locations they originally selected. For each of this location we have notes
### either attesting that the students fulfills the requirement or that they do not. 
### This file is needed in case students want to know why they did not qualify for a specific location of their choice
df_locations_validated_all <- 
  df_locations_complete %>% 
  mutate(bonus = if_else(matricola %in% dd_students_admitted, 50, 25)) %>% 
  mutate(discrezionale = 0) %>% 
  left_join(., l, by = c("matricola" = "matricola", "nomeaccordo" = "nomeaccordo")) %>% 
  mutate(language_requirement = replace(language_requirement, is.na(language_requirement), "Language requirement not met")) %>% 
  left_join(., d, by = c("matricola" = "matricola", "nomeaccordo" = "nomeaccordo")) %>% 
  left_join(., dd, by = c("matricola" = "matricola", "nomeaccordo" = "nomeaccordo"))


### Matching Algorithm Call
# Remove unnecessary objects
objects_to_keep <- c("df_locations_validated_all", "df_dd_assignment_notes", "df_locations_available_clean", "df_locations_available", "df_student_personal", "df_locations_available")
all_objects <- ls()
rm(list = setdiff(all_objects, objects_to_keep))
rm(all_objects)


# Run next script
source("matching-algos.R")

