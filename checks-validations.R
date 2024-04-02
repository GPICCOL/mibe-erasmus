aaa <- read_xlsx(path = "./output/esito_selezioni.xlsx", sheet = "esiti", range = cell_cols("A:O"))
ddd <- read_xlsx(path = "./data/dd_candidati2024.xlsx", sheet = "sheet", range = cell_cols("A:F")) %>% 
  rename(matricola = MATRICOLA, cognome = Last_Name, nome = First_Name, 
         master = Master, codice_erasmus_sedi = CODICE_ERASMUS_SEDI, nome_accordo = MESI) %>% 
  mutate(matricola = as.character(matricola), 
         nome_accordo = str_replace_all(nome_accordo, "\\s{2,}", " ")) 

aaa <- aaa %>% select(matricola, cognome, nome, assigned_locations)

fff <- left_join(ddd, aaa, 
          by = c("matricola" = "matricola"), keep = TRUE) %>%
  mutate(controllo = ifelse(nome_accordo == assigned_locations, "Erasmus same as DD", "Not the same"))

write_xlsx(fff, path = "output/controlli.xlsx")
