


# Import libraries
library(tidylibrary(janitor)
library(readxl)
library(dplyr)
library(gtools)

# Read the file containing static information
df_students <- read_excel(path = "./data/static/student_static.xlsx", sheet = 1, 
                          range = cell_cols("A:E")) %>% clean_names() %>% 
  mutate(email = tolower(email), supervisor_email = tolower(supervisor_email)) 

df_supervisors <- read_excel(path = "./data/static/supervisor_static.xlsx", sheet = 1, 
                             range = cell_cols("A:D")) %>% clean_names() %>% 
  mutate(email = tolower(email)) 

# Set basic parameters and values
# Define the start date as "Monday, August 7, 2023"
semester_start_date <- ymd("2023-07-31")
week_data <- tibble(
  week = 0:20,  # Sequence of week numbers from 0 to 20
  day = format(semester_start_date + weeks(0:20), "%A, %B %d, %Y")  # Calculate Monday dates
)

certification_teaching_hours <- tibble(
  program = c("PK3", "Secondary Holmes", "Health & PE", "GT Math & Science", 
              "GT Humanities", "Elementary", "Dual Cert", "Elem Holmes"),
  `Required Hours` = c(180, 180, 120, 180, 130, 180, 120, 180)
)

# Prepare dataframes
df_static <- left_join(df_students, df_supervisors, by = c("supervisor_email" = "email"))

file_list <- list.files(path = "./data/weeks/", pattern = "^week[[:digit:]]{1,2}.xlsx") %>% 
  paste0("./data/weeks/", .) %>% 
  mixedsort()
df_weeks <- lapply(file_list, read_excel, range = cell_cols("A:E"))

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




