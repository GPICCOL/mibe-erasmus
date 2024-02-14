library(tidyverse)
library(readxl)

# Read file with results to extract matricola of students to report on
df <- read_xlsx(path = "./output/esito_selezioni.xlsx",
                                 sheet = "esiti", range = cell_cols("A:D"))

# Get variables you need
filename <- df %>% 
  mutate(fn = paste(cognome, nome, sep = "_")) %>% pull() %>% 
  tolower()

students <- df %>% 
  select(matricola) %>% pull()

filename <- filename[1:3]
students <- students[1:3]


# Loop through students and render the R Markdown file
i = 0
for (s in students) {
  print(i)
  i = i + 1
  rmarkdown::render(
    input = "student_report.Rmd", 
    output_file = paste0("./reports/", filename[i], ".pdf"),
    params = list(p_student = s)
  )
}


