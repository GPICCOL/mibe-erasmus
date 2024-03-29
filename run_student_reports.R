library(tidyverse)
library(readxl)

# Read file with results to extract matricola of students to report on
df <- read_xlsx(path = "./output/esito_selezioni.xlsx",
                                 sheet = "esiti", range = cell_cols("A:D"))

# Get variables you need
filename <- df %>% 
  mutate(fn = paste(cognome, nome, sep = "_")) %>% pull() %>% 
  tolower()

matr <- df %>% 
  select(matricola) %>% pull()

# filename <- filename[1:3]
# matr <- matr[1:3]

filename <- filename
matr <- matr


# Loop through students and render the R Markdown file
i = 0
for (m in matr) {
  print(i)
  
  i = i + 1
  
  print(filename[i])
  print(m)
  
  rmarkdown::render(
    input = "student_report.Rmd", 
    output_file = paste0("./reports/", filename[i], ".pdf"),
    params = list(p_student_number = m)
  )
}


