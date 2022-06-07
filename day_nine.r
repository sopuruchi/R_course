#install.packages('janitor')
library('readr')
library('janitor')
library('dplyr')

path <- "C:/Users/TRAINER/Desktop/finnstats/FinData.csv"
df <- read_csv(path)
View(df)

clean_df <- clean_names(df)
col_names <- colnames(clean_df) 
print(col_names)
View(clean_df)

for (i in col_names){
  summ <- tabyl(clean_df, i)
  print(summ)
}

for (i in col_names){
  formatt <- clean_df %>% tabyl(i) %>% adorn_pct_formatting(digits = 2, affix_sign = T)
  print(formatt)
}

for (i in col_names){
  pct_format <- clean_df %>% tabyl(employee_status, full_time,i) %>% adorn_totals()
  print(pct_format)
}

clean_df %>% tabyl(employee_status, full_time) %>% adorn_totals()

clean_df %>% tabyl(employee_status, full_time) %>% adorn_totals(where = c("row", "col"))

clean_df %>% tabyl(employee_status, full_time, subject)

new_clean <- clean_df %>% remove_empty(which = c("rows"))
clean_new <- new_clean %>% remove_empty(which = c("cols"))
View(clean_new)

clean_new %>% get_dupes(first_name)
dat <- clean_new %>% get_dupes(first_name, certification_10)
View(dat)

filtered_one <- clean_new %>% filter(!grepl('/', hire_date))
View(filtered_one)

filtered_two <- clean_new %>% filter(grepl('/', hire_date))
View(filtered_two)
