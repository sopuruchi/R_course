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

num_date <- as.numeric(filtered_one$hire_date)
clean_date <- excel_numeric_to_date(num_date)
print(clean_date)

clean_one <- filtered_one %>% mutate(hire_date = clean_date)
View(clean_one)

new_dt <- mdy(filtered_two$hire_date)
clean_two <- filtered_two %>% mutate(hire_date = new_dt)
View(clean_two)

new_df <- rbind(clean_one, clean_two)
View(new_df)

for (i in colnames(new_df)){
  k <- sum(is.na(new_df[i]))
  p <- paste(i, "=>", k, sep = " ")
  print(p)
}

new_df$subject[is.na(new_df$subject)] <- "English"
new_df$certification_9[is.na(new_df$certification_9)] <- "Physical ed"
new_df$certification_10[is.na(new_df$certification_10)] <- "Physics"

print(table(new_df$subject))
print(table(new_df$certification_9))
print(table(new_df$certification_10))

print(unique(new_df$subject))

new_df$subject[new_df$subject == "#REF!"] <- "English"
View(new_df)

filtered_percent <- new_df %>% filter(!grepl('%', percent_allocated))
filtered_percent2 <- new_df %>% filter(grepl('%', percent_allocated))
View(filtered_percent2)


new_col <- c()
for (i in filtered_percent2$percent_allocated){
  b <- substr(i, 1, nchar(i) - 1)
  c <- as.numeric(b)
  new_col <- c(new_col, c)
}

filtered_percent2 <- filtered_percent2 %>% mutate(percent_allocated = new_col)
View(filtered_percent2)

mean_percent2 <- mean(filtered_percent2$percent_allocated)
filtered_percent$percent_allocated[filtered_percent$percent_allocated == "#N/A"] <- mean_percent2
View(filtered_percent)

new_percent <- rbind(filtered_percent, filtered_percent2)
View(new_percent)

