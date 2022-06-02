install.packages('readr')

library('readr')
path <- "C:/Users/TRAINER/Downloads/diabetes.csv"
df <- read_csv(path)
View(df)

cols <- colnames(df)
print(cols)

print(nrow(df))
print(ncol(df))
a_val <- df$Age >= 50
b_val <- df[a_val,]
print(b_val)
View(b_val)

d_val <- which(a_val)
print(d_val)

age_dependency <- function(age_val){
  a_val <- df$Age >= age_val
  b_val <- df[a_val,]
  print(b_val)
}

age_dependency(25)

age_outcome_dependency <- function(age, outcome_val){
  age_outcome <- df$Age > age & df$Outcome == outcome_val
  a_out <- df[age_outcome,]
  print(a_out)
  View(a_out)
}

age_outcome_dependency(50, 1)

a.mean_val <- function(outcome_val){
  mean_out_a <- df$Outcome == outcome_val
  df_out <- df[mean_out_a,]
  mean_ped <- mean(df_out$DiabetesPedigreeFunction)
  value <-paste("The mean of outcome ", outcome_val, "for the pedigree function is", mean_ped, sep = " ")
  return(value)
}

zero_out <- a.mean_val(0)
print(zero_out)
one_out <- a.mean_val(1)
print(one_out)


#Get the average bp for different ages
unique_age_val <- unique(df$Age)
bp_mean <- c()
for (i in unique_age_val){
  bp <- df$Age == i
  df_age <- df[bp, ]
  mean_bp <- mean(df_age$BloodPressure)
  bp_mean <- c(bp_mean, mean_bp)
}
print(bp_mean)

new_bp_mean <- data.frame(
  age_val = unique_age_val,
  mean_BP = bp_mean
  
)

print(new_bp_mean)

max_bp <- min(new_bp_mean$mean_BP)
a <- new_bp_mean$mean_BP == max_bp
new_bp_mean[a, ]
#print(unique_age_val)

#Calculates the sum for the bp instead of the mean
unique_age_val <- unique(df$Age)
bp_mean <- c()
for (i in unique_age_val){
  bp <- df$Age == i
  df_age <- df[bp, ]
  mean_bp <- sum(df_age$BloodPressure)
  bp_mean <- c(bp_mean, mean_bp)
}
print(bp_mean)

new_bp_mean <- data.frame(
  age_val = unique_age_val,
  mean_BP = bp_mean
  
)

print(new_bp_mean)

max_bp <- min(new_bp_mean$mean_BP)
a <- new_bp_mean$mean_BP == max_bp
new_bp_mean[a, ]


#calculates the percentage of different columns
percentage_function <- function(col_name){
  percent <- c()
  for (i in col_name){
    percent_formula <- round((i/ sum(col_name))* 100, 3)
    percent <- c(percent, percent_formula)
  }
  data_percent <- data.frame(
    main_data = col_name,
    percentage = percent
  )
  return(data_percent)
}
a <- percentage_function(df$BloodPressure)
View(a)

user_response_power <- as.numeric(readline(prompt = "Enter value for the power: "))
multiplier <- 10
last_digit_generator <- 2 ** user_response_power
last_digit <- last_digit_generator %% multiplier
print(last_digit)

user_response_power <- as.numeric(readline(prompt = "Enter value for the power: "))
multiplier <- 100
last_digit_generator <- 2 ** user_response_power
last_digit <- last_digit_generator %% multiplier
print(last_digit)

user_response_power <- as.numeric(readline(prompt = "Enter value for the power: "))
user_response_digits <- as.numeric(readline(prompt = "Enter value for last digits needed: "))
multiplier <- 1
for (i in 1:user_response_digits){
  multiplier <- multiplier * 10
}
print(multiplier)
last_digit_generator <- 2 ** user_response_power
last_digit <- last_digit_generator %% multiplier
print(last_digit)

d <- 4
fact <- 1
for (i in 1:d){
    fact <- fact * i
}
print(fact)

