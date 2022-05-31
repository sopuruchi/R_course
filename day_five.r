x <- 1:3
print(x)

y <- seq(2, 10, 3)
print(y)

print(sum(20:40))
print(mean(20:30))

value <- 50:10
mean_function <- function(value){
  len <- length(value)
  sum_value <- 0
  for (i in value){
    sum_value <- i + sum_value
  }
  mean_val <- sum_value/len
  mean_val
}
mean_function(value)

factorial_func <- function(x){
  count_val <- 1:nchar(x)
  fac <- 1
  for (i in count_val){
    fac <- i * fac
  }
  print(fac)
}

name <- 'jamie'
factorial_func(name)


volume_cube <- function(a, b, c){
  vol <- a * b * c
  print(vol)
}

calc_age <- function(date, dob, name){
  age <- date - dob
  print(age)
}

date <- as.numeric(readline(prompt = "Enter current year: "))
dob <- as.numeric(readline(prompt = "Enter year of birth: "))
name <- readline(prompt = "Enter Name: ")

calc_age(date, dob, name)

x <- 1:50
for(i in x){
  y <- sample(1:i+1, 1, replace = F)
  print(y)
}

x <- as.numeric(readline(prompt = "Enter value of X: "))
y <- as.numeric(readline(prompt = "Enter value of Y: "))
