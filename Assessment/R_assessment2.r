# EXERCISE 3.8.1
random_nums <- sample(3:6, 50, replace = T)
print(random_nums)

# EXERCISE 3.8.2
x <- sample(1:10, 1)
y <- sample(2:5, 1)
z <- x**y
print(z)


# EXERCISE 3.8.3
x <- sample(1:10, 1)
name <- "Hannah"
for (i in 1:x){
  print(name)
}

p_name <- function(x){
  d <- sample(1:10, 1)
  for (i in 1:d){
    print(x)
  }
}
a <- p_name('Hannah')
print(a)


# EXERCISE 3.8.4
random_num <- round(runif(1, 1, 10), 2)
print(random_num)


# EXERCISE 3.8.5
random_num <- 1:50
for (i in random_num){
  y <- sample(1:i+1, 1, replace = F)
  print(y)
}


# EXERCISE 3.8.6
x <- as.numeric(readline(prompt = "Enter value for X: "))
y <- as.numeric(readline(prompt = "Enter value for Y: "))
z <- x - y
if (z < 0){
  z <- z * (-1)
}
answer <- z / (x + y)
print(answer)

# EXERCISE 3.8.7


# EXERCISE 3.8.8
num_of_secs <- as.numeric(readline(prompt = "Enter number of seconds: "))
mins <- num_of_secs / 60
sec <- num_of_secs %% 60
print(paste("You have", mins, "mins and", sec, "secs", sep = " "))


# EXERCISE 3.8.9
current_hour <- as.numeric(readline(prompt = "Enter hour: "))
future_hour <- as.numeric(readline(prompt = "How many hours ahead: "))
z <- current_hour + future_hour
future_time <- z %% 12
print(paste("Future time is", future_time, "O'clock", sep = " "))


# EXERCISE 3.8.10a
user_response_power <- as.numeric(readline(prompt = "Enter value for the power: "))
multiplier <- 10
last_digit_generator <- 2 ** user_response_power
last_digit <- last_digit_generator %% multiplier
print(last_digit)


# EXERCISE 3.8.10b
user_response_power <- as.numeric(readline(prompt = "Enter value for the power: "))
multiplier <- 100
last_digit_generator <- 2 ** user_response_power
last_digit <- last_digit_generator %% multiplier
print(last_digit)


# EXERCISE 3.8.10c
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


# EXERCISE 3.8.11
user_response_weight <- as.numeric(readline(prompt = "Enter value for the weight in kg: "))
pound_converter <- round(2.205 * user_response_weight, 1)
print(paste(pound_converter, "pounds", sep = " "))


# EXERCISE 3.8.12
user_response <- as.numeric(readline(prompt = "Enter Value: "))
fact <- 1
for (i in 1:user_response){
  fact <- fact * i
}
print(fact)


# EXERCISE 3.8.13
user_response <- as.numeric(readline(prompt = "Enter Value: "))
sine_val <- sin(user_response)
cosine_val <- cos(user_response)
tangent_val <- tan(user_response)
print(paste("The sine is: ", sine_val, "; cosine is: ", cosine_val, "; tangent is: ", tangent_val, sep = " "))


# EXERCISE 3.8.14
user_response <- as.numeric(readline(prompt = "Enter Value: "))
sine_val <- sin(user_response)
print(paste("The sine of", user_response, "is:", sine_val, sep = " "))





####------------------------------------------EXERCISE 4.5----------------------------------------------------####
# EXERCISE 4.5.1
user_response_length <- as.numeric(readline(prompt = "Enter value for the length in cm: "))
if (user_response_length < 0){
  print("Invalid entry")
} else {
  cm_converter <- 0.394 * user_response_weight
  print(paste(cm_converter, "inches", sep = " "))
}


# EXERCISE 4.5.2
temp <- as.numeric(readline(prompt = "Enter temperature value: "))
unit <- tolower(readline(prompt = "Enter unit(F/C): "))
if (unit == 'f'){
  celsius <- (5/9) * (temp - 32)
  print(paste("The temperature value is ", celsius, "degrees celsius", sep = " "))
} else if (unit == 'c'){
  fahr <- (9/5) * (temp + 32)
  print(paste("The temperature value is ", fahr, "degrees fahrenheit", sep = " "))
}



# EXERCISE 4.5.3
temperature <- as.numeric(readline(prompt = "Enter temperature value: "))
if (temperature < -273.15){
  print("The temperature is invalid because it is below absolute 0")
} else if(temperature == -273.15){
  print("The temperature is absolute 0")
} else if(temperature > -273.15 & temperature < 0){
  print("The temperature is below freezing")
} else if(temperature == 0){
  print("The temperature is at the freezing point")
} else if(temperature > 0 & temperature < 100){
  print("The temperature is in the normal range")
} else if(temperature == 100){
  print("The temperature is at the boiling point")
} else if (temperature > 100){
  print("The temperature is above boiling point")
}



# EXERCISE 4.5.4
credits <- as.numeric(readline(prompt = "Enter credits taken: "))
if (credits <= 23){
  print("You are a Freshman")
} else if (credits >= 24 & credits <= 53){
  print("You are a Sophomore")
} else if (credits >= 54 & credits <= 83){
  print("you are a Junior")
} else if (credits >= 84){
  print("You are a Senior")
}



# EXERCISE 4.5.5
user_response <- as.numeric(readline(prompt = "Enter value: "))
sum_divisors <- 0
for (i in 1:user_response){
  if (user_response %% i == 0){
    sum_divisors <- sum_divisors + i
  }
}
print(paste("The sum of divisors of", user_response, "is:", sum_divisors, sep = " "))



# EXERCISE 4.5.6
user_response <- 1:10000
perfect_numbers <- c()
for (j in user_response){
  index <- j
  i <- 1
  sum_divisors <- 0
  while (i < index) {
    if(index %% i == 0){
      sum_divisors <- sum_divisors + i
    }
    i = i + 1
  }
  if(sum_divisors == index){
    perfect_numbers <- c(perfect_numbers, index)
  }
}
print(perfect_numbers)



# EXERCISE 4.5.7
user_response <- as.numeric(readline(prompt = "Enter value: "))
divisors <- c()
perf <- c()
for (i in 1:user_response){
  if (user_response %% i == 0){
    divisors <- c(divisors, i)
  }
}
for (k in divisors){
  n <- k
  j <- 1
  while (j < n) {
    if (n / j == j){
      perf <- c(perf, n)
    }
    j <- j + 1
  }
}
if(length(perf) == 0){
  print(paste(user_response, "is squarefree", sep = " "))
} else {
  print(paste(user_response, "is not squarefree", sep = " "))
}



# EXERCISE 4.5.8
x <- 5
y <- 9
z <- 10
x <- x + y
y <- x - y
x <- x - y
print(x)
y <- y + z
z <- y - z
y <- y - z
print(y)
z <- z + x
x <- z - x
z <- z - x
print(z)
