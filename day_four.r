#d <- c('a', 'b', 'c', 'd', 'e', 'j')
#t <- c('g', 'k', 'l', 'a', 'e', 'c')
`%!in%` <- Negate(`%in%`)
for (i in d){
  if (i %!in% t){
    print(i)
  }
}

d <- c('jamie', 'kate', 'pogba')
t <- c('kate', 'john', 'jamie')
`%!in%` <- Negate(`%in%`)
t_d <- c()
for (i in d){
  if (i %!in% t){
    t_d <- c(t_d, nchar(i))
  }
}
print(t_d)


d <- c('jamie', 'kate', 'pogba')
for (i in length(d)){
  for (j in d[i]){
    print(j)
  }
}


y <- d[!(d %in% t)]
for (i in y){
  print(nchar(i))
}

value <- readline(prompt = "Enter your number: ")
value2 <- readline(prompt = "Enter your number: ")
print(as.interger(value) + as.integer(value2))

arr <- c()
range_arr <- as.integer(readline(prompt ="Enter range for array: "))

for (i in 1:range_arr){
  enter_val <- as.integer(readline(prompt ="Enter value for array: "))
  arr <- c(arr, enter_val)
}
print(arr)

fact_arr <- 1
range_fact <- as.character(readline(prompt = "Enter Name"))
for (i in 1:nchar(range_fact)){
  fact_arr <- (i * fact_arr)
}
print(paste(range_fact, "can be arranged", fact_arr, "times", sep = " "))


temp <- as.numeric(readline(prompt = "Enter temperature value: "))
unit <- tolower(readline(prompt = "Enter unit(F/C): "))
if (unit == 'f'){
  celsius <- (9/5) * (temp - 32)
  print(paste("The temperature value is ", celsius, "degrees celsius", sep = " "))
} else if (unit == 'c'){
  fahr <- (5/9) * (temp + 32)
  print(paste("The temperature value is ", fahr, "degrees fahrenheit", sep = " "))
} else {
  print("Invalid Value")
}

range_max <- as.integer(readline(prompt = "Enter the array max value: "))  
arr_val <- c()
for (i in 1:range_max){
  arr_val <- c(arr_val, readline(prompt="enter array values: "))
}
for (i in arr_val){
  k <- 1
  b <- nchar(i)
  for (n in 1:b){
    k <- k * n
  }
  print(paste("the permutation of ", i, "is", k, sep=" "))
}



