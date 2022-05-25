a <- c(6, 10, 12, 3, 1)
b <- c(4, 13, 8, 9, 5)
diff <- c()
if (length(a) == length(b)){
  for (i in 1:length(a)){
    val <- a[i] - b[i]
    diff <- c(diff, val)
  }
}

n <- length(diff)
sum_data <- 0
for (i in diff){
  sum_data <- i + sum_data
}
mean_data <- sum_data/n

ind_val <- c()
for (i in diff){
  h <- i - mean_data
  ind_val <- c(ind_val, (h ** 2))
}

sum_ival <- 0
for (i in ind_val){
  sum_ival <- i + sum_ival
}

std_dev <- ((sum_ival/n) ** (1/2))
print(paste("The standard deviation of a - b is:", std_dev, sep = " "))

#NUMBER TWO SOLUTION
num_a <- length(a)
sum_a <- 0
for (i in a){
  sum_a <- i + sum_a
}

mean_b <- sum_b/num_b

#MAX OF A
max_a <- a[1]
for (i in a){
  if(i > max_a){
    max_a <- i
  }
}


# MEAN OF B
num_b <- length(b)
sum_b <- 0
for (i in b){
  sum_b <- i + sum_b
}

mean_b <- sum_b/num_b

#MAX OF B
max_b <- b[1]
for (i in b){
  if(i > max_b){
    max_b <- i
  }
}
print(max_b)

solution_two <- ((mean_a / max_b) - (mean_b / max_a))
print(paste("The solution to number two is:", solution_two, sep = " "))


#NUMBER THREE SOLUTION
data_sort <- sort(a)
mid <- length(a)/2
index <- round(mid)
a_index <- index + 1
if (index %%2 != 0){
  med <- data_sort[index]
} else if (index %%2 == 0){
  med <- data_sort[a_index]
}
min_a <- a[1]
for (i in a){
  if (i < min_a){
    min_a <- i
  }
}
min_b <- b[1]
for (i in b){
  if (i < min_b){
    min_b <- i
  }
}

solution_three <- med/((min_b + min_a) ** 2) ** (1/2)
print(paste("The anwser to number three is:", solution_three, sep = " "))
