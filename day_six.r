fact_func <- function(x){
  str_count <- nchar(x)
  fact <- 1
  for (i in 1:str_count){
    fact <- fact * i
  }
  return(fact)
}
mean_val <- function(fact, x){
  len_x <- nchar(x)
  formular <- fact/len_x
  return(formular)
}
p <- fact_func('jamie')
x <- mean_val(p, 'jamie')
print(p)
print(x)

sum_dev <- function(vector_val){
  sum_val <- 0
  for (i in vector_val){
    sum_val <- sum_val + i
  }
  return(sum_val)
}

mean_vector <- function(sum_val, vector_val){
  count_vec <- length(vector_val)
  mean.val <- sum_val / count_vec
  return(mean.val)
}

ind_flat <- function(vector_val, mean_val){
  p_vec <- c()
  for (i in vector_val){
    x <- i - mean_val
    p_vec <- c(p_vec, (x ** 2))
  }
  return(p_vec)
}

sum_ind <- function(p_vec){
  sum_vec <- 0
  for (i in p_vec){
    sum_vec <- sum_vec + i
  }
  return(sum_vec)
}

std_dev <- function(vector_val, sum_vec){
  len_std <- length(vector_val)
  std <- (sum_vec / len_std) ** (1/2)
  return(std)
}

val_vector <- c(3, 6, 8, 3, 5, 9, 11)
h <- sum_dev(val_vector)
g <- mean_vector(h, val_vector)
i <- ind_f


























































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































lat(val_vector, g)
j <- sum_ind(i)
k <- std_dev(val_vector, j)
print(g)
print(h)
print(i)
print(j)
print(k)

