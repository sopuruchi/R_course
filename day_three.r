data_set <- c(4, 10, 12, 13, 5, 8.9, 1.4, 1.3)
n <- length(data_set)
sum_data <- 0
for(i in data_set){
  sum_data <- i + sum_data
}
#print(sum_data)
mean_data <- sum_data/n
data_i <- c()
for (i in data_set){
  a <- i - mean_data
  data_i <- c(data_i, (a ** 2))
}
#print(data_i)
sum_idata <- 0
for(i in data_i){
  sum_idata <- i + sum_idata
}
#print(sum_idata)
std_dev <- ((sum_idata/n) ** (1/2))
print(std_dev)

data_set <- c(4, 10, 12, 13, 5, 8.9, 1.4, 1.3)
n <- length(data_set)
sum_set <- 0
for (i in data_set){
  sum_set <- i + sum_set
}
mean_set <- sum_set/n
data_s <- c()
for (i in data_set){
  a <- i - mean_set
  data_s <- c(data_s, (a ** 2))
}

sum_sdata <- 0
for (i in data_s){
  sum_sdata <- i + sum_sdata
}

variance <- ((sum_sdata/n-1) ** (1/2))
print(variance)

t <- c(3, 10, 22, 11, 8, 12)
pos_val <- t[1]
for (i in t){
  if (i > pos_val){
    pos_val <- i
  }
}
print(pos_val)

t <- c(3, 10, 22, 11, 8, 12)
min_val <- t[1]
for (i in t){
  if (i < min_val){
    min_val <- i
  }
}
print(min_val)