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
  enter_val <- as.integer(readline(prompt ="Enter range for array: "))
  arr <- c(arr, enter_val)
}
print(arr)


