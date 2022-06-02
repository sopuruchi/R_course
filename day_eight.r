library('readr')

path <- "C:/Users/TRAINER/Downloads/diabetes.csv"
df <- read_csv(path)
View(df)

#not_outcome <- !(df$outcome == 1)
not_outcome <- df$Outcome != 1
a <- df[not_outcome, ]
View(a)

last_three_years <- unique(df2$YEARS)
print(last_three_years)

countries <- unique(df2$COUNTRIES)
print(countries)


#######--------------------------------------------------------------------
load.data <- function(path_val){
  df2 <- read_csv(path_val)
  return(df2)
}

new.function <- function(df2){
  county <- df2$COUNTRIES
  territories <- c()
  for (i in county){
    if (i == "Nigeria" | i == "Ghana"){
      territories <- c(territories, "Anglophone")
    } else {
      territories <- c(territories, "Francophone")
    }
  }
  return(territories)
}

add.dataframe <- function(df2, territories){
  df3 <- mutate(df2, TERRITORY = territories)
  return(df3)
}

# territory <- data.frame(
#   TERRITORY <- territories
# )
# print(territory)

path_val <- "C:/Users/TRAINER/Documents/data/Brewery_data.csv"
x <- load.data(path_val)
new_territory <- new.function(x)
add_df <- add.dataframe(x, new_territory)

print(new_territory)
print(add_df)
View(add_df)
#######--------------------------------------------------------------------------


install.packages('dplyr')
library('dplyr')


df4 <- df2 %>% mutate(TERRITORY = territories,
               .after= COUNTRIES)
View(df4)

total_profit <- sum(df3$PROFIT)
print(total_profit)

a_y <- df3$TERRITORY == "Anglophone"
anglo <- df3[a_y,]
anglo_profit <- sum(anglo$PROFIT)
print(anglo_profit)
