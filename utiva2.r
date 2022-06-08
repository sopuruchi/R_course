path <- "C:/Users/TRAINER/Documents/data/Brewery_data.csv"
df <- read_csv(path)
View(df)


percentage_function <- function(year){
  year_val <- df$YEARS == year
  year_df <- df[year_val,]
  unique_months <- unique(year_df$MONTHS)
  sum_profit <- c()
  percent <- c()
  for (i in unique_months){
    months <- year_df$MONTHS == i
    months_df <- year_df[months,]
    sum_months <- sum(months_df$PROFIT)
    sum_profit <- c(sum_profit, sum_months)
  }
  for (i in sum_profit){
    percent_formula <- round((i/ sum(sum_profit))* 100, 3)
    percent <- c(percent, percent_formula)
  }
  data_percent <- data.frame(
    main_data = unique_months,
    percentage = percent
  )
  return(data_percent)
}
month_percent <- percentage_function(2019)
View(month_percent)


#NUMBER 8
country_val <- df$COUNTRIES == "Senegal"
country_df <- df[country_val,]
unique_brand <- unique(country_df$BRANDS)
print(unique_brand)
sum_profit <- c()
percent <- c()
for (i in unique_brand){
  brand <- country_df$BRANDS == i
  brand_df <- country_df[brand,]
  sum_months <- sum(brand_df$PROFIT)
  sum_profit <- c(sum_profit, sum_months)
}
highest_index <- which.max(sum_profit)
highest_brand <- country_df$BRANDS[highest_index]
print(highest_brand)



############SECTION B
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

new_territory <- new.function(df)
add_df <- add.dataframe(df, new_territory)
View(add_df)

franc_condition <- add_df$TERRITORY == "Francophone"
franc_df <- add_df[franc_condition,]
years_condition <- franc_df$YEARS == 2018 | franc_df$YEARS == 2019
years_df <- franc_df[years_condition,]
sum_quantity <- c()
unique_brands <- unique(years_df$BRANDS)
for (i in unique_brands){
  brand <- year_df$BRANDS == i
  brand_df <- year_df[brand,]
  sum_year <- sum(brand_df$QUANTITY)
  sum_quantity <- c(sum_quantity, sum_year)
}
data_percent <- data.frame(
  main_data = unique_brands,
  percentage = sum_quantity
)
print(data_percent)
print(sum_quantity)
quantity <- sort(sum_quantity)
print(quantity)
quantity_index <- year_df$BRANDS[length(sum_quantity)]
print(quantity_index)

