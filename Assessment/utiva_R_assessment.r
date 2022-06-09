install.packages('readr')
install.packages('dplyr')
install.packages('janitor')

library('readr')
library('dplyr')
library('janitor')

path <- "C:/Users/HANNAH/Documents/data/brewery_data.csv"
df <- read_csv(path)
View(df)

#SESSION A QUESTION ONE
brewery_profit <- sum(df$PROFIT)
print(brewery_profit)

#SESSION A QUESTION TWO
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

unique_territory <- unique(new_territory)
profit_sum <- c()
for (i in unique_territory){
  territory_val <- add_df$TERRITORY == i
  territory_df <- add_df[territory_val, ]
  territory_profit <- sum(territory_df$PROFIT)
  profit_sum <- c(profit_sum, territory_profit)
}

territory_profit <- data.frame(
  Territory <- unique_territory,
  Profit <- profit_sum
)
View(territory_profit)


#SESSION A QUESTION THREE
country_with_highest_profit <- function(x){
  year_condition <- df$YEARS == x
  condition_df <- df[year_condition, ]
  unique_countries <- unique(df$COUNTRIES)
  profit_vec <- c()
  for(i in unique_countries){
    country_val <- condition_df$COUNTRIES == i
    country_df <- condition_df[country_val, ]
    country_profit <- sum(country_df$PROFIT)
    profit_vec <- c(profit_vec, country_profit)
  }
  
  highest_profit <- which.max(profit_vec)
  country <- unique_countries[highest_profit]
  return(country)
}
function_call <- country_with_highest_profit(2019)
print(function_call)


# profit_df <- data.frame(
#   Countries <- unique_countries,
#   Profit <- profit_vec
# )
# View(profit_df)
# 
# highest_profit <- which.max(profit_df$Profit)
# country <- profit_df$Countries....unique_countries[highest_profit]
# print(country)

#SESSION A QUESTION FOUR
unique_years <- unique(df$YEARS)
profit_vec <- c()
for(i in unique_years){
  year_val <- df$YEARS == i
  year_df <- df[year_val, ]
  year_profit <- sum(year_df$PROFIT)
  profit_vec <- c(profit_vec, year_profit)
}

highest_profit <- which.max(profit_vec)
year <- unique_years[highest_profit]
print(year)

#SESSION A QUESTION FIVE
unique_month <- unique(df$MONTHS)
profit_vec <- c()
for(i in unique_month){
  month_val <- df$MONTHS == i
  month_df <- df[month_val, ]
  month_profit <- sum(month_df$PROFIT)
  profit_vec <- c(profit_vec, month_profit)
}

highest_profit <- which.min(profit_vec)
month <- unique_month[highest_profit]
print(month)


##----------------------------------------OR------------------------------------------##
min_profit <- which.min(df$PROFIT)
month <- df$MONTHS[min_profit]
print(month)


#SESSION A QUESTION SIX
year_month_condition <- df$YEARS == 2018 & df$MONTHS == 'December'
condition_df <- df[year_month_condition, ]
min_profit <- which.min(condition_df$PROFIT)
profit <- condition_df$PROFIT[min_profit]
print(profit)


#SESSION A QUESTION SEVEN
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


#SESSION A QUESTION EIGHT
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



####-----------------------------------SESSION B-------------------------------------####
#SESSION B NUMBER ONE
franc_condition <- add_df$TERRITORY == "Francophone"
franc_df <- add_df[franc_condition,]
years_condition <- franc_df$YEARS == 2018 | franc_df$YEARS == 2019
years_df <- franc_df[years_condition,]
sum_quantity <- c()
unique_brands <- unique(years_df$BRANDS)
for (i in unique_brands){
  brand <- years_df$BRANDS == i
  brand_df <- years_df[brand,]
  sum_year <- sum(brand_df$QUANTITY)
  sum_quantity <- c(sum_quantity, sum_year)
}
data_quantity <- data.frame(
  BRAND = unique_brands,
  QUANTITY = sum_quantity
)
new_quantity <- data_quantity[order(-data_quantity$QUANTITY), ]
high_quantity <- new_quantity %>% slice(1:3)
print(high_quantity$BRAND)


#SESSION B NUMBER TWO
condition_val <- df$COUNTRIES == 'Ghana'
condition_df <- df[condition_val,]
sum_quantity <- c()
unique_brands <- unique(condition_df$BRANDS)
for (i in unique_brands){
  brand <- condition_df$BRANDS == i
  brand_df <- condition_df[brand,]
  sum_country <- sum(brand_df$QUANTITY)
  sum_quantity <- c(sum_quantity, sum_country)
}
highest_index <- which.max(sum_quantity)
highest_brand <- condition_df$BRANDS[highest_index]
print(highest_brand)



#SESSION B NUMBER THREE
country_df <- df[df$COUNTRIES == "Nigeria", ]
n_beer_df <- country_df[country_df$BRANDS != "grand malt" & country_df$BRANDS != "beta malt", ]
unique_beer_brand <- unique(n_beer_df$BRANDS)
sum_quantity <- c()
for(i in unique_beer_brand){
  beer_df <- n_beer_df[n_beer_df$BRANDS == i, ]
  sum_quantity <- c(sum_quantity, sum(beer_df$QUANTITY))
}
data_quantity <- data.frame(
  BRAND = unique_beer_brand,
  QUANTITY = sum_quantity
)
View(data_quantity)


#SESSION B NUMBER FOUR
anglo_df <- add_df[add_df$TERRITORY == "Anglophone",]
n_beer_df <- anglo_df[anglo_df$BRANDS == "grand malt" | anglo_df$BRANDS == "beta malt", ]
years_df <- n_beer_df[n_beer_df$YEARS == 2018 | n_beer_df$YEARS == 2019,]
sum_quantity <- c()
unique_brands <- unique(years_df$BRANDS)
for (i in unique_brands){
  brand_df <- years_df[years_df$BRANDS == i,]
  sum_year <- sum(brand_df$QUANTITY)
  sum_quantity <- c(sum_quantity, sum_year)
}
data_quantity <- data.frame(
  BRAND = unique_brands,
  QUANTITY = sum_quantity
)
new_quantity <- data_quantity[order(-data_quantity$QUANTITY), ]
print(new_quantity)



#SESSION B NUMBER FIVE
country_df <- df[df$COUNTRIES == "Nigeria", ]
unique_brand <- unique(country_df$BRANDS)
sum_quantity <- c()
for(i in unique_brand){
  brand_df <- country_df[country_df$BRANDS == i, ]
  sum_quantity <- c(sum_quantity, sum(brand_df$QUANTITY))
}
data_quantity <- data.frame(
  BRAND = unique_brand,
  QUANTITY = sum_quantity
)
new_quantity <- data_quantity[order(-data_quantity$QUANTITY),]
print(new_quantity)



#SESSION B NUMBER SIX
country_df <- df[df$COUNTRIES == "Nigeria", ]
region_df <- country_df[country_df$REGION == "southsouth", ]
unique_brand <- unique(region_df$BRANDS)
sum_quantity <- c()
for(i in unique_brand){
  brand_df <- region_df[region_df$BRANDS == i, ]
  sum_quantity <- c(sum_quantity, sum(brand_df$QUANTITY))
}
data_quantity <- data.frame(
  BRAND = unique_brand,
  QUANTITY = sum_quantity
)
new_quantity <- data_quantity[order(-data_quantity$QUANTITY),]
print(new_quantity)
