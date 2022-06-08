install.packages("RColorBrewer")
library("RColorBrewer")
library("janitor")

path <- "C:/Users/TRAINER/Documents/data/Brewery_data.csv"
df <- read_csv(path)
View(df)

par(mar = c(1, 1, 1, 1, 1, 1, 1))
plant_cost <- df$PLANT_COST
hist(plant_cost)

frame_df <- df[df$YEARS == 2019 & df$COUNTRIES == "Ghana",]
View(frame_df)

plot(frame_df$PROFIT, type= "l")
hist(df$PLANT_COST, breaks = 5, col = brewer.pal(4, "Set3"), 
     main = "Histogram showing plant cost")

barplot(frame_df$PROFIT, col = brewer.pal(4, "Set3"))

boxplot(frame_df$UNIT_PRICE ~ frame_df$MONTHS, col = "red")

plot(x = frame_df$UNIT_PRICE, y = frame_df$PROFIT, col = "red")

barplot(table(frame_df$BRANDS))
pie(table(frame_df$BRANDS))


heatmap(as.matrix(frame_df))
df_val <- data.frame(frame_df$UNIT_PRICE, frame_df$PROFIT)
heatmap(as.matrix(df_val))

data("iris")
library(magrittr)

install.packages(c("rgl", "car"))

library("car")
library("rgl")

a <- iris$Sepal.Length
b <- iris$Petal.Length
c <- iris$Petal.Width

scatter3d(x = a, y = b, z = c)
scatter3d(x = frame_df$UNIT_PRICE, y =frame_df$PROFIT ,
          z = frame_df$QUANTITY)

#http://www.sthda.com/english/wiki/amazing-interactive-3d-scatter-plots-r-software-and-data-visualization


