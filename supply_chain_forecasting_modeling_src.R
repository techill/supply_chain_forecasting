## ------ Set Up R Packages & Libraries ------

library(tidyverse)
library(dplyr)
library(lubridate)
library(plyr)

## Load the package to read large file through fread() function:
library(data.table)

## Intall the package to use clean_names() function:
# install.packages("janitor")
library(janitor)

## Install the party package for Classification using Decison Tree:
# install.packages("party")
library(party)

## Install the rpart package for Decison Tree using rpart() function:
# install.packages("rpart")
library(rpart)

## Install the rpart.plot package for visualizing the Decison Tree using rpart.plot() function:
# install.packages("rpart.plot")
library(rpart.plot)

## Install the GGally package to use ggpairs() function to visualize the correlation between variables (can be numerical and categorical data):
# install.packages("GGally")
library(GGally)

## Install the stats package to use lm() function to make the linear regression model:
# install.packages("stats")
library(stats)

## Install the Metrics package to use rmse() function to compute the Root Mean Square Error (RMSE):
# install.packages("Metrics")
library(Metrics)

## Install the package to use the CrossTable function to evaluate model performance:
# install.packages("gmodels")
library(gmodels)

## Install the package to use the randomForest() function for the Random Forest model:
# install.packages("randomForest")
library(randomForest)

## Install the package for the SVM model:
# install.packages("caret")
library(caret)

## Install the package for the kNN model:
# install.packages("class")
library(class)

# ## Install the forecast package to use the auto.arima function to make forecasts with the ARIMAX model:
# # install.packages("forecast")
# library(forecast)

# ## For data splitting 
# library(rsample)
# ## For data bagging
# library(ipred)


## Turn off scientific notation
options(scipen = 999)

## Graphical scales map data to aesthetics, and provide methods for automatically 
## determining breaks and labels for axes and legends for visulization.
# install.packages("scales")
library(scales)

## Install the package of R Color Palettes: 
# install.packages("colorspace")
library(colorspace)




## ------ Data Import ------

getwd()

goodsale <- fread("supply-chain_dataset_Kaggle-CCFBDCI_JollyChic/goodsale.csv")
map_dbl(goodsale, ~sum(is.na(.)))
summary(goodsale$goods_price)
View(goodsale)
glimpse(goodsale)
## Convert the data.table to a tibble to better the processing performance:
goodsale <- as_tibble(goodsale)
class(goodsale)
## Columns to keep based on the data validation results: data_date, goods_id, goods_num, goods_price, orginal_shop_price
## What can be predicted: Sales


goodsdaily <- fread("supply-chain_dataset_Kaggle-CCFBDCI_JollyChic/goodsdaily.csv")
map_dbl(goodsdaily, ~sum(is.na(.)))
summary(goodsdaily)
glimpse(goodsdaily)
class(goodsdaily)
## Convert the data.table to a tibble to better the processing performance:
goodsdaily <- as_tibble(goodsdaily)
class(goodsdaily)
## Columns to keep based on the data validation results: data_date, goods_id, goods_click, cart_click, favorites_click, onsale_days
## What can be predicted: Sales, Conversion Rate


goods_promote_price <- fread("supply-chain_dataset_Kaggle-CCFBDCI_JollyChic/goods_promote_price.csv")
map_dbl(goods_promote_price, ~sum(is.na(.)))
summary(goods_promote_price)
glimpse(goods_promote_price)
## Columns to keep based on the data validation results: None -- Drop the table as the not all promotions were captured -- Incompleted Dataset


goodsinfo <- fread("supply-chain_dataset_Kaggle-CCFBDCI_JollyChic/goodsinfo.csv")
map_dbl(goodsinfo, ~sum(is.na(.)))
summary(goodsinfo)
glimpse(goodsinfo)
class(goodsinfo)
goodsinfo
## Convert the data.table to a tibble to better the processing performance:
goodsinfo <- as_tibble(goodsinfo)
class(goodsinfo)
## Columns to keep based on the data validation results: goods_id, goods_season




## ------ Data Preprocessing ------
## Remove the comma in some prices and convert prices from characters to numbers:
goodsale$goods_price <- as.numeric(gsub(",", "", goodsale$goods_price))
goodsale$orginal_shop_price <- as.numeric(gsub(",", "", goodsale$orginal_shop_price))
map_dbl(goodsale, ~sum(is.na(.)))
summary(goodsale)


head(goodsale)
head(goodsdaily)
head(goods_promote_price)
head(goodsinfo)

## Get the frequency of each goods_id in goodsdaily: 
count.alldaily.goods_id <- count(goodsdaily$goods_id)
glimpse(count.alldaily.goods_id)
class(count.alldaily.goods_id)
## Sort the data by freq in descending order -- View the top selling goods_id:
count.alldaily.goods_id[order(-count.alldaily.goods_id$freq), ]
## Print the distribution of selling frequency among all goods_id:
summary(count.alldaily.goods_id$freq)

## Print the rows that have the maximum selling frequency:
count.alldaily.goods_id %>% 
  filter(freq == 381) %>% 
  str()

summary(goodsdaily$sales_uv)
## Print the rows that have the maximum sales_uv:
goodsdaily %>% 
  filter(sales_uv == 7962) %>% 
  print()

## Print the rows that have the maximum sales_uv by the data_date and goods_id found above:
goodsale %>% 
  filter(data_date == 20171124, goods_id == "GbnE5D") %>% 
  print()

summary(goodsale$goods_num)
## Print the rows that have the maximum goods_num:
goodsale %>% 
  filter(goods_num == 2157) %>% 
  print()




## ------ Data Validation ------
## Investigate which table & while variable is useful/should be keep in the master table:

temp <- goodsale %>% 
  filter(goods_id == "G01ENo")
str(temp)
count(temp$data_date)
summary(temp)

temp2 <- temp %>% 
  filter(data_date == 20171125)

temp3 <- goodsdaily %>% 
  filter(data_date == 20171125, goods_id == "G01ENo")

temp4 <- goods_promote_price %>% 
  filter(data_date == 20171125, goods_id == "G01ENo")

temp5 <- goodsale %>% 
  filter(goods_id == "GBLrYn")

temp6 <- goodsdaily %>% 
  filter(goods_id == "GBLrYn")

goods_promote_price %>% 
  filter(data_date == 20180129, goods_id == "GBLrYn") %>% 
  View()

goods_promote_price %>% 
  filter(goods_id == "GrwmCH") %>% 
  print()

goodsale %>% 
  filter(data_date == 20180226, goods_id == "GrwmCH") %>% 
  print()


summary(goodsale$data_date)

summary(count(goodsale$sku_id))


## Remove datasets that is no longer needed and release memory:
rm(temp, temp2, temp3, temp4, temp5, temp6, goods_promote_price)
gc()




## ------ Data Manipulation ------

## Join two datasets for the sales prediction model:
goodsale.comb <- merge(goodsale, goodsinfo, by = "goods_id")

str(goodsale.comb)

## Remove datasets that is no longer needed and release memory:
rm(goodsale, goodsinfo)
gc()


## Check how many types of goods_season in total: 0-6 -- Good to be a feature
count(goodsale.comb$goods_season)
summary(goodsale.comb$goods_season)

## Check the totol unique cat_level1_id: 50
count(goodsale.comb$cat_level1_id)
summary(goodsale.comb$cat_level1_id)
## Check the totol unique cat_level2_id: 357
count(goodsale.comb$cat_level2_id)
summary(goodsale.comb$cat_level2_id)
## Check the totol unique cat_level3_id: 1011
count(goodsale.comb$cat_level3_id)
summary(goodsale.comb$cat_level3_id)
## Check the totol unique brand_id: 918
count(goodsale.comb$brand_id)
summary(goodsale.comb$brand_id)


## Export dataframes for future use:
## Rda file: small size, fast export and import speed, can be used immediately after loading, but only readable in R.
## CSV file: large size, slow export and import speed, need to be converted to dataframe or tibble if needed, readable in most environments.
save(goodsale.comb, file = "./data/goodsale.comb_df.Rda")
write.csv(goodsale.comb, file = "./data/goodsale.comb_df.csv")


## Subset the goodsale.comb dataset by keeping only useful attributes:
salesDaily <- as_tibble(subset(goodsale.comb, select = c(data_date, goods_id, sku_id, goods_num, goods_price, orginal_shop_price, goods_season)))


## Check the products sold with invalid goods_price and orginal_shop_price: 4237 products in total
salesDaily %>% 
  filter(goods_price == 0 & orginal_shop_price == 0) %>% 
  dim()

## Keep only the products have valid orginal_shop_price:
salesDaily <- salesDaily %>% 
  filter(orginal_shop_price > 0)

str(salesDaily)


## Remove dataset from memory after export:
rm(goodsale.comb)
gc()


# ## Calculate the daily sales of each product: total quantity sold, average goods_price, average orginal_shop_price:
# salesDaily.allGoods <- ddply(salesDaily, .(data_date, goods_id), summarize, goods_num = sum(goods_num), goods_price = mean(goods_price), 
#                              orginal_shop_price = mean(orginal_shop_price))
# ## Dataset is too large to be processed. Need to subset the data for this project due to memory limits.


# ## Join two datasets for the sales prediction model:
# salesDaily.allGoods <- inner_join(salesDaily, goodsdaily, match = c("data_date", "goods_id"))
# ## Dataset is too large to be processed. Need to subset the data for this project due to memory limits.


## Add new columns to show the discount_rate, gross_sales, discounts, net_sales:
salesDaily$discount_rate <- (salesDaily$orginal_shop_price - salesDaily$goods_price) / salesDaily$orginal_shop_price * 100
salesDaily$gross_sales <- salesDaily$orginal_shop_price * salesDaily$goods_num
salesDaily$discounts <- (salesDaily$orginal_shop_price - salesDaily$goods_price) * salesDaily$goods_num
salesDaily$net_sales <- salesDaily$gross_sales - salesDaily$discounts


str(salesDaily)

## Check the quantity of missing values in each column:
map_dbl(salesDaily, ~sum(is.na(.)))

summary(salesDaily)

## Find the products that have error discount_rate so it can be elimated later:
salesDaily %>% 
  filter(discount_rate < 0)

## Remove the rows that have invalid discount_rate:
salesDaily <- salesDaily %>% 
  filter(discount_rate >= 0)
summary(salesDaily)


# salesDaily %>% 
#   filter(goods_num == 2157)
# 
# salesDaily %>% 
#   filter(goods_price == 1332)
# 
# salesDaily %>% 
#   filter(orginal_shop_price == 1332)


## Convert data_date from integer to character and then date date type:
str(salesDaily$data_date)
require(lubridate)
salesDaily$data_date <- as_date(as.character(salesDaily$data_date))

## Extract year, month, day from date using functions from the lubridate package:
# salesDaily$data_date <- ymd(salesDaily$data_date)
salesDaily$data_year <- year(salesDaily$data_date)
summary(salesDaily$data_year)

salesDaily$data_month <- month(salesDaily$data_date)
summary(salesDaily$data_month)   
hist(salesDaily$data_month)   
## Insight: 3rd Percentile -- November has the most sales by SKU -- Holiday Seaon.

# ggplot(data = salesDaily, aes(salesDaily$data_month)) + geom_histogram(col="red", aes(fill=..count..))

## Export the plot:
ggsave(filename = "./img/sales-frequency-by-month.png", plot = hist(salesDaily$data_month))


salesDaily$data_day <- day(salesDaily$data_date)
summary(salesDaily$data_day)
hist(salesDaily$data_day)   
## Insight: 3rd Percentile -- The 23rd day of the month has the most sales by SKU.

## Export the plot:
ggsave(filename = "./img/sales-frequency-by-day.png", plot = hist(salesDaily$data_day))


salesDaily %>% 
  filter(data_year == 2017 & data_month == 11) %>% 
  dim()


# ## Calculate the total quantity sold over the time of each sku_id:
# total.sales.by.sku <- aggregate(goods_num ~ sku_id, data = salesDaily, sum) %>% 
#   print()
# 
# ## Sort the good_id by total goods_num sold in descending order -- View the top selling sku_id:
# total.sales.by.sku[order(-total.sales.by.sku$goods_num), ]
# summary(total.sales.by.sku$goods_num)
# 
# ## Calculate the total gross revenue over the time by each sku_id:
# gross.revenue.by.sku <- aggregate(gross_sales ~ sku_id, data = salesDaily, sum)
# summary(gross.revenue.by.sku$gross_sales)
# ## Find the top 6 sku_id by gross sales generated:
# head(gross.revenue.by.sku[order(-gross.revenue.by.sku$gross_sales), ])
# 
# top.revenue.sku <- head(gross.revenue.by.sku[order(-gross.revenue.by.sku$gross_sales), ]) %>% 
#   select(sku_id)
# top.revenue.sku$sku_id
# 
# ## Find the number of observations of 6 top-revenue sku_id:
# salesDaily %>% 
#   filter(sku_id == "SK7aYFVx" | sku_id == "SK0zgq8K" | sku_id == "SK4taRSU" | sku_id == "SKCocIuf" | 
#            sku_id == "SK2AbpzN" | sku_id == "SKfkXMRm") %>% 
#   str() 
# ## Only 1778 observations -- too narrow for modeling
# 
# 
# 
# ## Calculate the total gross revenue over the time by each goods_id:
# gross.revenue.by.goods <- aggregate(gross_sales ~ goods_id, data = salesDaily, sum)
# summary(gross.revenue.by.goods$gross_sales)
# ## Find the top 6 goods_id by gross sales generated:
# head(gross.revenue.by.goods[order(-gross.revenue.by.goods$gross_sales), ])
# 
# top.revenue.goods <- head(gross.revenue.by.goods[order(-gross.revenue.by.goods$gross_sales), ]) %>% 
#   select(goods_id)
# top.revenue.goods$goods_id
# 
# ## Find the observations of 6 top-revenue goods_id:
# top.sales.by.goods.revenue <- salesDaily %>% 
#   filter(goods_id == "GlSCp6" | goods_id == "G2qeoS" | goods_id == "GsG5JW" | goods_id == "GJpAlu" | goods_id == "G8ks47" | goods_id == "GZu16V")
# str(top.sales.by.goods.revenue)
# ## 13232 observations amoung 6 goods_id -- save for classfication modeling.




## Calculate the total quantity sold over the time of each good_id:
total.sales.by.goods <- aggregate(goods_num ~ goods_id, data = salesDaily, sum) %>% 
  print()

## Sort the good_id by total goods_num sold in descending order -- View the top-selling goods_id:
head(total.sales.by.goods[order(-total.sales.by.goods$goods_num), ])
summary(total.sales.by.goods$goods_num)
# ggplot(total.sales.by.goods, aes(x = goods_num)) + 
#   geom_histogram(aes(y = ..density..), binwidth = 0.5, colours = "black", fill = "white") + 
#   geom_density(alpha = 0.2, fill = "#FF6666")

## Calculate the number of 0.1% of total quantity sold among all goods as the threshhold for Top Selling Product:
sum(total.sales.by.goods$goods_num) * 0.001

mean(total.sales.by.goods$goods_num)
                                                                  
total.sales.by.goods %>% 
  filter(goods_num >= sum(total.sales.by.goods$goods_num) * 0.001) %>% 
  View()

## Add a new column to categorize all goods_id if they are top selling goods or not:
## 1 -- Top Selling Product, if a product's total quantity sold among the year is greater or equal than 0.1% of the total quantity sold among all goods; 
## 0 -- not a Top Selling Product, otherwise.
total.sales.by.goods$top_selling_goods <- ifelse(total.sales.by.goods$goods_num >= sum(total.sales.by.goods$goods_num) * 0.001, 
                                 1, 0)

total.sales.by.goods %>%
  filter(top_selling_goods == 1) %>% 
  dim()

str(total.sales.by.goods)

head(total.sales.by.goods)

## Export dataframes for future use:
save(total.sales.by.goods, file = "./data/total.sales.by.goods_df.Rda")
write.csv(total.sales.by.goods, file = "./data/total.sales.by.goods_df.csv")


## Join two datasets for the sales prediction model:
salesDailyAll <- merge(salesDaily, total.sales.by.goods[ , c("goods_id", "top_selling_goods")], by = "goods_id")

head(salesDailyAll)
str(salesDailyAll)

salesDailyAll <- as_tibble(salesDailyAll)

## Export dataframes for future use:
save(salesDailyAll, file = "./data/salesDailyAll_tbl.Rda")   ## This is the master dataset for linear regression modeling.


rm(salesDaily)
gc()


count(salesDailyAll$top_selling_goods)


## Subset the daily sales data of August 2017 as it has more sales than 50% of other months:
salesAugust <- salesDailyAll %>%
  filter(data_year == 2017 & data_month == 8)

str(salesAugust)
salesAugust <- as_tibble(salesAugust)
str(salesAugust)

## Export dataframes for future use:  
save(salesAugust, file = "./data/salesAugust_tbl.Rda")

rm(salesDailyAll, total.sales.by.goods)
gc()


str(goodsdaily)
goodsdaily$data_date <- as_date(as.character(goodsdaily$data_date))
str(goodsdaily)

## Export dataframes for future use:  
save(goodsdaily, file = "./data/goodsdaily_tbl.Rda")


## Subset all goodsdaily details in August 2017:
goodsdailyAugust <- goodsdaily %>% 
  filter(data_date >= "2017-08-01" & data_date <= "2017-08-31")
head(goodsdailyAugust)
summary(goodsdailyAugust$data_date)

## Export dataframes for future use:  
save(goodsdailyAugust, file = "./data/goodsdailyAugust_tbl.Rda")

rm(goodsdaily)
gc()


## Join two datasets for the sales prediction model:
salesAugust.details <- inner_join(salesAugust, goodsdailyAugust, match = c("data_date", "goods_id"))

str(salesAugust.details)

## Check number of missing values:
map_dbl(salesAugust.details, ~sum(is.na(.)))

count(salesAugust.details$goods_id)
count(salesAugust.details$goods_season)
count(salesAugust.details$top_selling_goods)

## Export dataframes for future use:  
save(salesAugust.details, file = "./data/salesAugust.details_tbl.Rda")   ## This is the Auguest sales dateset for classification modeling.


rm(goodsdailyAugust, salesAugust)
gc()

# rm(salesAug.trainData, salesAug.testData)
# gc()


## ------ Data Visualization ------
require(colorspace)
require(scales)

str(top.sales.by.goods.revenue)

## A barplot showing the Total Gross Revenue generated by the Top Six goods_id among All Time:
plot.top.revenue.by.goods <- top.sales.by.goods.revenue %>%
  ggplot(aes(x = goods_id, y = gross_sales)) + coord_flip() +
  labs(title = "Gross Revenue of The Top Six Contributing Goods, All Time", x ="") +
  geom_bar(stat = "identity", aes(fill = goods_id)) + 
  scale_fill_manual(values = c("#39BEB1", "#E69F00", "#56B4E9", "#f74842", "#fce00a", "#E495A5")) +
  theme(axis.title = element_blank()) + scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(color = "dark blue", size = 10))

ggsave(filename = "plot_top_revenue_by_goods.png", plot = plot.top.revenue.by.goods)


## Barplots showing the Total Quantity Sold of Six Top-Revenue goods_id, grouped by goods_season:
plot.total.qty.by.top.goods.season <- top.sales.by.goods.revenue %>%
  ggplot(aes(x = goods_id)) +
  geom_bar(aes(fill = goods_id)) + 
  scale_fill_manual(values = c("#39BEB1", "#E69F00", "#56B4E9", "#f74842", "#fce00a", "#E495A5")) +
  theme(axis.title = element_blank()) + scale_y_continuous(labels = comma) + 
  facet_wrap(~goods_season) + labs(title = "Total Quantity Sold of Six Top-Revenue Goods by Goods Season Code") +
  theme(plot.title = element_text(color = "dark blue", size = 10))

ggsave(filename = "plot_total_qty_by_top_goods_season.png", plot = plot.total.qty.by.top.goods.season)



## ------ Store The Datasets ------
str(goodsale)
str(goodsdaily)
str(goodsinfo)
str(count.alldaily.goods_id)
str(top.sales.by.goods.revenue)
str(gross.revenue.by.goods)
str(gross.revenue.by.sku)


## Save datasets to Rda file for loading later:
getwd()
setwd("./data")
getwd()
save(goodsale, file = "goodsale_tbl.Rda")

save(goodsinfo, file = "goodsinfo_tbl.Rda")
save(count.alldaily.goods_id, file = "alldaily.goods.count_df.Rda")
save(top.sales.by.goods.revenue, file = "daily.sales.by.top.good.revenue_tbl.Rda")   ## This is the dataset of top 6 goods_id for classification modeling.
save(gross.revenue.by.goods, file = "gross.revenue.by.goods_df.Rda")
save(gross.revenue.by.sku, file = "gross.revenue.by.sku_df.Rda")


## Remove unuseful objects and clean the memory:
rm(goodsale.comb, count.alldaily.goods_id, goodsdaily)
rm(plot.top.revenue.by.goods, plot.total.qty.by.top.goods.season, top.profitable.goods, top.profitable.sku, 
   total.sales.by.goods, total.sales.by.sku, gross.revenue.by.goods, gross.revenue.by.sku)
gc()




## ------ Data Modeling ------

getwd()


## Load previously saved datasets:
load("./data/salesAugust.details_tbl.Rda")
load("./data/salesDailyAll_tbl.Rda")
# load("daily.sales.by.top.good.revenue_tbl.Rda")


str(salesAugust.details)
# ## View the correlation between variables of August Sales:
# ggpairs(data = salesAugust.details, columns = c("goods_num", "goods_price", "orginal_shop_price", "discount_rate", "data_day", 
#                                                 "goods_click", "cart_click", "favorites_click", "onsale_days"), 
#         title = "Correlations of August Sales Variables")
# ## Data size is too large -- system hangs.

str(salesDailyAll)


# str(top.sales.by.goods.revenue)
# count(top.sales.by.goods.revenue$sku_id)   ## There were total 65 sku_id of all 6 top-revenue goods_id sold in the given time period.
# summary(top.sales.by.goods.revenue$gross_sales)


# ## View the correlation between variables of the sales of 6 Top-Revenue goods_id:
# ggpairs(data = top.sales.by.goods.revenue, columns = c("goods_num", "goods_price", "orginal_shop_price", "discount_rate", "gross_sales", "data_day"), 
#         title = "Total Sales of Six Top Revenue Goods")




## ------ 1. Top Selling Product Prediction ------

## ------ 1.1 Classification Model: Decision Tree ------

## Set the seed of R‘s random number generator, which is useful for creating simulations or random objects that can be reproduced.
set.seed(456)   ## Use for the salesAugust.details dataset sampling.

str(salesAugust.details)

## goods_season definitions:
## 1 = "Spring", 2 = "Summer", 3 = "Fall", 4 = "Winter", 5 = "Spring-Summer", 6 = "Spring-Fall", 7 = "Spring-Winter",
## 8 = "Summer-Fall", 9 = "Sunmmer-Winter", 10 = "Fall-Winter", 0 = "All-Season"

## Convert some columns from integer to factor:
salesAugust.details$goods_season <- as.factor(salesAugust.details$goods_season)
salesAugust.details$top_selling_goods <- as.factor(salesAugust.details$top_selling_goods)

str(salesAugust.details)

## Split the dataset into two random samples: 80% for training and 20% for testing by generating random numbers for each observation:
salesAug.s <- sample(2, nrow(salesAugust.details), replace = TRUE, prob = c(0.8, 0.2))

## Assign the two sets of random data into training and testing data by the random number they belong to:
salesAug.trainData <- salesAugust.details[salesAug.s == 1, ]
salesAug.testData <- salesAugust.details[salesAug.s == 2, ]

## Print the sizes of the training dataset and test dataset:
dim(salesAug.trainData)
dim(salesAug.testData)

str(salesAug.trainData)
str(salesAug.testData)

## Verify if the randomization process is correct by using the function prop.table() combined with table():
prop.table(table(salesAug.trainData$top_selling_goods))
prop.table(table(salesAug.testData$top_selling_goods))


## Export the training dataset and test dataset of August 2017 sales for future use:
save(salesAug.trainData, file = "./data/salesAug.trainData_tbl.Rda")
save(salesAug.testData, file = "./data/salesAug.testData_tbl.Rda")


rm(salesAugust.details, salesAug.s)
gc()


## Formulate the classification model:
topSeller.clasFormula <- top_selling_goods ~ goods_price + orginal_shop_price + goods_season + 
  goods_click + cart_click + favorites_click + onsale_days


## ------ 1.1.1 Using the "party" package to predict top selling product based on August 2017 sales data ------

require(party)

## Train the date for prediction:
topSeller_ctree <- ctree(topSeller.clasFormula, data = salesAug.trainData)


## Display the accuracy of the trained data model:
topSeller.trainPred.ctr <- predict(topSeller_ctree, newdata = salesAug.trainData, type = "response")
## Create confusion matrix of training prediction:
table(topSeller.trainPred.ctr, salesAug.trainData$top_selling_goods)
## Calculate the accuracy rate of the model on training data:
mean(topSeller.trainPred.ctr != salesAug.trainData$top_selling_goods) * 100   
## Misclassification Errors Rate: 0.08686049%
## View the decision tree:
print(topSeller_ctree)

CrossTable(x = salesAug.trainData$top_selling_goods, y = topSeller.trainPred.ctr, prop.chisq = FALSE)
confusionMatrix(topSeller.trainPred.ctr, salesAug.trainData$top_selling_goods)


## Load the testing data into the decison tree model:
topSeller.testPred.ctr <- predict(topSeller_ctree, newdata = salesAug.testData, type = "response")
## Create confusion matrix of testing prediction:
table(topSeller.testPred.ctr, salesAug.testData$top_selling_goods)
## Calculate the accuracy rate of the model on training data:
mean(topSeller.testPred.ctr != salesAug.testData$top_selling_goods) * 100   
## Misclassification Errors Rate: 0.1115303%

CrossTable(x = salesAug.testData$top_selling_goods, y = topSeller.testPred.ctr, prop.chisq = FALSE)
confusionMatrix(topSeller.testPred.ctr, salesAug.testData$top_selling_goods)


## Export the topSeller_ctree model for reuse:
saveRDS(topSeller_ctree, file = "./models/topSeller_ctree.Rds")

## Remove the ctree model after saving it into Rds file to release memory:
rm(topSeller_ctree, topSeller.trainPred.ctr, topSeller.testPred.ctr)
gc()


## ------ 1.1.2 Using the "rpart" package to predict top selling product based on August 2017 sales data ------

require(rpart)

## Decision Tree Model using rpart():
topSeller_rpart <- rpart(topSeller.clasFormula, data = salesAug.trainData, method = "class")
print(topSeller_rpart)


topSeller.trainPred.rpart <- predict(topSeller_rpart, salesAug.trainData, type = "class")
table(topSeller.trainPred.rpart, salesAug.trainData$top_selling_goods)
mean(topSeller.trainPred.rpart != salesAug.trainData$top_selling_goods) * 100
## Misclassification Errors Rate: 0.1623464%

CrossTable(x = salesAug.trainData$top_selling_goods, y = topSeller.trainPred.rpart, prop.chisq = FALSE)
confusionMatrix(topSeller.trainPred.rpart, salesAug.trainData$top_selling_goods)


topSeller.testPred.rpart <- predict(topSeller_rpart, salesAug.testData, type = "class")
table(topSeller.testPred.rpart, salesAug.testData$top_selling_goods)
mean(topSeller.testPred.rpart != salesAug.testData$top_selling_goods) * 100
## Misclassification Errors Rate: 0.2041988%

CrossTable(x = salesAug.testData$top_selling_goods, y = topSeller.testPred.rpart, prop.chisq = FALSE)
confusionMatrix(topSeller.testPred.rpart, salesAug.testData$top_selling_goods)


## Export the topSeller_rpart model for reuse:
saveRDS(topSeller_rpart, file = "./models/topSeller_rpart.Rds")


rm(topSeller_rpart, topSeller.trainPred.rpart, topSeller.testPred.rpart)
gc()




## ------ 1.3 Classification Model: kNN ------

## Load the previously exported dataset for normalization and modeling:
load("./data/salesAugust.details_tbl.Rda")

require(class)

str(salesAugust.details)

## Get the index of all columns:
as.data.frame(colnames(salesAugust.details))

## Create a function to normalize the August dataset for kNN modeling:
normFun <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


## Subset the August Sales Details dataset with selected predictors:
salesAugust.details.pd <- subset(salesAugust.details, 
                                 select = c(goods_price, orginal_shop_price, goods_season, 
                                            goods_click, cart_click, favorites_click, onsale_days, top_selling_goods))

rm(salesAugust.details)
gc()

str(salesAugust.details.pd)

## Get the index of each column:
as.data.frame(colnames(salesAugust.details.pd))

## Normalize only the numeric columns of predictors using the function defined eariler:
salesAugust.details.pd.nor <- as.data.frame(lapply(salesAugust.details.pd[, c(1, 2, 4, 5, 6, 7)], normFun))
head(salesAugust.details.pd.nor)

## Add the categorical variables back as factors:
salesAugust.details.pd.nor$goods_season <- as.factor(salesAugust.details.pd$goods_season)
salesAugust.details.pd.nor$top_selling_goods <- as.factor(salesAugust.details.pd$top_selling_goods)   ## This the the classification label.


str(salesAugust.details.pd.nor)


## Export the normalized dataset for future use:
save(salesAugust.details.pd.nor, file = "./data/salesAugust.details.pd.nor_df.Rda")


## Set the seed of R‘s random number generator, which is useful for creating simulations or random objects that can be reproduced.
set.seed(789)   ## Use for the salesAugust.details dataset sampling.


## Split the dataset into two random samples: 80% for training and 20% for testing by generating random numbers for each observation:
sApd.s <- sample(2, nrow(salesAugust.details.pd.nor), replace = TRUE, prob = c(0.8, 0.2))

## Assign the two sets of random data into training and testing data by the random number they belong to:
salesAugNor.trainData <- salesAugust.details.pd.nor[sApd.s == 1, ]
salesAugNor.testData <- salesAugust.details.pd.nor[sApd.s == 2, ]

## Print the sizes of the training dataset and test dataset:
dim(salesAugNor.trainData)
dim(salesAugNor.testData)

str(salesAugNor.trainData)
str(salesAugNor.testData)

## Verify if the randomization process is correct by using the function prop.table() combined with table():
prop.table(table(salesAugNor.trainData$top_selling_goods))
prop.table(table(salesAugNor.testData$top_selling_goods))


## Export the training dataset and test dataset of August 2017 sales for future use:
save(salesAugNor.trainData, file = "./data/salesAugNor.trainData_df.Rda")
save(salesAugNor.testData, file = "./data/salesAugNor.testData_df.Rda")


rm(salesAugust.details.pd, salesAugust.details.pd.nor, sApd.s)
gc()

topSeller.testPred.knn <- knn(train = salesAugNor.trainData, test = salesAugNor.testData, 
                              cl = salesAugNor.trainData$top_selling_goods, k = 20)

## Evaluate the model performance:
table(topSeller.testPred.knn, salesAugNor.testData$top_selling_goods)
mean(topSeller.testPred.knn != salesAugNor.testData$top_selling_goods) * 100
## Misclassification Errors Rate: 0%

CrossTable(x = salesAugNor.testData$top_selling_goods, y = topSeller.testPred.knn, prop.chisq = FALSE)
confusionMatrix(topSeller.testPred.knn, salesAugNor.testData$top_selling_goods)


## Export the kNN test prediction result in case need to reuse later as it took long time to predict:
save(topSeller.testPred.knn, file = "./data/topSeller.testPred.knn.Rda")




## ------ 1.3 Classification Model: Random Forest (Unsuccessful Run due to memory) ------

require(party)
require(randomForest)

# topSeller_rf <- randomForest(topSeller.clasFormula, data = salesAug.trainData)
# ## System Error: vector memory exhausted (limit reached?). 

# print(topSeller_rf)
# print(importance(topSeller_rf, type = 2))   
# ## importance() type: either 1 or 2, specifying the type of importance measure (1 = mean decrease in accuracy, 2 = mean decrease in node impurity).
# 
# topSeller.trainPred.rf <- predict(topSeller_rf, newdata = salesAug.trainData)
# table(topSeller.trainPred.rf, salesAug.trainData$top_selling_goods)
# mean(topSeller.trainPred.rf != salesAug.trainData$top_selling_goods) * 100
# ## Misclassification Errors Rate:
# 
# topSeller.testPred.rf <- predict(topSeller_rf, newdata = salesAug.testData)
# table(topSeller.testPred.rf, salesAug.testData$top_selling_goods)
# mean(topSeller.testPred.rf != salesAug.testData$top_selling_goods) * 100
# ## Misclassification Errors Rate:
# 
# ## Export the topSeller_rf model for reuse:
# saveRDS(topSeller_rf, file = "./model/topSeller_rf.Rds")
# 
# 
# ## Remove the randomforest model after saving it into Rds file to release memory:




###############################################



## Export Factor columns as Rda files so they can be loaded into the App because the training dataset is too large for publishing the Shinny App.
goods_seasonFactor <- salesAug.trainData$goods_season
levels(goods_seasonFactor) <- levels(salesAug.trainData$goods_season)
levels(goods_seasonFactor)

top_selling_goodsFactor <- salesAug.trainData$top_selling_goods
levels(top_selling_goodsFactor) <- levels(salesAug.trainData$top_selling_goods)
levels(top_selling_goodsFactor)

save(goods_seasonFactor, file = "./data/goods_seasonFactor.Rda")
save(top_selling_goodsFactor, file = "./data/top_selling_goodsFactor.Rda")


rm(salesAug.trainData)
gc()




## ------ 2. Product Demand Forecasting ------


## ------ 2.1 Multiple Linear Regression Model: predicting the product demand based on August 2017 sales data ------

require(graphics)
qtyAug_lm <- lm(formula = qtyAug.regFormula, data = salesAug.trainData)
summary(qtyAug_lm)

qtyAug.trainPred.lm <- predict(qtyAug_lm, newdata = salesAug.trainData)
summary(qtyAug.trainPred.lm)
## Compute the Root Mean Square Error (RMSE):
rmse(qtyAug.trainPred.lm, salesAug.trainData$goods_num)   ## RMSE: 3.215114

qtyAug.testPred.lm <- predict(qtyAug_lm, newdata = salesAug.testData)
summary(qtyAug.testPred.lm)
## Compute the Root Mean Square Error (RMSE):
rmse(qtyAug.testPred.lm, salesAug.testData$goods_num)   ## RMSE: 3.383022


## Export the qtyAug_lm regression tree model for reuse:
saveRDS(qtyAug_lm, file = "./models/qtyAug_lm.Rds")


rm(qtyAug_lm, qtyAug.trainPred.lm, qtyAug.testPred.lm)
gc()




## ------ 2.2. Regression Tree Model: predicting the product demand based on August 2017 sales data ------

require(rpart)
qtyAug.regFormula <- goods_num ~ goods_price + orginal_shop_price + goods_season + 
  goods_click + cart_click + favorites_click + onsale_days

qtyAug_regTree_rpart <- rpart(qtyAug.regFormula, data = salesAug.trainData, method = "anova")
print(qtyAug_regTree_rpart)

qtyAug.trainPred.reg.rpart <- predict(qtyAug_regTree_rpart, salesAug.trainData)
summary(qtyAug.trainPred.reg.rpart)
## Compute the Root Mean Square Error (RMSE):
rmse(qtyAug.trainPred.reg.rpart, salesAug.trainData$goods_num)   ## RMSE: 3.015905

qtyAug.testPred.reg.rpart <- predict(qtyAug_regTree_rpart, salesAug.testData)
summary(qtyAug.testPred.reg.rpart)
## Compute the Root Mean Square Error (RMSE):
rmse(qtyAug.testPred.reg.rpart, salesAug.testData$goods_num)   ## RMSE: 3.090346


## Export the qtyAug_regTree_rpart regression tree model for reuse:
saveRDS(qtyAug_regTree_rpart, file = "./models/qtyAug_regTree_rpart.Rds")


rm(qtyAug.regFormula, qtyAug_regTree_rpart, qtyAug.trainPred.reg.rpart, qtyAug.testPred.reg.rpart)
gc()





## Set the seed of R‘s random number generator, which is useful for creating simulations or random objects that can be reproduced.
set.seed(123)   ## Use for the salesDailyAll dataset sampling.


## Split the dataset into two random samples: 80% for training and 20% for testing by generating random numbers for each observation:
salesAll.s <- sample(2, nrow(salesDailyAll), replace = TRUE, prob = c(0.8, 0.2))

## Assign the two sets of random data into training and testing data by the random number they belong to:
salesAll.trainData <- salesDailyAll[salesAll.s == 1, ]
salesAll.testData <- salesDailyAll[salesAll.s == 2, ]

dim(salesAll.trainData)
dim(salesAll.testData)

str(salesAll.trainData)
str(salesAll.testData)

## Save sample datasets:
save(salesAll.trainData, file = "./data/salesAll.trainData_tbl.Rda")
save(salesAll.testData, file = "./data/salesAll.testData_tbl.Rda")


rm(salesDailyAll, salesAll.s)
gc()




## ------ 2.3 Multiple Linear Regression Model:  predicting the product demand based on the sales data of all time ------

require(graphics)
qty_lm <- lm(formula = goods_num ~ goods_price + orginal_shop_price + goods_season + data_day, data = salesAll.trainData)
summary(qty_lm)

qty.trainPred.lm <- predict(qty_lm, newdata = salesAll.trainData)
summary(qty.trainPred.lm)
## Compute the Root Mean Square Error (RMSE):
rmse(qty.trainPred.lm, salesAll.trainData$goods_num)   ## RMSE: 5.317377

qty.testPred.lm <- predict(qty_lm, newdata = salesAll.testData)
summary(qty.testPred.lm)
## Compute the Root Mean Square Error (RMSE):
rmse(qty.testPred.lm, salesAll.testData$goods_num)   ## RMSE: 4.940055


## Test the linear regression model with makeup numbers:
predict(qty_lm, data.frame(goods_price = 15.99, orginal_shop_price = 20.99, goods_season = 2, data_day = 23))


## Export the qty_lm regression tree model for reuse:
saveRDS(qty_lm, file = "./models/qty_lm.Rds")


rm(qty_lm, qty.trainPred.lm, qty.testPred.lm)
gc()




## ------ 2.4 Regression Tree Model:  predicting the product demand based on the sales data of all time ------

## Use the rpart package:
require(rpart)
qty.regFormula <- goods_num ~ goods_price + orginal_shop_price + goods_season + data_day
qty_regTree_rpart <- rpart(qty.regFormula, data = salesAll.trainData, method = "anova")
print(qty_regTree_rpart)

qty.trainPred.reg.rpart <- predict(qty_regTree_rpart, salesAll.trainData)
summary(qty.trainPred.reg.rpart)
## Compute the Root Mean Square Error (RMSE):
rmse(qty.trainPred.reg.rpart, salesAll.trainData$goods_num)   ## RMSE: 5.309253

qty.testPred.reg.rpart <- predict(qty_regTree_rpart, salesAll.testData)
summary(qty.testPred.reg.rpart)
## Compute the Root Mean Square Error (RMSE):
rmse(qty.testPred.reg.rpart, salesAll.testData$goods_num)   ## RMSE: 4.931921


## Export the qty_regTree_rpart regression tree model for reuse:
saveRDS(qty_regTree_rpart, file = "./models/qty_regTree_rpart.Rds")


rm(qty_regTree_rpart, qty.trainPred.reg.rpart, qty.testPred.reg.rpart)
gc()

rm(salesAll.s, salesAll.trainData, salesAll.testData)
gc()





#####################################################


## Experiment of two Decision Tree Packages:
## Compare the accuracies of Decision Trees using "party" and "rpart" packages:
s1 <- salesAugust %>% 
  filter(data_day <= 7)

dim(s1)
str(s1)
## party:
mean(predict(goods_season_ctree, newdata = s1, type = "response") != s1$goods_season) * 100

## rpart:
require(rpart)
fit.rp <- rpart(goods_season ~., data = s1, method = "class")
pred.rp <- predict(fit.rp, s1, type = "class")
mean(pred.rp != s1$goods_season) * 100


## Using rpart results in lower accuracies than party.

rm(s1, fit.rp, pred.rp, fit.rpt, test.pred.rpt)
gc()










