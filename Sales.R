#https://www.kaggle.com/jeetranjeet619/predict-future-sales-r
#https://www.kaggle.com/bhaveshthaker/data-preparation-exploratory-analysis-and-modeling
#Load in packages
library(tidyverse)
library(lubridate)
library(reshape2)
library(scales)
library(fpp2)
#install.packages("gbm")
library(tictoc)
library(gbm)
#Load in dataset, save in new variable, examine structure

#sales train data
sales_train <- sales_train_v2
str(sales_train)


#items data
str(items)


#Lets work with the above data first
sale_item_join <- sales_train %>% 
  left_join(items, by = "item_id")

#Fill name with null
sale_item_join$item_name <- NULL

str(sale_item_join)


#Remove the other datasets no longer needed
#rm("items", "sales_train")
#convert date column to date class
#use dmy from lubridate data
sale_item_join$date <- dmy(sale_item_join$date)

#Lets apply a variety of data to create year day and month columns
#that will give us insight in our data\
sale_item_join$year <- year(sale_item_join$date)
sale_item_join$month <- month(sale_item_join$date)
sale_item_join$day <- day(sale_item_join$date)
sale_item_join$weekday <- weekdays(sale_item_join$date)


#lets convert year and weekday to factor
sale_item_join$year <- as.factor(sale_item_join$year)
sale_item_join$weekday <- as.factor(sale_item_join$weekday)

#Lets examine how many products are sold monthly
monthly_count <- sale_item_join %>% 
  group_by(year, month, shop_id, item_id) %>% 
  summarise(item_count_month = sum(item_cnt_day)) %>% 
  ungroup()

#Left join with original data
summary_data <- sale_item_join %>% 
  left_join(monthly_count, by = c("year", "month", "shop_id", "item_id"))

str(summary_data)
summary(summary_data)

#lets examine if there are missing values
colSums(is.na(summary_data))
#There are no missing values

#examine for null data
is.null(summary_data
        )

#Exploratory data analysis
#correlation of all 
cor_data <- summary_data %>% 
  select(3:7, 12)

melt(cor(cor_data)) %>% 
  ggplot(., aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "grey", high = "darkred") +
  geom_text(aes(Var1, Var2, label = round(value,2)), size = 4) +
  labs(title = "Correlation Matrix", x = "Numeric column", y = "Numeric Column",
       fill = "Coefficient Range") +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
        plot.title = element_text(face = "bold", hjust = 0.5))

#How many distinct shops do we have
summary_data %>% 
  select(shop_id) %>% distinct() %>% 
  count()

#Examine the most popular shops and total sales of the shop
summary_data %>% 
  group_by(shop_id) %>% 
  summarise(item_count = sum(item_count_month)) %>% 
  ggplot(., aes(reorder(as.factor(shop_id), item_count), item_count, fill = as.factor(shop_id))) +
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() + 
  labs(title = "Shop sales", x = "Shop ID", ylab = "Item Count")



#Lets examine the amount of unique Item from various shops
summary_data %>% 
  select(item_id) %>% 
  distinct() %>% 
  count()


#Which shop has the most item in their shop
summary_data %>% 
  group_by(shop_id) %>% 
  summarise(item_count = n_distinct(item_id)) %>% 
  ggplot(.,aes(reorder(as.factor(shop_id), item_count), item_count, fill = as.factor(shop_id))) +
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() + 
  labs(title = "most Items Available at a shop", x = "Shop ID", ylab = "Item Count")



#Most popular item sold in each shop
summary_data %>% 
  group_by(shop_id, item_id) %>% 
  summarise(items_count = sum(item_cnt_day)) %>% 
  filter(items_count == max(items_count)) %>% 
  arrange(desc(items_count)) %>% 
  ungroup() %>% 
  ggplot(., aes(reorder(as.factor(shop_id), items_count), items_count, fill = as.factor(item_id))) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "Shop sales", x = "Shop ID", ylab = "Item Count",
       fill = "Item Id")


#How many unique categories of Item are there
summary_data %>% 
  select(item_category_id) %>% 
  distinct() %>% 
  count()


#Which shop has the most item categoriees
summary_data %>% 
  group_by(shop_id) %>% 
  summarise(category = n_distinct(item_category_id)) %>% 
  ggplot(.,aes(reorder(factor(shop_id), category), category, fill = factor(shop_id))) +
  geom_bar(stat = "identity") + coord_flip() +
  ylab("Total Item categories at shop") +
  xlab("Shop Id") + ggtitle("Shop with most Item categories")


#which Item is most popular and most sold at the shop
summary_data %>% 
  group_by(shop_id, item_category_id) %>% 
  summarise(category = sum(item_cnt_day)) %>%
  filter(category == max(category)) %>% 
  arrange(desc(category)) %>% 
  ggplot(., aes(reorder(as.factor(shop_id), category), category, fill = as.factor(item_category_id))) +
  geom_bar(stat = "identity") + coord_flip() +
  ylab("Number of Item sales") +
  xlab("Item ID") +
  ggtitle("Most Popular and Most Sold at the Shop")



#Highest grossing product
summary_data %>% 
  group_by(item_category_id) %>% 
  summarise(totalgross = sum(item_price*item_cnt_day)) %>% 
  arrange(desc(totalgross)) %>% 
  ungroup() %>% 
  ggplot(.,aes(reorder(factor(item_category_id), totalgross), totalgross,
               fill = factor(item_category_id))) +
  geom_bar(stat = "identity") + coord_flip()


#which item gets sold most and under which category
summary_data %>% 
  group_by(item_category_id, item_id) %>% 
  summarise(product = sum(item_cnt_day*item_price)) %>% 
  filter(product == max(product)) %>% 
  arrange(desc(product)) %>% 
  ungroup() %>% 
  ggplot(., aes(reorder(factor(item_category_id), product), product, fill = factor(item_id))) +
  geom_bar(stat = "identity") + coord_flip()



#What are the day and month wise sales
summary_data %>% 
  group_by(month, day) %>% 
  summarise(sales = sum(item_cnt_day*item_price)) %>% 
  ungroup() %>% 
  ggplot(.,aes(factor(day), sales, col = factor(month))) +
  geom_line(aes(group = month)) + geom_point() +
  labs(title = "Day - Month sales", fill = "Month")


#Individual plot for Day-Month Sales
summary_data %>% 
  group_by(month, day) %>% 
  summarise(sales = sum(item_cnt_day*item_price)) %>% 
  ungroup() %>% 
  ggplot(.,aes(factor(day), sales, fill = as.factor(day))) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~month, ncol = 2) +
  labs(title = "Day - Month wise sales")


#Total year sales
summary_data %>% 
  group_by(year) %>% 
  summarise(sales = sum(item_cnt_day*item_price)) %>% 
  ungroup() %>% 
  ggplot(., aes(year, sales, fill = year)) +
  geom_bar(stat = "identity") 

#What are the year month wise sales
summary_data %>% 
  group_by(year, month) %>% 
  summarise(sales = sum(item_cnt_day*item_price)) %>% 
  ungroup() %>% 
  ggplot(.,aes(year, sales, fill = factor(month))) +
  geom_bar(stat = "identity")


#What percentage of items is sold per month
#get daily pricing 
summary_data$price <- summary_data$item_price*summary_data$item_cnt_day
#total number of items sold
items_sold <- sum(summary_data$item_cnt_day)
item_revenue <- sum(summary_data$price)


#Percentage of items sold daily
summary_data %>% 
  group_by(date_block_num) %>% 
  summarise(monthly_sales_freq = round(sum(item_cnt_day)/items_sold, 3)) %>% 
  ungroup() %>% 
  ggplot(.,aes(x = "", y = monthly_sales_freq, fill = factor(date_block_num))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  geom_col(position = "fill") + 
  geom_label(aes(label = paste0(monthly_sales_freq *100, "%")), position = 
               position_fill(vjust = 0.5)) +
  labs(x = "", y = "Monthly Item Frequency Sales", fill =  "Months",
       title = "% of Items sold monthly")


#Items sold daily
summary_data %>% 
  group_by(date) %>% 
  summarise(items_daily = sum(item_cnt_day)) %>% 
  ungroup() %>% 
  ggplot(., aes(date, items_daily, color = items_daily)) +
  geom_line() + geom_point(size = 0.24) +
  labs(x = "Date", y = "Daily Sales", fill = "Items_Sold")


#Which dat got the highest sales, lete examine the  outliers
summary_data %>% 
  group_by(date) %>% 
  summarise(items_daily = sum(item_cnt_day * item_price)) %>% 
  arrange(desc(items_daily)) %>% 
  ungroup() %>% 
  ggplot(.,aes(date, items_daily)) +
  geom_point(na.rm = T, color = "darkred", size = 0.5) +
  (scale_x_date(breaks = date_breaks("9 months"), labels = date_format("%b %y")))


#Items sold in a particular weekday
summary_data %>% 
  group_by(weekday) %>% 
  summarise(weeksales = sum(item_cnt_day)) %>% 
  ungroup() %>% 
  arrange(desc(weeksales)) %>% 
  ggplot(., aes(reorder(factor(weekday),weeksales), weeksales, fill = factor(weekday))) +
  geom_bar(stat = "identity") + coord_flip()


#Items revenue per weekday
summary_data %>% 
  group_by(weekday) %>% 
  summarise(weeksales = sum(item_cnt_day*item_price)) %>% 
  ungroup() %>% 
  arrange(desc(weeksales)) %>% 
  ggplot(., aes(reorder(factor(weekday),weeksales), weeksales, fill = factor(weekday))) +
  geom_bar(stat = "identity") + coord_flip()


#EDA is done, lets try and forecast sales

library(fpp2)
#Lets summarise data into monthly data
total_yearly_sales <- summary_data %>% 
  group_by(date_block_num) %>% 
  summarise(sales = sum(item_cnt_day))


#Examine for seasonality
plot(decompose(ts(total_yearly_sales$sales, frequency = 12), type = "multiplicative"))

#We see a seaonal component
#Using models that remove seaonality and as well forecast sales
auto.arima(total_yearly_sales$sales)

fit <- Arima(total_yearly_sales$sales, order = c(0,1,1))    
checkresiduals(fit)
autoplot(forecast(fit))


#Build regression model
fit <- lm(data = summary_data,
   item_cnt_day ~ shop_id, item_id)

#use train dataset
prediction <- predict(fit, test[,c("shop_id", "item_id")])

#prediction table
sale_train_pred <- data.frame(prediction, test$item_cnt_day)






##Use a gbm
tic("Time Taken to Run GBM Model ")
gbm_model  =  gbm(item_cnt_day ~ shop_id + item_id,
                  data = summary_data,
                  shrinkage = 0.01,
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 5, 
                  bag.fraction = 0.5,
                  train.fraction = 0.8,
                  # cv.folds = 5,
                  n.cores = -1,
                  verbose = T)

#Use train (summary_data) for initial test
result2 = predict(gbm_model,newdata = summary_data[,c("shop_id","item_id")], n.trees = 1000)

sub2 = data.frame(ID = summary_data$item_cnt_day, 
                  item_cnt_month =  result2)




#Use test data
result2 = predict(gbm_model,newdata = test[,c("shop_id","item_id")], n.trees = 1000)

sub2 = data.frame(ID = test$ID, 
                  item_cnt_month =  result2)

