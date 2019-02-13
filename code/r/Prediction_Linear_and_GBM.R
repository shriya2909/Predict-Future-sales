
#Loading libraries to be used 
library(dplyr)
library(ggplot2)
library(lubridate)
library(gbm)

#input data files
sales_data = read.csv("C:/Users/Shriya/Desktop/Predict sales/sales_train_v2.csv")
test_data = read.csv("C:/Users/Shriya/Desktop/Predict sales/test.csv")
item_data = read.csv("C:/Users/Shriya/Desktop/Predict sales/items.csv")
shops_data = read.csv("C:/Users/Shriya/Desktop/Predict sales/shops.csv")
item_cat_data = read.csv("C:/Users/Shriya/Desktop/Predict sales/item_categories.csv")


sales_data = read.csv("C:/Users/Shriya/Desktop/Predict sales/sales_train_v2.csv")
sales_data = merge(sales_data, item_data[,c("item_id", "item_category_id")], by = "item_id", all.x = T)

glimpse(sales_data)

#Dividing date into subdivisions using lubridate library
sales_data$date = as.Date(sales_data$date, "%d.%m.%Y")
sales_data$year = year(sales_data$date)
sales_data$month = month(sales_data$date)
sales_data$day = day(sales_data$date)
sales_data$weekdays =  weekdays(sales_data$date)

#Turning above date columns into factors 
sales_data$year =  as.factor(sales_data$year)
sales_data$month = as.factor(sales_data$month)
sales_data$day = as.factor(sales_data$day)
sales_data$weekdays = as.factor(sales_data$weekdays)

#Factorising for each item category 
sales_data$item_category_id =  as.factor(sales_data$item_category_id)

glimpse(sales_data)

#ANOVA table for feature selection 
m0 <- lm(item_cnt_day~ 1, sales_data) #RSS - 20134908
m1 <- update(m0, ~ item_id) #RSS - 20129326
anova(m0,m1) 
m2 <-  update(m0, ~ item_category_id) #RSS - 19136309 
anova(m0,m2) 
m3<-  update(m0, ~ shop_id) #RSS -  20134357
anova(m0,m3) 
m4<-  update(m0, ~ day) #RSS - 20131792
anova(m0,m4)
m5<- update(m0, ~ weekdays)#RSS - 20131513 
anova(m0,m5)
m6<- update(m0, ~ month)#RSS - 20120007 
anova(m0,m6)
m7<- update(m0, ~ item_price)#RSS - 20132384
anova(m0,m7)
rm(m3,m4,m5,m6,m7)

# Picking item_category_id and next 2 best features 
m8 <- update(m2,~weekdays+month) #RSS - 20116528
anova(m2,m8)
rm(m8)
m9 <- update(m0,~item_id+shop_id) #RSS - 20128667
anova(m0,m9)
m10<- update(m0,~ month + item_id + weekdays + day)# RSS - 20107997
anova(m0,m10)
rm(m10)
m11<- update(m10,~ month + item_id + weekdays + day+shop_id) # RSS - 20107322
anova(m10,m11)

#Linear model for best suited model based on ANOVA table - m2 
summary(m2) #Residual standard error: 0.3668 Multiple R-squared:  0.3984
test_data = merge(test_data, item_data[,c("item_id", "item_category_id")], by = "item_id", all.x = T)

test_data$item_category_id = as.factor(test_data$item_category_id)
y_pred <- predict(m2, test_data, type="response")

submission =  data.frame(ID = test_data$ID,
                         item_cnt_month = y_pred)
head(submission)
write.csv(submission, file = "sub1.csv", row.names = F)

# Trying out another linear model
mexp = lm(item_cnt_day ~  item_id+shop_id , sales_data)
summary(mexp) #Multiple R-squared: 0.001059 Residual standard error 2.617
y_pred2 <- predict(mexp, test_data, type="response")

submission =  data.frame(ID = test_data$ID,
                         item_cnt_month = y_pred2)
head(submission)
write.csv(submission, file = "sub2.csv", row.names = F)

# Gradient Boost Model with item and shop 
start =  Sys.time()
gbm_model  =  gbm(item_cnt_day ~ shop_id + item_id,
                  data = sales_data,
                  shrinkage = 0.01,
                  distribution = "gaussian",
                  n.trees = 5000,
                  interaction.depth = 3, 
                  bag.fraction = 0.7,
                  train.fraction = 0.8,
                  # cv.folds = 5,
                  n.cores = NULL,
                  verbose = T)

end = Sys.time()
print(end - start)

result2 = predict(gbm_model,newdata = test_data[,c("shop_id","item_id")], n.trees = 5000 )

sub3 = data.frame(ID = test_data$ID, 
                  item_cnt_month =  result2)

write.csv(sub3, "sub3.csv", row.names = F)
rm(gbm_model)

#GBM with item category 
start =  Sys.time()
gbm_model_1 =  gbm(item_cnt_day ~ item_category_id,
                  data = sales_data,
                  shrinkage = 0.01,
                  distribution = "gaussian",
                  n.trees = 2000,
                  interaction.depth = 3, 
                  bag.fraction = 0.7,
                  train.fraction = 0.8,
                  n.cores = NULL,
                  verbose = T)

end = Sys.time()
print(end - start)

result3 = predict(gbm_model,newdata = test_data[,c("item_category_id")], n.trees = 2000 )

sub4= data.frame(ID = test_data$ID, 
                  item_cnt_month =  result2)

write.csv(sub4, "sub4.csv", row.names = F)
rm(gbm_model_1)

