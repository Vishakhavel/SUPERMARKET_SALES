setwd("C:/Users/jayanth/Desktop/Data analytics lab")
library(dplyr)
library(plyr)
library(data.table)
library(caret)
library(VIM)
library(corrplot) 
install.packages("Metrics")
library(Metrics)
data<-read.csv("sales_cleaned.csv")
data %>% 
  select(Item_Weight, Item_Visibility, Item_MRP)
model = lm(Item_Outlet_Sales ~ Item_Visibility,data=data) 

summary(model)
Test<-read.csv("sales_test.csv")
test<-test[,Item_Outlet_Sales := NA]

Test[is.na(Test)] <- 0
Test$Item_Weight[Test$Item_Weight == 0]<- mean(Test$Item_Weight)

index <- which(Test$Item_Fat_Content == "LF" | 
                 Test$Item_Fat_Content == "low fat")

Test[index, "Item_Fat_Content"] <- "Low Fat"


#Transforming "reg" to "Regular
index2 <- which(Test$Item_Fat_Content == "reg")

Test[index2, "Item_Fat_Content"] <- "Regular"

table(Test$Outlet_Identifier, Test$Outlet_Size)
table(Test$Outlet_Identifier, Test$Outlet_Type)
table(Test$Outlet_Type, Test$Outlet_Size)

index3 <- which(Test$Outlet_Identifier == "OUT010")
Test[index3, "Outlet_Size"] <- "Small"
index4 <- which(Test$Outlet_Identifier == "OUT017")
Test[index4, "Outlet_Size"] <- "Small"
index5 <- which(Test$Outlet_Identifier == "OUT045")
Test[index5, "Outlet_Size"] <- "Small"
Test$Outlet_Size <- factor(Test$Outlet_Size)

Test %>%  select(Item_Weight, Item_Visibility, Item_MRP)
pred=predict.lm(model,Test)
actuals_preds1 <- data.frame(cbind(actuals=data$Item_Outlet_Sales, predicteds=pred))


pred1=scale(actuals_preds1$predicteds)
sales=scale(actuals_preds1$actuals)

summary(pred)

correlation_accuracy1 <- cor(actuals_preds1)

show(correlation_accuracy1)
accuracy<-sum(diag(correlation_accuracy1))/sum(correlation_accuracy1)
show(accuracy)

write.csv(Test,"sales_Test1.csv")




rmse(sales,pred1)
mse(sales,pred1)

#item weight
model = lm(Item_Outlet_Sales ~ Item_Weight,data=data) 
pred=predict.lm(model,Test)
actuals_preds1 <- data.frame(cbind(actuals=data$Item_Outlet_Sales, predicteds=pred))


pred1=scale(actuals_preds1$predicteds)
sales=scale(actuals_preds1$actuals)
rmse(sales,pred1)
mse(sales,pred1)

#item_mrp
model = lm(Item_Outlet_Sales ~ Item_MRP,data=data) 
pred=predict.lm(model,Test)
actuals_preds1 <- data.frame(cbind(actuals=data$Item_Outlet_Sales, predicteds=pred))


pred1=scale(actuals_preds1$predicteds)
sales=scale(actuals_preds1$actuals)
rmse(sales,pred1)
mse(sales,pred1)

#ALL
model = lm(Item_Outlet_Sales ~ .,data=data) 
pred=predict.lm(model,Test)
actuals_preds1 <- data.frame(cbind(actuals=data$Item_Outlet_Sales, predicteds=pred))


pred1=scale(actuals_preds1$predicteds)
sales=scale(actuals_preds1$actuals)
rmse(sales,pred1)
mse(sales,pred1)
