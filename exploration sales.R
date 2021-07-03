rm=list(ls)
setwd("C:/Users/jayanth/Desktop/Data analytics lab")
data1 <- read.csv("sales_cleaned.csv")
data1 <- sales_cleaned
library(data.table) # used for reading and manipulation of data
library(plyr)
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(xgboost)    # used for building XGBoost model
library(cowplot)    # used for combini


table(data1$Outlet_Identifier , data1$Outlet_Size)
table(data1$Outlet_Identifier , data1$Outlet_Type)

table(data1$Outlet_Identifier , data1$Item_Weight)

table(data1$Outlet_Identifier , data1$Outlet_Location_Type)


table(data1$Outlet_Type , data1$Outlet_Size)

table(data1$Outlet_Type , data1$Item_Weight)


table(data1$Outlet_Type , data1$Outlet_Location_Type)


table(data1$Item_Identifier , data1$Item_Weight)


table(data1$Item_Weight , data1$Item_Fat_Content)


table(data1$Item_Type , data1$Item_Fat_Content)


table(data1$Item_Type , data1$Outlet_Type)


table(data1$Outlet_Location_Type , data1$Outlet_Size)


table(data1$Outlet_Identifier , data1$Item_Type)

df1 <- data1 %>% group_by(Outlet_Type )  %>% summarise(mean_outlet_sales = mean(Item_Outlet_Sales, na.rm=TRUE))

df2 <- data1 %>% group_by(Item_Type )  %>% summarise(mean_Item_MRP = mean(Item_MRP, na.rm=TRUE))

df3 <- data1 %>% group_by(Item_Type )  %>% summarise(max_Item_MRP = max(Item_MRP, na.rm=TRUE))

df4 <- data1 %>% group_by(Outlet_Location_Type )  %>% summarise(mean_outlet_sales = mean(Item_Outlet_Sales, na.rm=TRUE))

df5 <- data1 %>% group_by(Outlet_Identifier )  %>% summarise(mean_outlet_sales = mean(Item_Outlet_Sales, na.rm=TRUE))

df6 <- data1 %>% group_by(Outlet_Size )  %>% summarise(mean_outlet_sales = mean(Item_Outlet_Sales, na.rm=TRUE))

df7 <- data1 %>% group_by(Item_Fat_Content )  %>% summarise(Item_Weight = mean(Item_Weight, na.rm=TRUE))
