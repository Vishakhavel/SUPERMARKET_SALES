

library(readr)
sales_Test1 <- read_csv("Desktop/SEM 6/EDA THEORY/sales_Test1.csv")
View(sales_Test1)
#Applying RandomForest Algorithm for predicting RMSE factor and the importance order of variables 

library(randomForest)
 set.seed(123)

 
 
 train = sales_cleaned
#Converting all character data variables into factors, because we're doing regression to predict a continoues prediction.

train$Item_Type <- as.factor(train$Item_Type)
train$Outlet_Identifier <- as.factor(train$Outlet_Identifier)
train$Outlet_Size <- as.factor(train$Outlet_Size)
train$Outlet_Location_Type <- as.factor(train$Outlet_Location_Type)
train$Outlet_Type <- as.factor(train$Outlet_Type)
train$Item_Fat_Content <- as.factor(train$Item_Fat_Content)


 rf <- randomForest(Item_Outlet_Sales ~  Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type + Item_MRP + Outlet_Identifier + Outlet_Establishment_Year + Outlet_Size +Outlet_Location_Type + Outlet_Type, data = train, importance = TRUE, ntree=1000)
 
 
 test1= sales_Test1
 
 
 
 test1$Item_Type <- as.factor(test1$Item_Type)
 test1$Outlet_Identifier <- as.factor(test1$Outlet_Identifier)
 test1$Outlet_Size <- as.factor(test1$Outlet_Size)
 test1$Outlet_Location_Type <- as.factor(test1$Outlet_Location_Type)
 test1$Outlet_Type <- as.factor(test1$Outlet_Type)
 test1$Item_Fat_Content <- as.factor(test1$Item_Fat_Content)
 
prediction <- predict(rf,test1) 

# csv 
Item_Outlet_Sales<-prediction
Item_Identifier<-test1$Item_Identifier
Outlet_Identifier<-test1$Outlet_Identifier

rf1<-data.frame(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
write.csv(rf1,"rf1.csv")

print(prediction)

# increasing ntrees to 1500

rf <- randomForest(Item_Outlet_Sales ~  Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type + Item_MRP + Outlet_Identifier + Outlet_Establishment_Year + Outlet_Size +Outlet_Location_Type + Outlet_Type, data = train, importance = TRUE, ntree=1500)

prediction_ntree_inc <- predict(rf,test1) 



# csv 
Item_Outlet_Sales<-prediction_ntree_inc
Item_Identifier<-test1$Item_Identifier
Outlet_Identifier<-test1$Outlet_Identifier

rf2<-data.frame(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
write.csv(rf2,"rf2.csv")



#decreasing the ntrees to 500
rf <- randomForest(Item_Outlet_Sales ~  Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type + Item_MRP + Outlet_Identifier + Outlet_Establishment_Year + Outlet_Size +Outlet_Location_Type + Outlet_Type, data = train, importance = TRUE, ntree=500)

prediction_ntree_dec <- predict(rf,test1) 


# csv 
Item_Outlet_Sales<-prediction_ntree_dec
Item_Identifier<-test1$Item_Identifier
Outlet_Identifier<-test1$Outlet_Identifier

rf3<-data.frame(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
write.csv(rf3,"rf3.csv")





# SVM REGRESSION
library(caTools) 

set.seed(123) 




library(e1071) 



classifier = svm(formula = Item_Outlet_Sales ~  Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type + Item_MRP + Outlet_Identifier + Outlet_Establishment_Year + Outlet_Size +Outlet_Location_Type + Outlet_Type, 
                 
                 data = train, 
                 
                 type = 'eps-regression', 
                 
                 kernel = 'linear') 

# Predicting the Test set results 

svm_pred_1 = predict(classifier, newdata = test1) 



# csv 
Item_Outlet_Sales<-svm_pred_1
Item_Identifier<-test1$Item_Identifier
Outlet_Identifier<-test1$Outlet_Identifier

svm1<-data.frame(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
write.csv(svm1,"svm1.csv")










classifier = svm(formula = Item_Outlet_Sales ~  Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type + Item_MRP + Outlet_Identifier + Outlet_Establishment_Year + Outlet_Size +Outlet_Location_Type + Outlet_Type, 
                 
                 data = train, 
                 
                 type = 'eps-regression', 
                 
                 kernel = 'polynomial') 

# Predicting the Test set results 

svm_pred_2 = predict(classifier, newdata = test1) 



# csv 
Item_Outlet_Sales<-svm_pred_2
Item_Identifier<-test1$Item_Identifier
Outlet_Identifier<-test1$Outlet_Identifier

svm2<-data.frame(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
write.csv(svm2,"svm2.csv")











classifier = svm(formula = Item_Outlet_Sales ~  Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type + Item_MRP + Outlet_Identifier + Outlet_Establishment_Year + Outlet_Size +Outlet_Location_Type + Outlet_Type, 
                 
                 data = train, 
                 
                 type = 'nu-regression', 
                 
                 kernel = 'polynomial') 

# Predicting the Test set results 

svm_pred_3 = predict(classifier, newdata = test1) 



# csv 
Item_Outlet_Sales<-svm_pred_3
Item_Identifier<-test1$Item_Identifier
Outlet_Identifier<-test1$Outlet_Identifier

svm3<-data.frame(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
write.csv(svm3,"svm3.csv")
















classifier = svm(formula = Item_Outlet_Sales ~  Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type + Item_MRP + Outlet_Identifier + Outlet_Establishment_Year + Outlet_Size +Outlet_Location_Type + Outlet_Type, 
                 
                 data = train, 
                 
                 type = 'nu-regression', 
                 
                 kernel = 'linear') 

# Predicting the Test set results 

svm_pred_4 = predict(classifier, newdata = test1) 



# csv 
Item_Outlet_Sales<-svm_pred_4
Item_Identifier<-test1$Item_Identifier
Outlet_Identifier<-test1$Outlet_Identifier

svm4<-data.frame(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
write.csv(svm4,"svm4.csv")







