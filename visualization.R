rm=list(ls)
setwd("C:/Users/jayanth/Desktop/Data analytics lab")
data1 <- read.csv("sales_cleaned.csv")
library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(xgboost)    # used for building XGBoost model
library(cowplot)    # used for combini
p9 = ggplot(data) + 
  geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

ggplot(data1%>%group_by(Item_Fat_Content)%>%summarise(Count=n()))+geom_bar(aes(Item_Fat_Content,Count),stat = "identity",fill="darkblue")
ggplot(data1)+geom_point(aes(Item_Weight, Item_Outlet_Sales),col="blue")

qplot(data1$Item_Outlet_Sales, geom="histogram",col="darkgreen",binwidth=1000, xlab = "Item Outlet Sales", ylab = "Count")
qplot(data1$Item_Weight, geom="histogram", col="darkblue", binwidth=0.5, xlab = "Item Weight", ylab = "Count")
qplot(data1$Item_Visibility, geom= "histogram",binwidth=0.005, col="blue", xlab = "Item visibility", ylab = "Count")
qplot(data1$Item_MRP, geom = "histogram", col="darkred", binwidth=1, xlab = "Item MRP", ylab = "Count")
ggplot(data1 %>% group_by(Item_Fat_Content) %>% summarise(Count = n()))+geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

ggplot(data1%>%group_by(Item_Fat_Content)%>%summarise(Count=n()))+geom_bar(aes(Item_Fat_Content,Count),stat = "identity",fill="darkblue")
ggplot(data1%>%group_by(Item_Type)%>%summarise(Count=n()))+geom_bar(aes(Item_Type,Count),stat = "identity",fill="lightblue")
ggplot(data1%>%group_by(Outlet_Identifier)%>%summarise(Count=n()))+geom_bar(aes(Outlet_Identifier,Count),stat = "identity",fill="darkgreen")
ggplot(data1 %>% group_by(Outlet_Size) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "lightgreen")
ggplot(data1 %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "darkred")
ggplot(data1 %>% group_by(Outlet_Type) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "royalblue")


ggplot(data1)+geom_point(aes(Item_Weight, Item_Outlet_Sales),col="blue")
ggplot(data1)+geom_point(aes(Item_Visibility, Item_Outlet_Sales),col="blue")
ggplot(data1)+geom_point(aes(Item_MRP, Item_Outlet_Sales),col="blue")
ggplot(data1)+geom_point(aes(Item_Type, Item_Outlet_Sales),fill="blue")
ggplot(data1)+geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales),fill="blue")
ggplot(data1)+geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales),fill="blue")
ggplot(data1)+geom_violin(aes(Outlet_Size, Item_Outlet_Sales),fill="blue")
ggplot(data1)+geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales),fill="blue")
ggplot(data1)+geom_violin(aes(Outlet_Type, Item_Outlet_Sales),fill="blue")



#item type vs outlet sales
qplot(x = sqrt(Item_Outlet_Sales), data = data1, binwidth = 1,
      ylab = "Sales Distribution",
      xlab = "SQRT of Outlet Sales",
      fill=I("tomato")) +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(angle = 30)) +
  theme_minimal() +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,120,15)) +
  scale_y_continuous(limits = c(0,40), breaks = seq(0,40,10)) +
  facet_wrap(~Item_Type)

qplot(x = Item_Type, y = sqrt(Item_Outlet_Sales),
      ylab = "SQRT of Outlet Sales",
      data = data1,
      geom = "boxplot",
      fill=I("tomato")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(angle = 30))


#outlet identifier vs outlet sales
 
qplot(x = sqrt(Item_Outlet_Sales), data = data1, binwidth = 1,
      ylab = "Sales Distribution",
      xlab = "SQRT of Outlet Sales",
      fill=I("tomato")) +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(angle = 30)) +
  theme_minimal() +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,120,15)) +
  scale_y_continuous(limits = c(0,40), breaks = seq(0,40,10)) +
  facet_wrap(~Outlet_Identifier,2)


qplot(x = Outlet_Identifier, y = sqrt(Item_Outlet_Sales), 
            color = Outlet_Type,
            ylab = "SQRT of Outlet Sales",
            data = data1,
            geom = "boxplot",
            fill=Outlet_Type) +
  geom_vline(aes(xintercept=quantile(data1$Item_Outlet_Sales, c(.01))),  color="red", linetype="dashed", size=1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(angle = 30))




 
 ggplot(data1, aes(x = Item_Weight, y = Item_Outlet_Sales)) +
   geom_point() +
   geom_smooth(method = 'lm', se = FALSE, color='tomato') 
 
 
 
 
 #outlet sales vs fat content
 qplot(x = sqrt(Item_Outlet_Sales), data = data1, binwidth = 1,
       ylab = "",
       xlab = "SQRT of Outlet Sales",
       fill=I("tomato")) +
   theme(axis.text.x = element_text(angle = 90),
         axis.text.y = element_text(angle = 30)) +
   scale_x_continuous(limits = c(0,100), breaks = seq(0,120,15)) +
   scale_y_continuous(limits = c(0,200), breaks = seq(0,200,100)) +
   theme_minimal() +
   facet_wrap(~Item_Fat_Content)

 
 
 
  qplot(x = Item_Fat_Content, y = sqrt(Item_Outlet_Sales),
       ylab = "SQRT of Outlet Sales",
       data = data1,
       geom = "boxplot",
       fill=I("tomato")) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 0),
         axis.text.y = element_text(angle = 30))

 
 #outlet size vs outlet sales
  qplot(x = sqrt(Item_Outlet_Sales), data = new_train, binwidth = 0.5,
        ylab = "Sales Distribution",
        xlab = "SQRT of Outlet Sales",
        fill=I("tomato")) +
    theme(axis.text.x = element_text(angle = 90),
          axis.text.y = element_text(angle = 30)) +
    theme_minimal() +
    scale_x_continuous(limits = c(0,100), breaks = seq(0,120,15)) +
    scale_y_continuous(limits = c(0,40), breaks = seq(0,40,10)) +
    facet_wrap(~Outlet_Size)
  
  qplot(x = Outlet_Size, y = sqrt(Item_Outlet_Sales),
        ylab = "SQRT of Outlet Sales",
        data = data1,
        geom = "boxplot",
        fill=I("tomato")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90),
          axis.text.y = element_text(angle = 30))

  #outlet location type vs outlet sales
  qplot(x = sqrt(Item_Outlet_Sales), data = data1, binwidth = 0.5,
        ylab = "Sales Distribution",
        xlab = "SQRT of Outlet Sales",
        fill=I("tomato")) +
    theme(axis.text.x = element_text(angle = 90),
          axis.text.y = element_text(angle = 30)) +
    theme_minimal() +
    scale_x_continuous(limits = c(0,100), breaks = seq(0,120,15)) +
    scale_y_continuous(limits = c(0,40), breaks = seq(0,40,10)) +
    facet_wrap(~Outlet_Location_Type)
 
   qplot(x = Outlet_Location_Type, y = sqrt(Item_Outlet_Sales),
        ylab = "SQRT of Outlet Sales",
        data = data1,
        geom = "boxplot",
        fill=I("tomato")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0),
          axis.text.y = element_text(angle = 30))
  
    #outlet type vs outlet sales
   
   qplot(x = sqrt(Item_Outlet_Sales), data = data1, binwidth = 0.5,
        ylab = "Sales Distribution",
        xlab = "SQRT of Outlet Sales",
        fill=I("tomato")) +
     theme(axis.text.x = element_text(angle = 90),
           axis.text.y = element_text(angle = 30)) +
     theme_minimal() +
     scale_x_continuous(limits = c(0,100), breaks = seq(0,120,15)) +
     scale_y_continuous(limits = c(0,40), breaks = seq(0,40,10)) +
     facet_wrap(~Outlet_Type)
   
   qplot(x = Outlet_Type, y = sqrt(Item_Outlet_Sales),
         ylab = "SQRT of Outlet Sales",
         data = data1,
         geom = "boxplot",
         fill=I("tomato")) +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 0),
           axis.text.y = element_text(angle = 30))
   
   #item identifier vs outlet sales
   qplot(x = sqrt(Item_Outlet_Sales), data = data1, binwidth = 0.1,
         ylab = "Sales Distribution",
         xlab = "SQRT of Outlet Sales",
         fill=I("tomato")) +
     theme(axis.text.x = element_text(angle = 90),
           axis.text.y = element_text(angle = 30)) +
     theme_minimal() +
     scale_x_continuous(limits = c(0,100), breaks = seq(0,120,15)) +
     scale_y_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
     facet_wrap(~Item_Identifier)
   