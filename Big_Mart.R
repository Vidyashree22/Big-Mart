#Reading data
train=(read.csv(file.choose()))
head(train)
test=(read.csv(file.choose()))
head(test)

dim(train)
dim(test)

#Seperating train,test
train$source='train'
test$source='test'

#merging of test and train
library(gtools)
data=merge(train,test,all = TRUE)

#Getting null values
sapply(data, function(x) sum(is.na(x)))
data$Outlet_Size[data$Outlet_Size== " "]<-NA
summary(data)

#Unique value of each colum
sapply(data, function(x) length(unique(x)))
sapply(data, class)

table(data$Item_Fat_Content)
table(data$Item_Type)
table(data$Outlet_Identifier)
table(data$Outlet_Size)
table(data$Outlet_Location_Type)
table(data$Outlet_Type)

#Inserting mean value for missing values
#Item Weight
data$Item_Weight[is.na(data$Item_Weight)] <- mean(data$Item_Weight, na.rm = TRUE)

#Item Visibility
data$Item_Visibility[data$Item_Visibility==0.00]<- mean(data$Item_Visibility, na.rm = TRUE)

dim(data)

#Mode Function
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1)
    xmode <- ">1 mode"
  return(xmode)
}

#Outlet Size
Mode(data$Outlet_Size)
data$Outlet_Size[is.na(data$Outlet_Size)] <- Mode(data$Outlet_Size)

#Item Fat Content
data$Item_Fat_Content[data$Item_Fat_Content== "LF"]<-"Low Fat"
data$Item_Fat_Content[data$Item_Fat_Content== "low fat"]<-"Low Fat"
data$Item_Fat_Content[data$Item_Fat_Content== "reg"]<-"Regular"
table(data$Item_Fat_Content)
sapply(data, function(x) sum(is.na(x)))

#Feature Engineering
#Trying to combine the Outlet Type, but all the categories are different and hence cannot be combined

aggregate(data$Item_Outlet_Sales, by=list(Category=data$Outlet_Type), FUN=sum)
s1=data$Item_Outlet_Sales[data$Outlet_Type=="Grocery Store"]
mean(s1,na.rm=TRUE)
s2=data$Item_Outlet_Sales[data$Outlet_Type=="Supermarket Type1"]
mean(s2,na.rm=TRUE)
s3=data$Item_Outlet_Sales[data$Outlet_Type=="Supermarket Type2"]
mean(s3,na.rm=TRUE)
s4=data$Item_Outlet_Sales[data$Outlet_Type=="Supermarket Type3"]
mean(s4,na.rm=TRUE)

attach(data)

table(data$Item_Type)
table(data$Item_Identifier)

#Recoding the Item Identifier
data$Combined[grepl("^FD",data$Item_Identifier)]<-"Food"
data$Combined[grepl("^NC",data$Item_Identifier)]<-"Non-Consumable"
data$Combined[grepl("^DR",data$Item_Identifier)]<-"Drinks"
table(data$Combined)

#The data was collected in 2013
data$Year<-2013-data$Outlet_Establishment_Year
summary(data$Year)

#Non Edible Fat
levels(data$Item_Fat_Content)
data$Item_Fat_Content <- factor(data$Item_Fat_Content)

levels(data$Combined)
data$Combined <- factor(data$Combined)

levels <- levels(data$Item_Fat_Content)
levels[length(levels) + 1] <- 'Non-Edible'
data$Item_Fat_Content <- factor(data$Item_Fat_Content,levels = levels)
data$Item_Fat_Content[data$Combined=="Non-Consumable"]<-"Non-Edible"

table(data$Item_Fat_Content)
rm(train_1)
#Seperating Train, Test
train_1<-subset(data,data$source=='train')
test_1<-subset(data,data$source=='test')

#One-hot coding of Categorical Variables
new_train<-subset(train_1,select = c("Item_Weight","Item_Fat_Content","Item_Visibility","Item_Type",
                                     "Item_MRP","Outlet_Establishment_Year","Outlet_Size",
                                     "Outlet_Type","Outlet_Location_Type","Item_Outlet_Sales",
                                     "Combined","Year" ))
library(dummies)
data1<-dummy.data.frame(new_train, sep="_")

test_new<-subset(test_1,select=c("Item_Weight","Item_Fat_Content","Item_Visibility",
                                 "Item_Type","Item_MRP","Outlet_Establishment_Year",
                                 "Outlet_Size","Outlet_Location_Type","Outlet_Type",
                                 "Combined","Year"))
test_new<-dummy.data.frame(test_new, sep="_")

#Baseline Model
mean_output=mean(train_1$Item_Outlet_Sales,na.rm=TRUE)
submission=subset(test_1,select=c("Item_Identifier","Outlet_Identifier"))
submission$Item_Outlet_Sales=mean_output
head(submission)
#write.csv(submission,"submission_1.csv")

#Predicting Function

which(colnames(train_1)=="Outlet_Location_Type")
predictor<-subset(train_1,select = c())
train_1<-subset(train_1,select = c(2,3,4,5,6,8,9,11,10,13,14,15))

#model1
model<-lm(formula = Item_Outlet_Sales~ .,data=train_1)
summary(model)

new_data<-subset(train_1,select = c(6,7,8,10))

#Model 2

model_lm<-lm(formula = Item_Outlet_Sales~ .,data=data1)
summary(model_lm)
str(data1)

#Model 3
data2<-subset(data1,select=c("Item_MRP","Outlet_Establishment_Year","Outlet_Size_",
                             "Outlet_Size_High","Outlet_Type_Grocery Store",
                             "Outlet_Type_Supermarket Type1","Outlet_Type_Supermarket Type2",
                             "Outlet_Location_Type_Tier 1","Outlet_Location_Type_Tier 2",
                             "Item_Outlet_Sales"))
model_lm1<-lm(formula = Item_Outlet_Sales~ .,data=data2)
summary(model_lm1)

#Submission
submission_lm=subset(test_1,select=c("Item_Identifier","Outlet_Identifier"))
head(test_new)
result<-predict(model_lm1,newdata = test_new)
length(result)

submission_lm$Item_Outlet_Sales=result
#write.csv(submission_lm,"submission_2.csv")

install.packages("DAAG")
library(lattice)
library(DAAG)
CVlm(data=data2,form.lm = model_lm1,m=5)

