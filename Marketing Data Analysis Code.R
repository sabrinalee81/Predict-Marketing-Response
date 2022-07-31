library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(corrplot)
library(dummies)
library(VIM)
library(gridExtra)
library(DataExplorer)
library(GGally)

setwd("/Users/sabrina/Documents/2021/Data Science/Masters_APU/Sem 1/AML/AML Assignment/Dataset")

rawdf <- read.csv("marketing_campaign.csv")
head(rawdf)

#Obtain column names for dataframe
col.names <- str_split(colnames(rawdf), '\\.') %>% unlist() 
col.names <- col.names[1:length(col.names)]
#separate the data by delimiter ';' for data in each column
marketing <- rawdf %>% separate(colnames(rawdf), col.names, sep = ';', fill = 'right')
View(marketing)
head(marketing)
#Remove ZCost and ZContact variables - no information provided for these variables
marketing <- marketing [ ,-c(27,28)]
write.csv(marketing, 'marketing_formatraw.csv')



marketing <- read.csv('marketing_formatraw.csv')

glimpse(marketing) #no information about Z_CostContact and Z_Revenue column

#convert numeric columns to hold numeric data
#columns not to convert to numeric: Education, Marital_Status, Dt_Customer, Kidhome, TeenHome
numer_col <- col.names[c(c(2,5,6,7,9),seq(10,20))]
marketing <- mutate_at(marketing, numer_col, as.numeric)
#convert Dt_customer column to date type
marketing <- mutate_at(marketing, col.names[8], as.Date)
#convert columns to factor type: Education, Marital_Status, AcceptedCmpn, Responses
fact_col <- col.names[c(c(3,4),seq(21,26),29)]
marketing <- mutate_at(marketing, fact_col, as.factor)

str(marketing)
glimpse(marketing)


#Univariate Analysis: Category 1 - Marketing Response

#Acceptedcmp1 variable
marketing %>%
  group_by(AcceptedCmp1) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))

#Acceptedcmp2 variable
marketing %>%
  group_by(AcceptedCmp2) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))

#Acceptedcmp3 variable
marketing %>%
  group_by(AcceptedCmp3) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))

#Acceptedcmp4 variable
marketing %>%
  group_by(AcceptedCmp4) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))

#Acceptedcmp5 variable
marketing %>%
  group_by(AcceptedCmp5) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))

#Response variable
marketing %>%
  group_by(Response) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))

#plot marketing response
#Create a table for all campaign responses
t1 = as.data.frame(table(marketing$AcceptedCmp1)) ; t1
t2 = as.data.frame(table(marketing$AcceptedCmp2)) ; t2
t3 = as.data.frame(table(marketing$AcceptedCmp3)) ; t3
t4 = as.data.frame(table(marketing$AcceptedCmp4)) ; t4
t5 = as.data.frame(table(marketing$AcceptedCmp5)) ; t5
t6 = as.data.frame(table(marketing$Response)) ; t6

#Combine all responses into a table
responses.df <- c(t1[,2])
responses.df <- rbind(responses.df, c(t2[,2]))
responses.df <- rbind(responses.df, c(t3[,2]))
responses.df <- rbind(responses.df, c(t4[,2]))
responses.df <- rbind(responses.df, c(t5[,2]))
responses.df <- rbind(responses.df, c(t6[,2]))

colnames(responses.df) <- c("0","1")
rownames(responses.df) <- c("Campaign 1","Campaign 2",'Campaign 3',"Campaign 4","Campaign 5",'Campaign 6')

responses.df <- melt(responses.df)
responses.df$Var2 <- as.factor(responses.df$Var2)
responses.df

ggplot(responses.df, aes(fill=Var2, y=value, x=Var1)) + 
  geom_bar(position="dodge", stat="identity") + scale_fill_manual(values = c("#009E73","#0072B2")) +
  xlab("Marketing Campaigns") + ylab("Reponse Frequency") + ggtitle("Customers Responses to Marketing Campaigns",subtitle="0 = No Response, 1 = Responded") +
  guides(fill=guide_legend(title="Customer Response"))

#Univariate Analysis: Category 2 - Demographic Information

#plot age
#create age column
marketing$age = 2021 - marketing$Year_Birth
#plot
age_hist = hist(marketing$age, main = "Histogram of Customer Age",xlab='Customer Age') ;age_hist
age_box = boxplot(marketing$age, main = "Box Plot of Customer Age", ylab = "Customer Age")

#plot Dt_Customer
head(marketing$Dt_Customer)
boxplot(marketing$Dt_Customer)
ggplot(marketing, aes(x=Dt_Customer)) + geom_histogram(binwidth=30, colour='white') + ylab("Frequency") + xlab("Date of Customer Enrolment")+ ggtitle("Frequency of Customers Enrolled in each Date")

#plot income
income_box = boxplot(marketing$Income,main = 'Customer Income Range', ylab='Customer Annual Household Income')

#Education variable
marketing %>%
  group_by(Education) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))

#Marital status variable
marketing %>%
  group_by(Marital_Status) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))

#Kidhome variable
marketing %>%
  group_by(Kidhome) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))

#Teenhome variable
marketing %>%
  group_by(Teenhome) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))

#Univariate Analysis: Category 3 - Spending Amount on Retail Categories

marketing$TotalSpending <- marketing$MntWines + marketing$MntFruits + marketing$MntMeatProducts +
  marketing$MntFishProducts + marketing$MntSweetProducts + marketing$MntGoldProds

boxplot(marketing$TotalSpending, ylab = 'Total Spending', main = 'Total Spending of Customers')

#Univariate Analysis: Category 4 - Amount of purchases through multiple channels

deal = as.data.frame(table(marketing$NumDealsPurchases)) ; deal
colnames(deal)<-c("Number_of_Purchases","Frequency")
deal$Channel <- ("Deals")
deal

catalog = as.data.frame(table(marketing$NumCatalogPurchases)) ; catalog
colnames(catalog)<-c("Number_of_Purchases","Frequency")
catalog$Channel <- ("Catalog")
catalog

store = as.data.frame(table(marketing$NumStorePurchases)) ; store
colnames(store) <- c("Number_of_Purchases","Frequency")
store$Channel <- ("Store")
store

web = as.data.frame(table(marketing$NumStorePurchases)) ; web
colnames(web) <- c("Number_of_Purchases","Frequency")
web$Channel <- ("Web")
web

channelpurchase <- deal
channelpurchase <- rbind(channelpurchase, catalog)
channelpurchase <- rbind(channelpurchase, store)
channelpurchase <- rbind(channelpurchase, web)

ggplot(channelpurchase, aes(fill=Channel,y=Frequency,x=Number_of_Purchases)) + geom_bar(position="dodge", stat="identity") +
  xlab("Number of Purchases") + ylab("Frequency") + ggtitle("Purchases Made through Different Channels")

#Univariate Analysis: Category 5 - Additional Information

#Recency
View(marketing)
hist(marketing$Recency, xlab="Recency", ylab="Frequency", main="Recency of Customers")

#check for missing values
#replace all blanks with NA
marketing <- mutate_all(marketing,na_if,"")
sum(is.na(marketing))

plot_missing(marketing) #1.07% of missing values are from 24 missing data in Income column
aggr(marketing, numbers = TRUE , combine = TRUE, labels=names(marketing), cex.axis=0.6, oma=c(10,4,8,6)) #24 rows

#plot income vs total spending

#plot income vs total spending
plot(marketing$TotalSpending, marketing$Income , xlab = 'Total spendings (in 2 years)', ylab="Annual Household Income",main = 'Houshold Income vs Total Spendings', pch=19,cex=0.5)

str(marketing)
#plot complain vs total spending
complainplot = plot(marketing$Complain, marketing$TotalSpending, xlab="Customer Complained within 2 years\n (0 - No complains , 1 - Complained)",ylab='Total Spending',main='Total spending vs Complains Made') 
complainplot

#plot recency vs total spending
recencyplot = plot(marketing$Recency, marketing$total_spending, pch=19, cex=0.5, xlab="Recency",ylab="Total spending in 2 years", main='Total spending vs Recency')

#Correlation Analysis
#Find correlation between numeric columns
#correlation plot between numeric vectors
ggcorr(marketing,low = "steelblue", mid = "white", high = "darkred", label=TRUE, label_size=3,label_alpha=TRUE,hjust=0.85,size=3)

#Additional Feature Engineering

#Education into Below Degree, Degree, and Postgraduate
table(marketing$Education)
marketing$EducationLevel[marketing$Education %in% c('Basic')] <- 'Below Degree'
marketing$EducationLevel[marketing$Education %in% c('Graduation')] <- 'Degree'
marketing$EducationLevel[marketing$Education %in% c('PhD','Master','2n Cycle')] <- 'Postgraduate'
table(marketing$EducationLevel)

#Marital Status
#Create New Categories
marketing$Rel_Status[marketing$Marital_Status %in% c('Alone', 'Divorced', 'Widow', 'Single')] <- 'Single'
marketing$Rel_Status[marketing$Marital_Status %in% c('Married', 'Together')] <- 'Coupled'
marketing$Rel_Status[marketing$Marital_Status %in% c('Absurd', 'YOLO')] <- 'Others'
table(marketing$Rel_Status)


marketing <- subset(marketing, Rel_Status!= "Others")
View(marketing)

#Has Child
marketing$Num_Child = marketing$Kidhome + marketing$Teenhome

#PastCampaignResponse
marketing$CampaignResponse = as.numeric(marketing$AcceptedCmp1) + 
  as.numeric(marketing$AcceptedCmp2) + as.numeric(marketing$AcceptedCmp3) +
  as.numeric(marketing$AcceptedCmp4) + as.numeric(marketing$AcceptedCmp5)

#Target Variable is Response. Therefore, it is not included into the Campaign Response column
colnames(marketing)
head(marketing)
marketing <- marketing[-c(1)]


#Dt_Customer into Membership
today <- Sys.Date()
marketing$Membership <- as.numeric((today)-(marketing$Dt_Customer))


#Label Encoding for Marital Status
marketing$Rel_Single[marketing$Rel_Status %in% c('Single')] <- 1
marketing$Rel_Single[marketing$Rel_Status %in% c('Coupled')] <- 0
marketing$Rel_Coupled[marketing$Rel_Status %in% c('Single')] <- 0
marketing$Rel_Coupled[marketing$Rel_Status %in% c('Coupled')] <- 1

#Label Encoding for Education Level
marketing$Ed_Below_Degree[marketing$EducationLevel%in% c('Below Degree')] <- 1
marketing$Ed_Below_Degree[marketing$EducationLevel %in% c('Degree','Postgraduate')] <- 0
marketing$Ed_Degree[marketing$EducationLevel %in% c('Degree')] <- 1
marketing$Ed_Degree[marketing$EducationLevel%in% c('Below Degree','Postgraduate')] <- 0
marketing$Ed_Postgraduate[marketing$EducationLevel %in% c('Postgraduate')] <- 1
marketing$Ed_Postgraduate[marketing$EducationLevel%in% c('Degree', 'Below Degree')] <- 0

#Filter out unused columns
marketing <- marketing[-c(1,2,3,4,5,6,7,8,29,30)]
marketing <- marketing[-c(23,24)]
str(marketing)
head(marketing)
colnames(marketing)

#perform mean imputation for income
marketing_meanimpute <- marketing

marketing_meanimpute$Income[is.na(marketing$Income)] <- mean(marketing$Income, na.rm = TRUE)

marketing_meanimpute%>% summarise(
  is_NULL=sum(is.na(Income)==1),is_NOT_NULL=sum(!is.na(Income)==1),Max=max(Income), Min=min(Income), Mean=mean(Income), Median=median(Income), QUA1=quantile(Income,1/4), QUA3=quantile(Income,3/4), IQR=IQR(Income)
)


#perform median imputation for income
marketing_medianimpute <- marketing

marketing_medianimpute$Income[is.na(marketing$Income)] <- median(marketing$Income, na.rm = TRUE)

marketing_medianimpute%>% summarise(
  is_NULL=sum(is.na(Income)==1),is_NOT_NULL=sum(!is.na(Income)==1),Max=max(Income), Min=min(Income), Mean=mean(Income), Median=median(Income), QUA1=quantile(Income,1/4), QUA3=quantile(Income,3/4), IQR=IQR(Income)
)


#perform MICE imputation for income

library("mice")

marketing_miceimpute <- mice(marketing, m=3)
marketing_miceimpute <- complete (marketing_miceimpute)

marketing_miceimpute%>% summarise(
  is_NULL=sum(is.na(Income)==1),is_NOT_NULL=sum(!is.na(Income)==1),Max=max(Income), Min=min(Income), Mean=mean(Income), Median=median(Income), QUA1=quantile(Income,1/4), QUA3=quantile(Income,3/4), IQR=IQR(Income)
)

#Outlier Treatment

#age for mean imputed df
boxplot.stats(marketing_meanimpute$age)
stddev = sd(marketing_meanimpute$age)
mean = mean(marketing_meanimpute$age)
upper = 3.29*(stddev + mean)
upper

#income for median imputation
boxplot.stats(marketing_medianimpute$Income)
stddev = sd(marketing_medianimpute$Income, na.rm=TRUE) ; stddev
mean = mean(marketing_medianimpute$Income, na.rm=TRUE) ; mean
upper_median = 3.29*(stddev + mean)
upper_median

marketing_medianimpute <- subset(marketing_medianimpute, Income<upper_median)

#income for mean imputation
boxplot.stats(marketing_meanimpute$Income)
stddev = sd(marketing_meanimpute$Income, na.rm=TRUE) ; stddev
mean = mean(marketing_meanimpute$Income, na.rm=TRUE) ; mean
upper_mean = 3.29*(stddev + mean)
upper_mean

marketing_meanimpute <- subset(marketing_meanimpute, Income<upper_mean)


#income for mice imputation
boxplot.stats(marketing_miceimpute$Income)
stddev = sd(marketing_miceimpute$Income, na.rm=TRUE) ; stddev
mean = mean(marketing_miceimpute$Income, na.rm=TRUE) ; mean
upper_mice = 3.29*(stddev + mean)
upper_mice

marketing_miceimpute <- subset(marketing_miceimpute, Income<upper_mice)

#spending
boxplot.stats(marketing$TotalSpending)
stddev = sd(marketing$TotalSpending, na.rm=TRUE) ; stddev
mean = mean(marketing$TotalSpending, na.rm=TRUE) ; mean
upper = 3.29*(stddev + mean)
upper

sum(is.na(marketing_miceimpute))

#save preprocessed datatables
write.csv(marketing_meanimpute, 'marketing_meanimputed.csv')
write.csv(marketing_medianimpute, 'marketing_medianimputed.csv')
write.csv(marketing_miceimpute, 'marketing_miceimputed.csv')

