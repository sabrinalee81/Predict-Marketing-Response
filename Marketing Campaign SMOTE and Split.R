#Split data by 80%
setwd("/Users/sabrina/Documents/2021/Data Science/Masters_APU/Sem 1/AML/AML Assignment/Dataset")
marketing_meanclustered <- read.csv("marketing_meanclustered.csv")
marketing_medianclustered <- read.csv("marketing_medianclustered.csv")
marketing_miceclustered <- read.csv('marketing_miceclustered.csv')


library(caTools)
set.seed(123)

install.packages( "/Users/sabrina/Downloads/DMwR_0.4.1.tar.gz", repos=NULL, type="source" )
library(DMwR)

#change this to apply smote to all training sets
marketing = marketing_meanclustered
marketing = marketing_medianclustered
marketing = marketing_miceclustered

colnames(marketing)

marketing <- marketing[-c(1,15,16,17,18,19)]
head(marketing)



marketing = marketing_miceclustered

#Perform 80:20 split
split = sample.split(marketing$Response, SplitRatio = 0.8)

train_set = subset(marketing, split == TRUE)
test_set = subset(marketing, split == FALSE)

dim(train_set)
dim(test_set)

#Before implementing SMOTE
prop.table(table(train_set$Response)) #Class imbalance: 85% Class 0 and 14% of Class 1

#Convert to factor
train_set$Response = as.factor(train_set$Response) #Class imbalance: 85% Class 0 and 14% of Class 1

#~~~~~~~~~~~SMOTE Oversampling~~~~~~~~~~~~~~~~~~~~~~~~

library(DMwR)

#perc.over = oversampling of minority class 
#perc.under = undersampling of majority class
balanced_set = SMOTE(Response ~., train_set, perc.over = 100, perc.under=200)

#After implementing SMOTE
prop.table(table(balanced_set$Response)) #50% of class 0 and 50% of class 1

write.csv(balanced_set, 'marketing_smean_train.csv')
write.csv(test_set, 'marketing_smean_test.csv')

write.csv(balanced_set, 'marketing_smedian_train.csv')
write.csv(test_set, 'marketing_smedian_test.csv')

write.csv(balanced_set, 'marketing_smice_train.csv')
write.csv(test_set, 'marketing_smice_test.csv')
