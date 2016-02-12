########################################################################################
#Ryan Rigato copyright 2015 all rights reserved
#
#
# This data parse turns the extra observations for the one to many relationship for the 
#key into features 
#
#########################################################################################
#manipulating strings
library(stringr)

library(vcd)
library(plyr)
library(stats)

#sql queries
library(sqldf)

library(MASS)

#decision trees
library(tree)

library(ISLR)
#randomforests
library(randomForest)

library(foreign)
library(nnet)

#naive bayes
library(e1071)

#general boosting models
library(gbm)

#importing the datasets that were provided by Telstra
train <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\train.csv")
test <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\test.csv")
severity_type <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\severity_type.csv")
resource_type <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\resource_type.csv")
log_feature <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\log_feature.csv")
event_type <- read.csv("C:\\Users\\Randy\\Downloads\\Telstra Kaggle Competion\\event_type.csv")






#merging the datasets
train = merge(train, severity_type, by='id')
test = merge(test, severity_type, by='id')








###############################################################################
#	This section of code takes the log_feature as the variable name and the
#	volume associated with that as the value of each of those 386 unique variables
#	
#	precondition:train has all 7381 observations with four variables:
#	id, log_feature, volume, and severity_type already in the data_frame
#
###############################################################################

	#initialize the matrix	
	 mtest2  = as.data.frame(matrix(nrow = nrow(train), ncol = 390))
	mtest2[,5:390] = 0
	mtest2[,1:4] = train[,1:4]
	mtest2 = rename(mtest2, c('V1' = 'id', 'V2' = 'location',
	 'V3' = 'fault_severity', 'V4' = 'severity_type'))


#gets the unique 386 log_feature names as strings
feature_name = as.character(log_feature[!duplicated(log_feature[,2]),2]) 
for(i in 1:386)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+4 cause the first four columns of train are id,location
	#fault_severity and severity_type  
	colnames(mtest2)[i + 4] = 
	as.character(feature_name[i])
}
ncol(mtest2)


train_row = 0
column_num = 0

#puts the volume into the observation corresponding to the log_feature 
#variable name
for(z in 1:nrow(log_feature))
{
	#gets the row in train where the id corresponds to the id in log_feature
	train_row  = which(train$id == log_feature$id[z])

	#getting the column which corresponds to 'feature x' 
	column_num = which(colnames(mtest2) == log_feature$log_feature[z])
	
	#if it is length 0 then the observation corresponds to the test set
	#otherwise place it where it belongs in mtestm
	if(length(train_row) != 0)
	{
		mtest2[train_row,column_num] = 
		mtest2[train_row,column_num] + log_feature$volume[z]
	}
}


#tests to make sure the sum of the volume is equal to the sum of the volume
#for train observations
sum(mtest2[,5:390]) ==sum(log_feature[log_feature$id %in% train$id,3])

#set train equal to mtest2
train = mtest2




	#initialize the matrix	
	 mtest3  = as.data.frame(matrix(nrow = nrow(test), ncol = 389))
	mtest3[,4:389] = 0
	mtest3[,1:3] = test[,1:3]
	mtest3 = rename(mtest3, c('V1' = 'id', 'V2' = 'location',
	 'V3' = 'severity_type'))


#gets the unique 386 log_feature names as strings
feature_name = as.character(log_feature[!duplicated(log_feature[,2]),2]) 
for(i in 1:386)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+3 cause the first three columns of test are id,location
	#and severity_type  
	colnames(mtest3)[i + 3] = 
	as.character(feature_name[i])
}
ncol(mtest3)


test_row = 0
column_num = 0

#puts the volume into the observation corresponding to the log_feature 
#variable name
for(z in 1:nrow(log_feature))
{
	#gets the row in test where the id corresponds to the id in log_feature
	test_row  = which(test$id == log_feature$id[z])

	#getting the column which corresponds to 'feature x' 
	column_num = which(colnames(mtest3) == log_feature$log_feature[z])
	
	#if it is length 0 then the observation corresponds to the test set
	#otherwise place it where it belongs in mtestm
	if(length(test_row) != 0)
	{
		mtest3[test_row,column_num] = 
		mtest3[test_row,column_num] + log_feature$volume[z]
	}
}


#tests to make sure the volume for test is the same
sum(mtest3[,4:389]) ==sum(log_feature[log_feature$id %in% test$id,3])



#test to make sure total volume is the same
sum(mtest3[,4:389]) + sum(mtest2[,5:390]) ==sum(log_feature[,3])


#set test equal to mtest3
test = mtest3


#################################################################################
#	adds resource_type
#
#
#
#################################################################################
	



	#initialize the matrix	
	 mtest2  = train
	mtest2[,391:400] = 0




#gets the unique 10 resource_type names as strings
feature_name = as.character(resource_type[!duplicated(resource_type[,2]),2]) 
for(i in 1:10)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+390 cause the first four columns of train are id,location
	#fault_severity and severity_type  
	colnames(mtest2)[i + 390] = 
	as.character(feature_name[i])
}
ncol(mtest2)


train_row = 0
column_num = 0

#puts the volume into the observation corresponding to the resource_type
#variable name
for(z in 1:nrow(resource_type))
{
	#gets the row in train where the id corresponds to the id in event_type
	train_row  = which(train$id == resource_type$id[z])

	#getting the column which corresponds to 'feature x' 
	column_num = which(colnames(mtest2) == resource_type$resource_type[z])
	
	#if it is length 0 then the observation corresponds to the test set
	#otherwise place it where it belongs in mtest2
	if(length(train_row) != 0)
	{
		mtest2[train_row,column_num] = 
		mtest2[train_row,column_num] + 1
	}
}


#tests to make sure the sum of the volume is equal to the sum of the resource_type
#for train observations
sum(mtest2[,391:400]) ==length(resource_type[resource_type$id %in% train$id,2]) 

#set train equal to mtest2
train = mtest2






	#initialize the matrix	
	 mtest3  = test
	mtest3[,390:399] = 0




#gets the unique 10 resource_type names as strings
feature_name = as.character(resource_type[!duplicated(resource_type[,2]),2]) 
for(i in 1:10)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+389 cause the first four columns of test are id,location
	# severity_type  
	colnames(mtest3)[i + 389] = 
	as.character(feature_name[i])
}
ncol(mtest3)


test_row = 0
column_num = 0

#puts the volume into the observation corresponding to the resource_type
#variable name
for(z in 1:nrow(resource_type))
{
	#gets the row in test where the id corresponds to the id in event_type
	test_row  = which(test$id == resource_type$id[z])

	#getting the column which corresponds to 'feature x' 
	column_num = which(colnames(mtest3) == resource_type$resource_type[z])
	
	#if it is length 0 then the observation corresponds to the test set
	#otherwise place it where it belongs in mtest3
	if(length(test_row) != 0)
	{
		mtest3[test_row,column_num] = 
		mtest3[test_row,column_num] + 1
	}
}


#tests to make sure the sum of the volume is equal to the sum of the resource_type
#for test observations
sum(mtest3[,390:399]) ==length(resource_type[resource_type$id %in% test$id,2]) 


#checks total resource_type
sum(mtest3[,390:399]) + sum(mtest2[,391:400]) == nrow(resource_type)

#set test equal to mtest3
test = mtest3





#################################################################################
#	adds event_type to the model
#
#
#
#
##################################################################################
	#initialize the matrix	
	 mtest2  = train
	mtest2[,401:453] = 0




#gets the unique 53 event_type names as strings
feature_name = as.character(event_type[!duplicated(event_type[,2]),2]) 
for(i in 1:53)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+400 cause the first four columns of train are id,location
	#fault_severity and severity_type  
	colnames(mtest2)[i + 400] = 
	as.character(feature_name[i])
}
ncol(mtest2)


train_row = 0
column_num = 0

#puts the volume into the observation corresponding to the event_type
#variable name
for(z in 1:nrow(event_type))
{
	#gets the row in train where the id corresponds to the id in event_type
	train_row  = which(train$id == event_type$id[z])

	#getting the column which corresponds to 'feature x' 
	column_num = which(colnames(mtest2) == event_type$event_type[z])
	
	#if it is length 0 then the observation corresponds to the test set
	#otherwise place it where it belongs in mtest2
	if(length(train_row) != 0)
	{
		mtest2[train_row,column_num] = 
		mtest2[train_row,column_num] + 1
	}
}


#tests to make sure the sum of the volume is equal to the sum of the event_type
#for train observations
sum(mtest2[,401:453]) ==length(event_type[event_type$id %in% train$id,2]) 

#set train equal to mtest2
train = mtest2









	#initialize the matrix	
	 mtest3  = test
	mtest3[,400:452] = 0




#gets the unique 53 event_type names as strings
feature_name = as.character(event_type[!duplicated(event_type[,2]),2]) 
for(i in 1:53)
{

	#gets the value of each unique log_features
	#then uses those as a column name
	#starts at i+399 cause the first three columns of test are id,location
	# severity_type  
	colnames(mtest3)[i + 399] = 
	as.character(feature_name[i])
}
ncol(mtest3)


test_row = 0
column_num = 0

#puts the volume into the observation corresponding to the event_type
#variable name
for(z in 1:nrow(event_type))
{
	#gets the row in test where the id corresponds to the id in event_type
	test_row  = which(test$id == event_type$id[z])

	#getting the column which corresponds to 'feature x' 
	column_num = which(colnames(mtest3) == event_type$event_type[z])
	
	#if it is length 0 then the observation corresponds to the test set
	#otherwise place it where it belongs in mtest3
	if(length(test_row) != 0)
	{
		mtest3[test_row,column_num] = 
		mtest3[test_row,column_num] + 1
	}
}


#tests to make sure the sum of the volume is equal to the sum of the event_type
#for test observations
sum(mtest3[,400:452]) ==length(event_type[event_type$id %in% test$id,2]) 

#tests total
sum(mtest3[,400:452]) + sum(mtest2[,401:453]) ==nrow(event_type)
#set test equal to mtest3
test = mtest3






####################################################################################
#	Writes the final test and train to a csv
#
######################################################################################

write.csv(train, 'C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\trainParse.csv',
		row.names = FALSE)

write.csv(test, 'C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\testParse.csv',
		row.names = FALSE)




