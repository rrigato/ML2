
########################################################################################
#Ryan Rigato copyright 2016 all rights reserved
#Telstra Kaggle Competition
#The goal of this script is to predict Telstra fault severity at any location.
#The response variable is 0(no fault) 1 (a few faults)  or 2 (many faults)
#
#Part one
#
#
#
#
#
#########################################################################################

#for the extraTrees model
library(extraTrees)


#for knn model
library(class)

#used for glmnet models
library("glmnet")

#for examining classification and regression trees
library(caret)

#for neuralnetwork analysis
library(neuralnet)

#write to an xlsx file
library(xlsx)
#xgboost
library(DiagrammeR)
library(Ckmeans.1d.dp)
library(xgboost)
library(methods)
library(data.table)
library(magrittr)


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







train <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\trainParse.csv")
test <- read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\testParse.csv")













################################################################
#	Splitting the train dataset into train2 and test2
#
#
#
#################################################################




#edit The percentage of the dataset in the train2 and test2, used to build a model 
size_of_train = floor(1*nrow(train))
ran_num_test = 1:nrow(train)

#gets random numbers for train2 using a sample
ran_num_train = sample(1:nrow(train), size_of_train)

#numbers not randomly selected for train2 are included in test2
#this command gets the numbers not in ran_num_train
ran_num_test = ran_num_test[(!(ran_num_test %in% ran_num_train)) == TRUE]
train2 = train[ran_num_train,]
test2 = train[ran_num_test,]









################################################################
#	implementation of extreme gradient boosting algorithms(xgboost)
#
#location, severity_type, log_feature(368 variables), volume
#eta = .05, gamma = .05 subsample = .75  log_loss = .5899256
#
#location, severity_type, log_feature(368 variables), volume
#eta = .1, gamma = .1 subsample = .75  log_loss = .5368068
#
#
##location, severity_type, log_feature(368 variables), volume, event_type(53 variables)
#eta = .1, gamma = .1 subsample = .75  log_loss = .5791565
#
###location, severity_type, log_feature(368 variables), 
#volume, resource_type(10 variables)
#eta = .1, gamma = .1 subsample = .75  log_loss = .5530854
#
location, severity_type, log_feature(368 variables), volume
#eta = .1, gamma = .1 subsample = .75  log_loss = .5415364
#test.mlogloss.mean = .546575  train = .9
#
#location, severity_type, log_feature(368 variables), volume
#eta = .1, gamma = .1 subsample = .75  log_loss = .546621
#test.mlogloss.mean = NA  train = full train
#
#
#
########################################################################



#extracts the location variable as a string
train2[,2] = as.numeric(str_sub(train2$location, start= 10))
test2[,2] = as.numeric(str_sub(test2$location, start= 10))

#extracts severity_type
train2[,4] = as.numeric(str_sub(train2$severity_type, start= 15))
test2[,4] = as.numeric(str_sub(test2$severity_type, start= 15))

#stores the ids in a vector and removes id from data frames
train2id = train2[,1]
train2 = train2[,-c(1)]

test3id = test2[,1]
test3 = test2[,-c(1)]

#checks that the number of ids in the vector is equal to the number of rows in 
#the data frames
length(train2id) == nrow(train2)
length(test3id) == nrow(test3)






#saves the outcome variable into a seperate vector
train2_response = train2[,2]
test3_response = test3[,2]

#removes outcome vector from the data_frame
test3 = test3[,-c(2)]
train2 = train2[,-c(2)]



length(train2_response) == nrow(train2)
length(test3_response) == nrow(test3)

train2[,2] = as.numeric(train2[,2])
train2Matrix = as.matrix(train2)

test3[,2] = as.numeric(test3[,2])
test3Matrix = as.matrix(test3)





#used to keep only those variables in the importance matrix
#train2Matrix = train2Matrix[,keep]
#test3Matrix = test3Matrix[,keep]


#train2Matrix = train2Matrix[,452:493]
#test3Matrix = test3Matrix[,452:493]



#create interaction for feature.203 and location after two keeps
#train2Matrix = cbind(train2Matrix, train2Matrix[,10]*train2Matrix[,1])

#test3Matrix = cbind(test3Matrix, test3Matrix[,10]*test3Matrix[,1])




#cross_validation parameters
numberOfClasses = 3
param = list( "objective" = "multi:softprob",
		"eval_metric" = "mlogloss",
		"num_class" = numberOfClasses
		)
cv.nround <- 250
cv.nfold <- 3

#setting up cross_validation
bst.cv = xgb.cv(param=param, data = train2Matrix, label = train2_response, 
                nfold = cv.nfold, nrounds = cv.nround)

#test for optimal nround
bst.cv[which(min(bst.cv$test.mlogloss.mean) == bst.cv$test.mlogloss.mean),]

#sets the number of rounds based on the number of rounds determined by cross_validation
nround = which(min(bst.cv$test.mlogloss.mean) == bst.cv$test.mlogloss.mean)
#actual xgboost
bst = xgboost(param=param, data = train2Matrix, label = train2_response,
		gamma = .1, eta = .1, nrounds=nround,
		subsample = .75, max_delta_step = 15)







# Get the feature real names
names <- dimnames(train2Matrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst); importance_matrix

# Nice graph for importance
xgb.plot.importance(importance_matrix[1:100,])



#the predictions are in a nrow(test3)*3 long vector
#bstPred[1:3] is the probability of 0,1,2 for fault_severity
#for the first observation of test2
#has to be a numeric matrix just like the training set
bstPred = predict(bst, test3Matrix)
is.vector(bstPred)
str(bstPred)


#initialize output frame
outputFrame = data.frame(matrix(nrow= nrow(test2), ncol=4))
outputFrame = rename(outputFrame, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1","X4" = "predict_2")) 

#Puts the ids for the observations into the first column of outputFrame[,1]
outputFrame[,1] = test3id
#test to make sure ids are the same
sum(outputFrame[,1] != test3id)
z_element = 1
for (i in 1:nrow(test2))
{
	for (z in 1:3)
	{
		#the ith row of outputFrame is given observation z_element
		#probability of occuring from bstPred
		#column z+1 since id is in column 1
		outputFrame[i,z+1] = bstPred[z_element]
		z_element = z_element + 1
	}
}


xgFrame2 = outputFrame


num_predict = 3
log_loss(outputFrame,num_predict)





################################################################################
#xgboost + gbm = .8643995
#
#
#
################################################################################
bTree = gbm(train2_response ~. -id, distribution = "multinomial", n.trees = 200,
		data = train5)


bTreeP = predict(bTree, newdata = test5, n.trees = 500, type = "response")

bTreeP = as.data.frame(bTreeP)
head(bTreeP)


	#initializes and fills the outputFrame that will be tested in the log_loss function 
	#and then returned
	doubleFrame = data.frame(matrix(nrow= nrow(test2), ncol=4))
	doubleFrame = rename(doubleFrame, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1", "X4" = "predict_2"))

doubleFrame[,1] = test3id
doubleFrame[,2:4] = bTreeP[,1:3]
head(doubleFrame)

num_predict = 3
log_loss(doubleFrame,num_predict)
######################################################################################
#
#
#feature_selection
####################################################################################


importance_matrix = as.data.frame(importance_matrix)


#gets the names of the variables that matter
top124 = importance_matrix[,1]


#gets all the names in train2Matrix
train2Col = colnames(train2Matrix)

#gets the column numbers of the variables you should keep
keep = which(train2Col %in%  top124)

keep2 = which(train2Col %in%  top124)

keep3 = which(train2Col %in% top124[1:125])




######################################################################################
#
#Plotting the matrix
#
#
######################################################################################
#backups while I am doing feature selection
train5Matrix = train2Matrix
test5Matrix = test3Matrix
x11()

top20 = importance_matrix[1:20,1]
train_num = which(colnames(train) %in% top20)
par(mfrow = c(3,3))
 for (i in 1:9){  plot(train[,3], train[,train_num[i]], main = train_num[i])}

for (i in 1:9){  plot(train[,train_num[i + 1]], train[,train_num[i]],main =  train_num[i])}



train2Matrix = train5Matrix
test3Matrix = test5Matrix


train2Matrix = as.data.frame(train2Matrix)
test3Matrix = as.data.frame(test3Matrix)






##adding new variables
train2Matrix[,452:456] = 0
test3Matrix[,452:456] = 0

#only severity_type 1
train2Matrix[which(train2Matrix[,2] ==1),452] = 1
test3Matrix[which(test3Matrix[,2] ==1),452] = 1


#severtity_type 2 3 4 5
train2Matrix[which(train2Matrix[,2] !=1),453] = 1
test3Matrix[which(test3Matrix[,2] !=1),453] = 1


#only resource_type.8
train2Matrix[which(train2Matrix[,389] !=0),454] = 1
test3Matrix[which(test3Matrix[,389] !=0),454] = 1

#all other resource_types
train2Matrix[which(train2Matrix[,389] ==0),455] = 1
test3Matrix[which(test3Matrix[,389] ==0),455] = 1


#severity_type 1 or 2

train2Matrix[which(train2Matrix[,2] ==1  | train2Matrix[,2] == 2),456] = 1
test3Matrix[which(test3Matrix[,2] ==1 | test3Matrix[,2] == 2),456] = 1




train2Matrix[,457:465] = 0
test3Matrix[,457:465] = 0

#severity_type*location
train2Matrix[,457] = train2Matrix[,1]* train2Matrix[,2]
test3Matrix[,457] = test3Matrix[,1] * test3Matrix[,2]


#severity_type * id
train2Matrix[,458] = train2id* train2Matrix[,2]
test3Matrix[,458] = test3id * test3Matrix[,2]


#location * id
train2Matrix[,459] = train2id* train2Matrix[,1]
test3Matrix[,459] = test3id * test3Matrix[,1]



#location > 750
train2Matrix[which(train2Matrix[,1] >750),460] = 1
test3Matrix[which(test3Matrix[,1] >750),460] = 1

#location >500 <= 750
train2Matrix[which(train2Matrix[,1] > 500  & train2Matrix[,1] <= 750),461] = 1
test3Matrix[which(test3Matrix[,1]   > 500  & test3Matrix[,1]  <= 750),461] = 1




#location >250 <= 500
train2Matrix[which(train2Matrix[,1] > 250  & train2Matrix[,1] <= 500),462] = 1
test3Matrix[which(test3Matrix[,1]   > 250  & test3Matrix[,1]  <= 500),462] = 1


#location <=250
train2Matrix[which( train2Matrix[,1] <= 250),463] = 1
test3Matrix[which( test3Matrix[,1]  <= 250),463] = 1



#location*severity_type <1850
train2Matrix[which( train2Matrix[,457] <= 1850),464] = 1
test3Matrix[which( test3Matrix[,457]  <= 1850),464] = 1



#location*severity_type >1850
train2Matrix[which( train2Matrix[,457] > 1850),465] = 1
test3Matrix[which( test3Matrix[,457]  > 1850),465] = 1





train2Matrix[,466:851] = train2Matrix[,3:388] * train2id
test3Matrix[,466:851] = test3Matrix[3:388] * test3id









###########################################################################
#finalFrame3 variables
#
#
#
############################################################################



train2Matrix[,452:514] = train2Matrix[,389:451] * train2id
test3Matrix[,452:514] = test3Matrix[389:451] * test3id



train2Matrix[,515:577] = train2Matrix[,389:451] * train2Matrix[,1]
test3Matrix[,515:577] = test3Matrix[389:451] * test3Matrix[,1]



train2Matrix[,578:640] = train2Matrix[,389:451] * train2Matrix[,2]
test3Matrix[,578:640] = test3Matrix[389:451] * test3Matrix[,2]



train2Matrix[,641:1026] = train2Matrix[,3:388] * train2Matrix[,2]
test3Matrix[,641:1026] = test3Matrix[3:388] * test3Matrix[,2]

train2Matrix = train2Matrix[,-c(1:452)]
test3Matrix = test3Matrix[,-c(1:452)]



###########################################################################
#finalFrame5 variables
#
#
#
############################################################################

train2Matrix[,1:451] = exp(-train2Matrix[,1:451])
test3Matrix[,1:451] = exp(-test3Matrix[,1:451])


##adding new variables
train2Matrix[,452:456] = 0
test3Matrix[,452:456] = 0

#only severity_type 1
train2Matrix[which(train2Matrix[,2] ==1),452] = 1
test3Matrix[which(test3Matrix[,2] ==1),452] = 1


#severtity_type 2 3 4 5
train2Matrix[which(train2Matrix[,2] !=1),453] = 1
test3Matrix[which(test3Matrix[,2] !=1),453] = 1


#only resource_type.8
train2Matrix[which(train2Matrix[,389] !=0),454] = 1
test3Matrix[which(test3Matrix[,389] !=0),454] = 1

#all other resource_types
train2Matrix[which(train2Matrix[,389] ==0),455] = 1
test3Matrix[which(test3Matrix[,389] ==0),455] = 1


#severity_type 1 or 2

train2Matrix[which(train2Matrix[,2] ==1  | train2Matrix[,2] == 2),456] = 1
test3Matrix[which(test3Matrix[,2] ==1 | test3Matrix[,2] == 2),456] = 1




train2Matrix[,457:465] = 0
test3Matrix[,457:465] = 0

#severity_type*location
train2Matrix[,457] = train2Matrix[,1]* train2Matrix[,2]
test3Matrix[,457] = test3Matrix[,1] * test3Matrix[,2]


#severity_type * id
train2Matrix[,458] = train2id* train2Matrix[,2]
test3Matrix[,458] = test3id * test3Matrix[,2]


#location * id
train2Matrix[,459] = train2id* train2Matrix[,1]
test3Matrix[,459] = test3id * test3Matrix[,1]



#location > 750
train2Matrix[which(train2Matrix[,1] >750),460] = 1
test3Matrix[which(test3Matrix[,1] >750),460] = 1

#location >500 <= 750
train2Matrix[which(train2Matrix[,1] > 500  & train2Matrix[,1] <= 750),461] = 1
test3Matrix[which(test3Matrix[,1]   > 500  & test3Matrix[,1]  <= 750),461] = 1




#location >250 <= 500
train2Matrix[which(train2Matrix[,1] > 250  & train2Matrix[,1] <= 500),462] = 1
test3Matrix[which(test3Matrix[,1]   > 250  & test3Matrix[,1]  <= 500),462] = 1


#location <=250
train2Matrix[which( train2Matrix[,1] <= 250),463] = 1
test3Matrix[which( test3Matrix[,1]  <= 250),463] = 1



#location*severity_type <1850
train2Matrix[which( train2Matrix[,457] <= 1850),464] = 1
test3Matrix[which( test3Matrix[,457]  <= 1850),464] = 1



#location*severity_type <400
train2Matrix[which( train2Matrix[,457] <400),465] = 1
test3Matrix[which( test3Matrix[,457]  < 400),465] = 1


##adding new variables
train2Matrix[,466:475] = 0
test3Matrix[,466:475] = 0


#location*severity_type <800 >=400
train2Matrix[which( train2Matrix[,457] < 800 & train2Matrix[,457] >= 400),466] = 1
test3Matrix[which( test3Matrix[,457]  < 800  & test3Matrix[,457]  >= 400),466] = 1



#location*severity_type <1200 >=800
train2Matrix[which( train2Matrix[,457] < 1200  & train2Matrix[,457] >= 800),467] = 1
test3Matrix[which( test3Matrix[,457]   < 1200  & test3Matrix[,457]  >= 800),467] = 1


#location*severity_type <1600 >=1200
train2Matrix[which( train2Matrix[,457] < 1600  & train2Matrix[,457] >= 1200),468] = 1
test3Matrix[which( test3Matrix[,457]   < 1600  & test3Matrix[,457]  >= 1200),468] = 1


#location*severity_type <2000 >=1600
train2Matrix[which( train2Matrix[,457] < 2000  & train2Matrix[,457] >= 1600),469] = 1
test3Matrix[which( test3Matrix[,457]   < 2000  & test3Matrix[,457]  >= 1600),469] = 1





#location*severity_type <2400 >=2000
train2Matrix[which( train2Matrix[,457] < 2400  & train2Matrix[,457] >= 2000),469] = 1
test3Matrix[which( test3Matrix[,457]   < 2400  & test3Matrix[,457]  >= 2000),469] = 1




#location*severity_type <2800 >=2400
train2Matrix[which( train2Matrix[,457] < 2800  & train2Matrix[,457] >= 2400),470] = 1
test3Matrix[which( test3Matrix[,457]   < 2800  & test3Matrix[,457]  >= 2400),470] = 1


#location*severity_type <3200 >=2800
train2Matrix[which( train2Matrix[,457] < 3200  & train2Matrix[,457] >= 2800),471] = 1
test3Matrix[which( test3Matrix[,457]   < 3200  & test3Matrix[,457]  >= 2800),471] = 1

#location*severity_type >= 3200
train2Matrix[which( train2Matrix[,457] >= 3200  ),472] = 1
test3Matrix[which( test3Matrix[,457]   >= 3200 ),472] = 1


#location* resource_type8


train2Matrix[,473] = train2Matrix[,389] * train2Matrix[,1]
test3Matrix[,473] = test3Matrix[,389] * test3Matrix[,1]


train2Matrix[,474:483] = train2Matrix[,389:398] * train2id
test3Matrix[,474:483] = test3Matrix[,389:398] * test3id












###########################################################################
#finalFrame6 variables
#
#
#
############################################################################



##adding new variables
train2Matrix[,452:456] = 0
test3Matrix[,452:456] = 0

#only severity_type 1
train2Matrix[which(train2Matrix[,2] ==1),452] = 1
test3Matrix[which(test3Matrix[,2] ==1),452] = 1


#severtity_type 2 3 4 5
train2Matrix[which(train2Matrix[,2] !=1),453] = 1
test3Matrix[which(test3Matrix[,2] !=1),453] = 1


#only resource_type.8
train2Matrix[which(train2Matrix[,389] !=0),454] = 1
test3Matrix[which(test3Matrix[,389] !=0),454] = 1

#all other resource_types
train2Matrix[which(train2Matrix[,389] ==0),455] = 1
test3Matrix[which(test3Matrix[,389] ==0),455] = 1


#severity_type 1 or 2

train2Matrix[which(train2Matrix[,2] ==1  | train2Matrix[,2] == 2),456] = 1
test3Matrix[which(test3Matrix[,2] ==1 | test3Matrix[,2] == 2),456] = 1




train2Matrix[,457:465] = 0
test3Matrix[,457:465] = 0

#severity_type*location
train2Matrix[,457] = train2Matrix[,1]* train2Matrix[,2]
test3Matrix[,457] = test3Matrix[,1] * test3Matrix[,2]


#severity_type * id
train2Matrix[,458] = train2id* train2Matrix[,2]
test3Matrix[,458] = test3id * test3Matrix[,2]


#location * id
train2Matrix[,459] = train2id* train2Matrix[,1]
test3Matrix[,459] = test3id * test3Matrix[,1]



#location > 750
train2Matrix[which(train2Matrix[,1] >750),460] = 1
test3Matrix[which(test3Matrix[,1] >750),460] = 1

#location >500 <= 750
train2Matrix[which(train2Matrix[,1] > 500  & train2Matrix[,1] <= 750),461] = 1
test3Matrix[which(test3Matrix[,1]   > 500  & test3Matrix[,1]  <= 750),461] = 1




#location >250 <= 500
train2Matrix[which(train2Matrix[,1] > 250  & train2Matrix[,1] <= 500),462] = 1
test3Matrix[which(test3Matrix[,1]   > 250  & test3Matrix[,1]  <= 500),462] = 1


#location <=250
train2Matrix[which( train2Matrix[,1] <= 250),463] = 1
test3Matrix[which( test3Matrix[,1]  <= 250),463] = 1



#location*severity_type <1850
train2Matrix[which( train2Matrix[,457] <= 1850),464] = 1
test3Matrix[which( test3Matrix[,457]  <= 1850),464] = 1



#location*severity_type <400
train2Matrix[which( train2Matrix[,457] <400),465] = 1
test3Matrix[which( test3Matrix[,457]  < 400),465] = 1


##adding new variables
train2Matrix[,466:480] = 0
test3Matrix[,466:480] = 0


#location*severity_type <800 >=400
train2Matrix[which( train2Matrix[,457] < 800 & train2Matrix[,457] >= 400),466] = 1
test3Matrix[which( test3Matrix[,457]  < 800  & test3Matrix[,457]  >= 400),466] = 1



#location*severity_type <1200 >=800
train2Matrix[which( train2Matrix[,457] < 1200  & train2Matrix[,457] >= 800),467] = 1
test3Matrix[which( test3Matrix[,457]   < 1200  & test3Matrix[,457]  >= 800),467] = 1


#location*severity_type <1600 >=1200
train2Matrix[which( train2Matrix[,457] < 1600  & train2Matrix[,457] >= 1200),468] = 1
test3Matrix[which( test3Matrix[,457]   < 1600  & test3Matrix[,457]  >= 1200),468] = 1


#location*severity_type <2000 >=1600
train2Matrix[which( train2Matrix[,457] < 2000  & train2Matrix[,457] >= 1600),469] = 1
test3Matrix[which( test3Matrix[,457]   < 2000  & test3Matrix[,457]  >= 1600),469] = 1





#location*severity_type <2400 >=2000
train2Matrix[which( train2Matrix[,457] < 2400  & train2Matrix[,457] >= 2000),469] = 1
test3Matrix[which( test3Matrix[,457]   < 2400  & test3Matrix[,457]  >= 2000),469] = 1




#location*severity_type <2800 >=2400
train2Matrix[which( train2Matrix[,457] < 2800  & train2Matrix[,457] >= 2400),470] = 1
test3Matrix[which( test3Matrix[,457]   < 2800  & test3Matrix[,457]  >= 2400),470] = 1


#location*severity_type <3200 >=2800
train2Matrix[which( train2Matrix[,457] < 3200  & train2Matrix[,457] >= 2800),471] = 1
test3Matrix[which( test3Matrix[,457]   < 3200  & test3Matrix[,457]  >= 2800),471] = 1

#location*severity_type >= 3200
train2Matrix[which( train2Matrix[,457] >= 3200  ),472] = 1
test3Matrix[which( test3Matrix[,457]   >= 3200 ),472] = 1


#location* resource_type8


train2Matrix[,473] = train2Matrix[,389] * train2Matrix[,1]
test3Matrix[,473] = test3Matrix[,389] * test3Matrix[,1]

#location* resource_type8 < 250
train2Matrix[which(train2Matrix[,473] <250),474] = 1
test3Matrix[which(test3Matrix[,473] <250),474] = 1


#location* resource_type8 <500 >= 250
train2Matrix[which( train2Matrix[,473] < 500  & train2Matrix[,473] >= 250),475] = 1
test3Matrix[which( test3Matrix[,473]   < 500  & test3Matrix[,473]  >= 250),475] = 1


#location* resource_type8 <750 >= 500
train2Matrix[which( train2Matrix[,473] < 750  & train2Matrix[,473] >= 500),476] = 1
test3Matrix[which( test3Matrix[,473]   < 750  & test3Matrix[,473]  >= 500),476] = 1


#location* resource_type8 <1000 >= 750
train2Matrix[which( train2Matrix[,473] < 1000  & train2Matrix[,473] >= 750),477] = 1
test3Matrix[which( test3Matrix[,473]   < 1000  & test3Matrix[,473]  >= 750),477] = 1




#location* resource_type8 >=1000
train2Matrix[which( train2Matrix[,473] >= 1000) ,478] = 1
test3Matrix[which( test3Matrix[,473] >=1000),478] = 1



#location if  severity_type = 1
train2Matrix[which(train2Matrix[,2] == 1),479] = train2Matrix[which(train2Matrix[,2] == 1),1]
test3Matrix[which(test3Matrix[,2]  == 1),479]  = test3Matrix[which(test3Matrix[,2]  == 1),1] 

#location if  severity_type = 2
train2Matrix[which(train2Matrix[,2] == 2),480] = train2Matrix[which(train2Matrix[,2] == 2),1]
test3Matrix[which(test3Matrix[,2]  == 2),480]  = test3Matrix[which(test3Matrix[,2]  == 2),1] 



train2Matrix[,481:491] = 0
test3Matrix[,481:491] = 0
#location <200
train2Matrix[which(train2Matrix[,479] < 200),481]  = 1
test3Matrix[which(test3Matrix[,479]   < 200),481]  = 1 

#location <400  >= 200
train2Matrix[which(train2Matrix[,479] < 400  &  train2Matrix[,479] >= 200 ),482]  = 1
test3Matrix[which(test3Matrix[,479]   < 400  &  test3Matrix[,479]  >= 200 ),482]  = 1 




#location <600  >= 400
train2Matrix[which(train2Matrix[,479] < 600  &  train2Matrix[,479] >= 400 ),483]  = 1
test3Matrix[which(test3Matrix[,479]   < 600  &  test3Matrix[,479]  >= 400 ),483]  = 1



 

#location <800  >= 600
train2Matrix[which(train2Matrix[,479] < 800  &  train2Matrix[,479] >= 600 ),484]  = 1
test3Matrix[which(test3Matrix[,479]   < 800  &  test3Matrix[,479]  >= 600 ),484]  = 1



#location <1000  >= 800
train2Matrix[which(train2Matrix[,479] < 1000  &  train2Matrix[,479] >= 800 ),485]  = 1
test3Matrix[which(test3Matrix[,479]   < 1000  &  test3Matrix[,479]  >= 800 ),485]  = 1




#location >= 1000
train2Matrix[which(train2Matrix[,479] >=1000 ),486]  = 1
test3Matrix[which(test3Matrix[,479]   >=1000 ),486]  = 1




#location * id if  severity_type = 1
train2Matrix[which(train2Matrix[,2] == 1),487] = train2Matrix[which(train2Matrix[,2] == 1),1] * train2id[which(train2Matrix[,2] == 1)]
test3Matrix[which(test3Matrix[,2]  == 1),487]  = test3Matrix[which(test3Matrix[,2]  == 1),1]* test3id[which(test3Matrix[,2]  == 1)] 



#location *id < 1.5 million
train2Matrix[which(train2Matrix[,487] < 1500000  ),488]  = 1
test3Matrix[which(test3Matrix[,487]   < 1500000  ),488]  = 1 

#location *id >= 1.5 million  < 3 mill
train2Matrix[which(train2Matrix[,487] < 3000000  &  train2Matrix[,487] >= 1500000 ),489]  = 1
test3Matrix[which(test3Matrix[,487]   < 3000000  &  test3Matrix[,487]  >= 1500000 ),489]  = 1 



#location *id >= 3 million  < 4.5 mill
train2Matrix[which(train2Matrix[,487] >= 3000000  &  train2Matrix[,487] < 4500000 ),490]  = 1
test3Matrix[which(test3Matrix[,487]   >= 3000000  &  test3Matrix[,487]  < 4500000 ),490]  = 1 



#location *id >= 4.5 million  < 6 mill
train2Matrix[which(train2Matrix[,487] >= 4500000  &  train2Matrix[,487] < 6000000 ),491]  = 1
test3Matrix[which(test3Matrix[,487]   >= 4500000  &  test3Matrix[,487]  < 6000000 ),491]  = 1





train2Matrix[,492:493] = 0
test3Matrix[,492:493] = 0



#location *id >= 6 million  < 7.5 mill
train2Matrix[which(train2Matrix[,487] >= 6000000  &  train2Matrix[,487] < 7500000 ),492]  = 1
test3Matrix[which(test3Matrix[,487]   >= 6000000  &  test3Matrix[,487]  < 7500000 ),492]  = 1





#location *id >= 7.5mill
train2Matrix[which(train2Matrix[,487] >= 7500000  ),493]  = 1
test3Matrix[which(test3Matrix[,487]   >= 7500000  ),493]  = 1


train2Matrix = train2Matrix[,452:493]
test3Matrix = test3Matrix[,452:493]




train2Matrix[,452] = train2[,1] * train2[,2]
test3Matrix[,452] = test3Matrix[,1] * test3Matrix[,2]



train2Matrix[,452] = train2Matrix[,1] * train2Matrix[,2]
test3Matrix[,452] = test3Matrix[,1] * test3Matrix[,2]

train2Matrix[,453:515]= train2Matrix[,389:451] * train2Matrix[,389:451]
test3Matrix[,453:515] = test3Matrix[,389:451] * test3Matrix[,389:451]






s1id = train2id[which(train2Matrix[,2] == 1)]
s1Testid = test3id[which(test3Matrix[,2] == 1)]
s1 = train2Matrix[which(train2Matrix[,2] == 1),1:465]
s1Test = test3Matrix[which(test3Matrix[,2] == 1),1:465]




sALLid = train2id[which(train2Matrix[,2] != 1)]
sAllTestid = test3id[which(test3Matrix[,2] != 1)]
sALL = train2Matrix[which(train2Matrix[,2] != 1),1:465]
sALLTest = test3Matrix[which(test3Matrix[,2] != 1),1:465]




train2Matrix[,1:451] = exp(-train2Matrix[,1:451])
test3Matrix[,1:451] = exp(-test3Matrix[,1:451])

train2Matrix[,133:142] = train2Matrix[,123:132] * train2Matrix[,1]
test3Matrix[,133:142] = test3Matrix[,123:132] * test3Matrix[,1]

train2Matrix[,1:451] = train2Matrix[,1:451] * train2Matrix[,1:451]
test3Matrix[,1:451] = test3Matrix[,1:451] * test3Matrix[,1:451]


train2Matrix = data.matrix(train2Matrix)
test3Matrix = data.matrix(test3Matrix)






Normalize  <- function(train2)
{


	#scale() is what normalizes the column, have to cast it as numeric first
	#iterates over ever column deemed numeric by the numericColumns function
	for (i in 1:451)
	{
		train2[,i] = as.numeric(scale(train2[,i])) 
	}	
	
	return(train2);
}

train2Matrix = Normalize(train2Matrix)
test3Matrix = Normalize(test3Matrix)

for (i in 1:ncol(train2Matrix))
{
train2Matrix[which(is.na(train2Matrix[,i])),i] = 0
}

for (i in 1:ncol(test3Matrix))
{
test3Matrix[which(is.na(test3Matrix[,i])),i] = 0
}





###################################################################################
#Attempting an extraTrees model
#extraTrees(x,y, mtry = 50, nodesize = 5, numRandomCuts = 5) = .7690955
#extraTrees(x,y, mtry = 10, nodesize = 5, numRandomCuts = 5) = .5759467
#
#
####################################################################################

x = train2Matrix
y = as.factor(train2_response)



eT = extraTrees(x,y, mtry = 5, nodesize = 5, numRandomCuts = 5)
etOut = predict(eT, newdata = test3Matrix, probability=TRUE)
etOut = as.data.frame(etOut)


#initialize output frame
etFrame = data.frame(matrix(nrow= nrow(test2), ncol=4))
etFrame = rename(etFrame, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1","X4" = "predict_2")) 

#Puts the ids for the observations into the first column of outputFrame[,1]
etFrame[,1] = test3id


etFrame[,2:4] = etOut[,1:3]


num_predict = 3
log_loss(etFrame,num_predict)


#######################################################################################
#All xgboost ensemble
#1/5 xgFrame1:5 gives .5688524 
#
#1/7 xgFrame1:7 gives .5665393 
#2/7 xgFrame3:4 1/7 xgFrame5:7 .5613909
######################################################################################


ensembleFrame = data.frame(matrix(nrow= nrow(test2), ncol=4))
ensembleFrame = rename(ensembleFrame, c("X1" = "id", "X2" = "predict_0", 
		"X3" = "predict_1","X4" = "predict_2")) 



ensembleFrame[,1] = xgFrame1[,1]
#checks to make sure the the ids are the same
sum(ensembleFrame[,1] != outputFrame[,1])


ensembleFrame[,2:4] =  ((0)*xgFrame1[,2:4] + (0)*xgFrame2[,2:4]
	+ (4/7)*xgFrame3[,2:4] + (0)*xgFrame4[,2:4] + (1/7)*xgFrame5[,2:4] +
(1/7)*xgFrame6[,2:4]+
(1/7)*xgFrame7[,2:4] ) 
#+ (0)*xgFrame8[,2:4] + (0)*xgFrame9[,2:4]
#	+ (0) * xgFrame10[,2:4])

as.numeric(sum(ensembleFrame[,2:4])) 
nrow(test2)








num_predict = 3
log_loss(ensembleFrame,num_predict)


#sees the correlation among predictor variables
cor(cbind(xgFrame2[,2:4], etFrame[,2:4]))






train2Matrix = train10Matrix; test3Matrix=test10Matrix








################################################################################
#Random Forest attempt
#
#
#
#
###############################################################################

ranOut = randomForest( y = as.factor(train2_response), 
		x = train2Matrix[,keepTop] )
ranImportance = as.data.frame(importance(ranOut))
ranImportance[,2] = rownames(ranImportance)
ranImportance = rename(ranImportance, c('V2' = 'VarName'))



ranTop = arrange(ranImportance, MeanDecreaseGini)

ranTop = ranTop[1:250,]
head(ranTop)
plot(ranOut)
keepTop = which(colnames(train2Matrix) %in% ranTop[,2])




ranPred = predict(ranOut, newdata = test3Matrix, type = 'prob')

str(ranPred)
ranPred = as.data.frame(ranPred)


outputFrame4 = data.frame(matrix(nrow= nrow(test2), ncol=4))
outputFrame4 = rename(outputFrame4, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1","X4" = "predict_2")) 
outputFrame4[,1] = test3id
outputFrame4[,2:4] = ranPred[,1:3]



num_predict = 3
log_loss(outputFrame4,num_predict)











###########################################################################
#RandomForest NDCG = 11.3959 XGBoost = .5803582
#
#ensembleFrame = .5 *XGBoost + .5* RandomForest = .6828388
#ensembleFrame = .75 *XGBoost + .25* RandomForest = .6035602
#ensembleFrame = .85 *XGBoost + .15* RandomForest = .587915
##ensembleFrame = .9 *XGBoost + .1* RandomForest = .5831317

#deepLearning NDCG = .70 XGBoost = .0.5317137
#ensembleFrame = .5 *XGBoost + .5* deepLearning =  0.5743975
#ensembleFrame = .7 *XGBoost + .3* RandomForest = .5513724
#ensembleFrame = .8 *XGBoost + .2* RandomForest = 0.5426687
#ensembleFrame = .95 *XGBoost + .05* RandomForest = 0.5334126

#deepLearning NDCG = on paper XGBoost = .0.532

#ensembleFrame = .1 *XGBoost + .1* dlFrame1:9 =  .7178831
#ensembleFrame = .55 *XGBoost + .05* dlFrame1:9 =  .5980807
#ensembleFrame = 1/9 *XGBoost + 1/9* dlFrame1:3 5:9 =  0.704988

#ensembleFrame = 11/19 *XGBoost + 1/19* dlFrame1:3 5:9 =  .5889603

#ensembleFrame = 1/8 *XGBoost + 1/8* dlFrame1:3 6:9 =  0.6881492

#ensembleFrame = 1/7 *XGBoost + 1/7* dlFrame2:3 6:9 =   0.7126687

#ensembleFrame = 11/17 *XGBoost + 1/17* dlFrame2:3 6:9 =  0.5783788

#ensembleFrame = 31/37 *XGBoost + 1/17* dlFrame2:3 6:9 =  0.5491831

#ensembleFrame = 30/35 *XGBoost + 1/5* dlFrame3 6:9 =  0.5469355


#ensembleFrame = 30/35 *XGBoost + 1/5* dlFrame3 6:8 =  0.5486761

#ensembleFrame = 30/35 *XGBoost + 1/5* dlFrame6:9 =  0.5477777


##ensembleFrame = .99*XGBoost + .01* dlFrame7 = 0.5359111

##ensembleFrame = .91*XGBoost + .03* dlFrame7:9 = 0.541342

##ensembleFrame = .98*XGBoost + .01* dlFrame8:9 =  0.5358049


##ensembleFrame = .99*XGBoost + .01* dlFrame10 =  0.5357707


#ensembleFrame = .5*XGBoost + .5* etFrame =  0.4832427

#ensembleFrame = .8*XGBoost + .2* etFrame =  0.4886569
#ensembleFrame = .2*XGBoost + .8* etFrame =  0.487004
#ensembleFrame = .6*XGBoost + .4* etFrame =  0.4831148
############################################################################
ensembleFrame = data.frame(matrix(nrow= nrow(test2), ncol=4))
ensembleFrame = rename(ensembleFrame, c("X1" = "id", "X2" = "predict_0", 
		"X3" = "predict_1","X4" = "predict_2")) 



ensembleFrame[,1] = outputFrame[,1]
#checks to make sure the the ids are the same
sum(ensembleFrame[,1] != outputFrame[,1])


ensembleFrame[,2:4] = ((.6)*outputFrame[,2:4] + (.4)*etFrame[,2:4]) 


+ (0)*dlFrame2[,2:4]
	+ (0)*dlFrame3[,2:4] + 0*dlFrame4[,2:4] + (0)*dlFrame5[,2:4] + (0)*dlFrame6[,2:4]
	 + (0)*dlFrame7[,2:4]  + (0)*dlFrame8[,2:4] + (0)*dlFrame9[,2:4]
	+ 0 * dlFrame10[,2:4])

as.numeric(sum(ensembleFrame[,2:4])) 
nrow(test2)








num_predict = 3
log_loss(ensembleFrame,num_predict)


#sees the correlation among predictor variables
cor(cbind(outputFrame[,2:4], dlFrame11[,2:4]))

#gives the columns in xgboost that are important
xgUse = which(colnames(train6) %in% as.data.frame(importance_matrix)[,1])


#############################################################################
#Deep Learning
#
#All variables .8 train .2 test log_loss = 1.0147
#
###############################################################################
	#initializes a thread by connecting to h2os clusters
	h2o.init(nthreads = -1)


	#turns the numeric outcome variable to a factor
	train2_response = as.factor(train2_response)
	test5[,3] = as.factor(test5[,3])

	trainOut = cbind(train2Matrix[,452:493],train2_response)
	#converts the two dataframes into h2o frames
	train5 = as.h2o(trainOut)
	test5 = as.h2o(test3Matrix[,452:493])

	explanFeatures = 1:42

	#builds the deep learning neural nets using only the features in explanFeatures
	#2 is the outcome feature
	trainDL = h2o.deeplearning(x = explanFeatures,y = 43 , training_frame = train5)

	#makes probability predictions on the test5 data using the model built
	predictions <- h2o.predict(trainDL, newdata = test5, type = "probs")

	#turns h2o output into dataframe
	DLPred = as.data.frame(predictions)


	dlFrame11 = data.frame(matrix(nrow= nrow(test), ncol=4))
	dlFrame11 = rename(dlFrame11, c("X1" = "id", "X2" = "predict_0", 
		"X3" = "predict_1","X4" = "predict_2")) 
	#adds ids back into outputFrame
	dlFrame11[,1] = test3id
	dlFrame11[,2:4] = DLPred[,2:4]

	log_loss(dlFrame11, 3)

##############################################################################
#write the results of importance matrix to a csv
#
#
#
#################################################################

#rows 1-133
write.xlsx(as.data.frame(importance_matrix),
	'C:/Users/Randy/Downloads/Telstra Kaggle Competion/importanceMatrix.xlsx'
	, append =TRUE)



write.xlsx(as.data.frame(importance_matrix),
	'C:/Users/Randy/Downloads/Telstra Kaggle Competion/importanceLogFeature.xlsx'
	, append =TRUE)


write.xlsx(as.data.frame(importance_matrix),
	'C:/Users/Randy/Downloads/Telstra Kaggle Competion/importanceResourceType.xlsx'
	, append =TRUE)


###############################################################
#
#The log_loss function takes two arguements, a data.frame and
#a vector of length 1 num_predict
#The data.frame has ncol = num_predict +1  with column 1 = id, column 2 = predict_0, 
#column 3 = predict_1 ... etc
#
#It will multiply the log of the predicted probability times 1 if the observation
#turned out to be that category, 0 otherwise
#It sums all of those up and divides by -N, where N is the number of 
#observations in data_frame
#
#################################################################
log_loss <- function(data_frame, num_predict)
{
	total = 0
	for( i in 1:nrow(data_frame))
	{
		
		for(j in 1: num_predict)
		{
			y=0
			#gets the id from the ith row of the data_frame
			#if the actual fault_severity == j-1 then that is when y is 1
			#This is the classification of the point
			#The [1] just takes the first in case their are duplicate observations
			#of the id
			if (test2[which(test2$id==data_frame$id[i])[1],3] == (j-1))
			{
				y=1;
				
			}

			#total is equal to total plus y times
			# the log of the ith row and the j+1 column
			#it is j+1 because predict_0 is in column 2
			total = total + y*log( max( min( data_frame[i,(j+1)], 1-10^(-15) ),  10^(-15) ) )
			

		}
		
	}
	print("Your logloss score is:")
	print(-total/nrow(data_frame))


}






