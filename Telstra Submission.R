########################################################################################
#Getting it into a submission after the model has been trained
#
#
#
#
#########################################################################################









################################################################
#	implementation of extreme gradient boosting algorithms(xgboost)
#
#
##################################################################


#extracts the location variable as a string
test[,2] = as.numeric(str_sub(test$location, start= 10))




#extracts severity_type
test[,3] = as.numeric(str_sub(test$severity_type, start= 15))


test3id = test[,1]
test3 = test[,-c(1)]

#checks that the number of ids in the vector is equal to the number of rows in 
#the data frames

length(test3id) == nrow(test3)






test3Matrix = as.matrix(test3)


testKeep = which(colnames(test3Matrix) %in% colnames(train2Matrix))
test3Matrix = test3Matrix[,testKeep]






#the predictions are in a nrow(test3)*3 long vector
#bstPred[1:3] is the probability of 0,1,2 for fault_severity
#for the first observation of test
#has to be a numeric matrix just like the training set
bstPred = predict(bst, test3Matrix)
is.vector(bstPred)
str(bstPred)


#initialize output frame
outputFrame = data.frame(matrix(nrow= nrow(test), ncol=4))
outputFrame = rename(outputFrame, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1","X4" = "predict_2")) 

#Puts the ids for the observations into the first column of outputFrame[,1]
outputFrame[,1] = test[,1]
#test to make sure ids are the same
sum(outputFrame[,1] != test[,1])
z_element = 1
for (i in 1:nrow(test))
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





#validation
nrow(outputFrame) == length(unique(test$id))
sum(outputFrame$id != unique(test$id))
sum(is.na(outputFrame))

#should be 11171
sum(outputFrame[,2:4])





#write to the file
write.csv(outputFrame, "C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\Results6.csv",
		row.names = FALSE)





Result1 = read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\Results.csv")


Result2 = read.csv("C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\Results2.csv")


Difference = abs((Result1[,2:4] - outputFrame[,2:4]))
#write the data frame to an excel file
write.xlsx(outputFrame,'C:/Users/Randy/Downloads/Kaggle Telstra/Results2.xlsx')

write.xlsx(outputFrame,'C:/Users/Randy/Downloads/Kaggle Telstra/Results3.xlsx')







































