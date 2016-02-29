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


#testKeep = which(colnames(test3Matrix) %in% colnames(train2Matrix))
#test3Matrix = test3Matrix[,testKeep]






#the predictions are in a nrow(test3)*3 long vector
#bstPred[1:3] is the probability of 0,1,2 for fault_severity
#for the first observation of test
#has to be a numeric matrix just like the training set
bstPred = predict(bst, test3Matrix)
is.vector(bstPred)
str(bstPred)


#initialize output frame
finalFrame = data.frame(matrix(nrow= nrow(test), ncol=4))
finalFrame = rename(finalFrame, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1","X4" = "predict_2")) 

#Puts the ids for the observations into the first column of finalFrame[,1]
finalFrame[,1] = test[,1]
#test to make sure ids are the same
sum(finalFrame[,1] != test[,1])
z_element = 1
for (i in 1:nrow(test))
{
	for (z in 1:3)
	{
		#the ith row of finalFrame is given observation z_element
		#probability of occuring from bstPred
		#column z+1 since id is in column 1
		finalFrame[i,z+1] = bstPred[z_element]
		z_element = z_element + 1
	}
}





#validation
nrow(finalFrame) == length(unique(test$id))
sum(finalFrame$id != unique(test$id))
sum(is.na(finalFrame))

#should be 11171
sum(finalFrame[,2:4])

write.csv(finalFrame, "C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\finalFrame.csv",
		row.names = FALSE)
###########################################################################################################
#extraTrees model
#
#
#
##########################################################################################################







etOut2 = predict(eT, newdata = test3Matrix, probability=TRUE)
etOut2 = as.data.frame(etOut2)


#initialize output frame
finalFrame2 = data.frame(matrix(nrow= nrow(test), ncol=4))
finalFrame2 = rename(finalFrame2, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1","X4" = "predict_2")) 

#Puts the ids for the observations into the first column of outputFrame[,1]
finalFrame2[,1] = test3id


finalFrame2[,2:4] = etOut2[,1:3]




#validation
nrow(finalFrame2) == length(unique(test$id))
sum(finalFrame2$id != unique(test$id))
sum(is.na(finalFrame2))

#should be 11171
sum(finalFrame2[,2:4])

write.csv(finalFrame2, "C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\finalFrame2.csv",
		row.names = FALSE)










###########################################################################################################
#extraTrees model 2 
#
#
#
##########################################################################################################






#needed more memory so I ran:
# options( java.parameters = "-Xmx4g" )

etOut2 = predict(eT, newdata = test3Matrix, probability=TRUE)
etOut2 = as.data.frame(etOut2)


#initialize output frame
finalFrame3 = data.frame(matrix(nrow= nrow(test), ncol=4))
finalFrame3 = rename(finalFrame3, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1","X4" = "predict_2")) 

#Puts the ids for the observations into the first column of outputFrame[,1]
finalFrame3[,1] = test3id


finalFrame3[,2:4] = etOut2[,1:3]




#validation
nrow(finalFrame3) == length(unique(test$id))
sum(finalFrame3$id != unique(test$id))
sum(is.na(finalFrame3))

#should be 11171
sum(finalFrame3[,2:4])

write.csv(finalFrame3, "C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\finalFrame3.csv",
		row.names = FALSE)








###########################################################################################################
#extraTrees model 4 
#
#
#
##########################################################################################################






#needed more memory so I ran:
# options( java.parameters = "-Xmx4g" )

etOut2 = predict(eT, newdata = test3Matrix, probability=TRUE)
etOut2 = as.data.frame(etOut2)


#initialize output frame
finalFrame4 = data.frame(matrix(nrow= nrow(test), ncol=4))
finalFrame4 = rename(finalFrame4, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1","X4" = "predict_2")) 

#Puts the ids for the observations into the first column of outputFrame[,1]
finalFrame4[,1] = test3id


finalFrame4[,2:4] = etOut2[,1:3]




#validation
nrow(finalFrame4) == length(unique(test$id))
sum(finalFrame4$id != unique(test$id))
sum(is.na(finalFrame4))

#should be 11171
sum(finalFrame4[,2:4])

write.csv(finalFrame4, "C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\finalFrame4.csv",
		row.names = FALSE)






cor(cbind(finalFrame[,4], finalFrame2[,4], finalFrame3[,4], finalFrame4[,4]))






###########################################################################################################
#extraTrees model 5 
#
#
#
##########################################################################################################






#needed more memory so I ran:
# options( java.parameters = "-Xmx4g" )

etOut2 = predict(eT, newdata = test3Matrix, probability=TRUE)
etOut2 = as.data.frame(etOut2)


#initialize output frame
finalFrame5 = data.frame(matrix(nrow= nrow(test), ncol=4))
finalFrame5 = rename(finalFrame5, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1","X4" = "predict_2")) 

#Puts the ids for the observations into the first column of outputFrame[,1]
finalFrame5[,1] = test3id


finalFrame5[,2:4] = etOut2[,1:3]




#validation
nrow(finalFrame5) == length(unique(test$id))
sum(finalFrame5$id != unique(test$id))
sum(is.na(finalFrame5))

#should be 11171
sum(finalFrame5[,2:4])

write.csv(finalFrame5, "C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\finalFrame5.csv",
		row.names = FALSE)




###########################################################################################################
#extraTrees model 6 
#
#
#
##########################################################################################################






#needed more memory so I ran:
# options( java.parameters = "-Xmx4g" )

etOut2 = predict(eT, newdata = test3Matrix, probability=TRUE)
etOut2 = as.data.frame(etOut2)


#initialize output frame
finalFrame6 = data.frame(matrix(nrow= nrow(test), ncol=4))
finalFrame6 = rename(finalFrame6, c("X1" = "id", "X2" = "predict_0", "X3" = "predict_1","X4" = "predict_2")) 

#Puts the ids for the observations into the first column of outputFrame[,1]
finalFrame6[,1] = test3id


finalFrame6[,2:4] = etOut2[,1:3]




#validation
nrow(finalFrame6) == length(unique(test$id))
sum(finalFrame6$id != unique(test$id))
sum(is.na(finalFrame6))

#should be 11171
sum(finalFrame6[,2:4])

write.csv(finalFrame6, "C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\finalFrame6.csv",
		row.names = FALSE)















cor(cbind(finalFrame[,4], finalFrame2[,4], finalFrame3[,4], finalFrame4[,4], finalFrame5[,4]))


















##########################################################################################################
#finalEnsemble between the two models
#
#
#
#
###########################################################################################################

finalEnsemble = data.frame(matrix(nrow= nrow(test), ncol=4))
finalEnsemble = rename(finalEnsemble, c("X1" = "id", "X2" = "predict_0", 
		"X3" = "predict_1","X4" = "predict_2")) 



finalEnsemble[,1] = test3id
#checks to make sure the the ids are the same
sum(finalEnsemble[,1] != test3id)


finalEnsemble[,2:4] = ((.7)*finalFrame[,2:4] + (.125)*finalFrame2[,2:4] + (.125) * finalFrame3[,2:4]
					+(.05)* finalFrame4[,2:4]) 

#validation
nrow(finalEnsemble) == length(unique(test$id))
sum(finalEnsemble$id != unique(test$id))
sum(is.na(finalEnsemble))

#should be 11171
sum(finalEnsemble[,2:4])





#write to the file
write.csv(finalEnsemble, "C:\\Users\\Randy\\Downloads\\Kaggle Telstra\\Results13.csv",
		row.names = FALSE)








































