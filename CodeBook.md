##The Code Book of GettingData CourseProject

###Transformations of raw data
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

A Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

The training and the test sets were merged to create one data set.Only the measurements on the mean and standard deviation for each measurement were extracted. 

Finally a independent tidy data set with the average of each variable for each activity and each subject was created.

###Informations of the features
These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean


These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

###The complete list of variables

 [1] "subject"                             
 [2] "activity"                            
 [3] "tBodyAcc-mean()-Y"                   
 [4] "tBodyAcc-mean()-Z"                   
 [5] "tBodyAcc-std()-X"                    
 [6] "tBodyAcc-std()-Y"                    
 [7] "tBodyAcc-std()-Z"                    
 [8] "tGravityAcc-mean()-X"                
 [9] "tGravityAcc-mean()-Y"                
[10] "tGravityAcc-mean()-Z"                
[11] "tGravityAcc-std()-X"                 
[12] "tGravityAcc-std()-Y"                 
[13] "tGravityAcc-std()-Z"                 
[14] "tBodyAccJerk-mean()-X"               
[15] "tBodyAccJerk-mean()-Y"               
[16] "tBodyAccJerk-mean()-Z"               
[17] "tBodyAccJerk-std()-X"                
[18] "tBodyAccJerk-std()-Y"                
[19] "tBodyAccJerk-std()-Z"                
[20] "tBodyGyro-mean()-X"                  
[21] "tBodyGyro-mean()-Y"                  
[22] "tBodyGyro-mean()-Z"                  
[23] "tBodyGyro-std()-X"                   
[24] "tBodyGyro-std()-Y"                   
[25] "tBodyGyro-std()-Z"                   
[26] "tBodyGyroJerk-mean()-X"              
[27] "tBodyGyroJerk-mean()-Y"              
[28] "tBodyGyroJerk-mean()-Z"              
[29] "tBodyGyroJerk-std()-X"               
[30] "tBodyGyroJerk-std()-Y"               
[31] "tBodyGyroJerk-std()-Z"               
[32] "tBodyAccMag-mean()"                  
[33] "tBodyAccMag-std()"                   
[34] "tGravityAccMag-mean()"               
[35] "tGravityAccMag-std()"                
[36] "tBodyAccJerkMag-mean()"              
[37] "tBodyAccJerkMag-std()"               
[38] "tBodyGyroMag-mean()"                 
[39] "tBodyGyroMag-std()"                   
[40] "tBodyGyroJerkMag-mean()"             
[41] "tBodyGyroJerkMag-std()"              
[42] "fBodyAcc-mean()-X"                   
[43] "fBodyAcc-mean()-Y"                   
[44] "fBodyAcc-mean()-Z"                   
[45] "fBodyAcc-std()-X"                    
[46] "fBodyAcc-std()-Y"                    
[47] "fBodyAcc-std()-Z"                    
[48] "fBodyAcc-meanFreq()-X"               
[49] "fBodyAcc-meanFreq()-Y"               
[50] "fBodyAcc-meanFreq()-Z"               
[51] "fBodyAccJerk-mean()-X"               
[52] "fBodyAccJerk-mean()-Y"               
[53] "fBodyAccJerk-mean()-Z"               
[54] "fBodyAccJerk-std()-X"                
[55] "fBodyAccJerk-std()-Y"                
[56] "fBodyAccJerk-std()-Z"                
[57] "fBodyAccJerk-meanFreq()-X"           
[58] "fBodyAccJerk-meanFreq()-Y"           
[59] "fBodyAccJerk-meanFreq()-Z"           
[60] "fBodyGyro-mean()-X"                  
[61] "fBodyGyro-mean()-Y"                  
[62] "fBodyGyro-mean()-Z"                  
[63] "fBodyGyro-std()-X"                   
[64] "fBodyGyro-std()-Y"                   
[65] "fBodyGyro-std()-Z"                   
[66] "fBodyGyro-meanFreq()-X"              
[67] "fBodyGyro-meanFreq()-Y"              
[68] "fBodyGyro-meanFreq()-Z"              
[69] "fBodyAccMag-mean()"                  
[70] "fBodyAccMag-std()"                   
[71] "fBodyAccMag-meanFreq()"              
[72] "fBodyBodyAccJerkMag-mean()"          
[73] "fBodyBodyAccJerkMag-std()"           
[74] "fBodyBodyAccJerkMag-meanFreq()"      
[75] "fBodyBodyGyroMag-mean()"             
[76] "fBodyBodyGyroMag-std()"              
[77] "fBodyBodyGyroMag-meanFreq()"         
[78] "fBodyBodyGyroJerkMag-mean()"         
[79] "fBodyBodyGyroJerkMag-std()"          
[80] "fBodyBodyGyroJerkMag-meanFreq()"     
[81] "angle(tBodyAccMean,gravity)"         
[82] "angle(tBodyAccJerkMean),gravityMean)"
[83] "angle(tBodyGyroMean,gravityMean)"    
[84] "angle(tBodyGyroJerkMean,gravityMean)"
[85] "angle(X,gravityMean)"                
[86] "angle(Y,gravityMean)"                
[87] "angle(Z,gravityMean)"                