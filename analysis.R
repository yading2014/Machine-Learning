setwd("~/Google Drive/JobApplication/DataScicentist/courses/08_PracticalMachineLearning/writeUp")

library(caret)
# "classe" is the target variable  
data <- read.csv("pml-training.csv")

factor_names <- names(data)
observations <- nrow(data)
noise <- vector()
count <- 1 

# remove NA data points 
for (i in seq(1:160)){
    na_number <- sum(is.na(data[factor_names[i]]))    
    if (na_number > observations/2 ){
        noise[count] <- factor_names[i]
        count <- count + 1 
        print(factor_names[i])  
    }
}

redundent <- c("raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")
null_part1 <- c("kurtosis_roll_belt", "kurtosis_picth_belt", "kurtosis_yaw_belt", 
                "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt", 
                "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt")
null_part2 <- c("kurtosis_roll_arm", "kurtosis_picth_arm", "kurtosis_yaw_arm", 
                "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm")
null_part3 <- c("kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", 
                "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell", "skewness_pitch_dumbbell", 
                "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell", "amplitude_yaw_dumbbell")
null_part4 <- c("kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm", 
                "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", 
                "max_yaw_forearm", "min_yaw_forearm", "amplitude_yaw_forearm")
noises <- c(noise, redundent, null_part1, null_part2, null_part3, null_part4)

my_vars <- factor_names %in% noises 
clean_data <- data[!my_vars]
clean_data <- clean_data[, 3:55]

# Training part
inTrain <- createDataPartition(y=clean_data$classe, p=0.7, list=FALSE)
training <- clean_data[inTrain, ]
testing <- clean_data[-inTrain, ]

#M <- abs(cor(training[,c(3:55)]))

modFit <- train(classe~., method="rpart", data=training)

result <- predict(modFit, newdata=testing)
sum(result==testing$classe)
# Read Validation Data 
validation <- read.csv("pml-testing.csv")
validate <- predict(modFit, newdata=validation)
print(validate) 