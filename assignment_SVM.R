###########################################################
######### Please fill the ??? with proper description (atleast 130 charaters for each)
######### for SVM function try different values to achieve better results


# loading neccessary packages and dataset
# install.packages("caret")
# install.packages("lattice")
# install.packages("ggplot2")
library(ggplot2)
library(caret)
library(e1071)
data(GermanCredit)
write.csv(GermanCredit,"GermanCredit.csv")
dataset = GermanCredit
  

# Show the structure of dataset
# Change type of column 1 to 7 to dataframe
# scale means: Normalized the data from column 1 to 7
str(dataset)
dataset[,1:7] = as.data.frame(lapply(dataset[,1:7], scale))
str(dataset)




# Take a smaple of 200 random numbers less than 1000 as sample_index
# Divided the dataset to test_dataset and train_dataset
# Take all the value of the index of sample_index in dataset and assign it to test_dataset
# Take all reminder value of dataset and assign it to train_dataset
# "-" in sample_index means when it is false
sample_index = sample(1000, 200)
test_dateset = dataset[sample_index,]
train_dateset = dataset[-sample_index,]




# Tuning SVM for find best parameters for all possible 4 kernels
tune.svm(Class~., data = train_dateset, gamma = 10^(-1:-1), cost = 10^(-1:1))


#-----------------Try different value to find best parameters----------------------------------------

model = svm(Class ~ ., kernel = "radial", cost = 0.1, gamma = 0.01, data = train_dateset, scale = F)
predictions <-  predict(model, test_dateset[-10])
table(test_dateset[,10], predictions)


model = svm(Class ~ ., kernel = "radial", cost = 0.3, gamma = 0.03, data = train_dateset, scale = F)
predictions <-  predict(model, test_dateset[-10])
table(test_dateset[,10], predictions)

model = svm(Class ~ ., kernel = "radial", cost =0.4 , gamma = 0.04, data = train_dateset, scale = F)
predictions <-  predict(model, test_dateset[-10])
table(test_dateset[,10], predictions)


model = svm(Class ~ ., kernel = "radial", cost =0.5 , gamma = 0.05, data = train_dateset, scale = F)
predictions <-  predict(model, test_dateset[-10])
table(test_dateset[,10], predictions)

model = svm(Class ~ ., kernel = "radial", cost =0.7 , gamma = 0.07, data = train_dateset, scale = F)
predictions <-  predict(model, test_dateset[-10])
table(test_dateset[,10], predictions)

model = svm(Class ~ ., kernel = "radial", cost =0.9 , gamma = 0.09, data = train_dateset, scale = F)
predictions <-  predict(model, test_dateset[-10])
table(test_dateset[,10], predictions)

model = svm(Class ~ ., kernel = "radial", cost = 1, gamma = 0.1, data = train_dateset, scale = F)
predictions <-  predict(model, test_dateset[-10])
table(test_dateset[,10], predictions)

# By trying different cost & gamma values, the best parameters are cost = 1, gamma = 0.1
#----------------------------------------------------------------------------------------------------


# Predicting the Values using model created using SVM.
predictions <-  predict(model, test_dateset[-10])


# In order to see the result of prediction, 
# using command table to compare the result of SVM prediction and the test_data 
table(test_dateset[,10], predictions)










