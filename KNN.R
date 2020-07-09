#Diagnosing breast cancer with the  kNN algorithm
library(class)
library(gmodels)

wbcd <- read.csv("F:/Data Analytics-MS!/Predictive Analytics/Week 2/Machine-Learning-with-R-datasets-master/wisc_bc_data.csv", stringsAsFactors = FALSE)

str(wbcd)
wbcd <- wbcd[-1]

table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#Transformation - normalizing numeric data

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))  }

#Testing
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

#Normalizing dataset
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n$area_mean)

#creating training and  test datasets
wbcd_train <- wbcd_n[1:469, ] 
wbcd_test <- wbcd_n[470:569, ]

#Storing class labels in factor vectors
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#training a model on the data
#Using KNN to clasify test data
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,                        
                      cl = wbcd_train_labels, k=21)
wbcd_test_pred
#Returns a factor vector of predicted labels for each of the examples in the test dataset


#evaluating model performance
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,             
           prop.chisq=FALSE)

#77 out of 100 values indicate cases where the mass was benign, and the kNN algorithm correctly identified it as such
#The bottom-right cell, indicates the true positive results, where the classifier and the clinically determined label agree that the mass is malignant.
#A total of 21 of 100 predictions were true positives.





