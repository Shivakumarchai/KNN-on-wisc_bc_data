# knn algorithm 
install.packages("class")
library("class")
install.packages("gmodels")
library("gmodels")
install.packages("caret")
library("caret")
install.packages("vcd")
library("vcd")

wbcd<-read.csv("E:/wisc_bc_data.csv")
dim(wbcd)
wbcd <- wbcd[-1]
str(wbcd)

wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c(0, 1))
ftable(wbcd$diagnosis)

round(prop.table(ftable(wbcd$diagnosis)) * 100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# defining a function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

#    Data preparation – creating training and test data sets

wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# training the data

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

class(wbcd_test_pred)

# Evaluating Model Performance

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

# Improving Model Performance

wbcd_z <- as.data.frame(scale(wbcd[-1]))

summary(wbcd_z$area_mean)

wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)


confusionMatrix(wbcd_test_labels, wbcd_test_pred, positive = "0")


Kappa(ftable(wbcd_test_labels, wbcd_test_pred))

sensitivity(wbcd_test_pred, wbcd_test_labels,
            positive = "1")
specificity(wbcd_test_pred, wbcd_test_labels,
            negative = "0")
posPredValue(wbcd_test_pred, wbcd_test_labels,
             positive = "1")


# -----------Conclusion---------------
# In this post we learned about classification using k-nearest neighbors. Unlike many classification algorithms, kNN does not do any learning.

# This algorithm simply stores the training data verbatim. Unlabeled test examples are then matched to the most similar records in the training set using a distance function, and the unlabeled example is assigned the label of its neighbors.

# Even the KNN algorithm is classified as a simple algorithm, it is capable of tackling complex tasks.

# They do not need any mathematics assumptions and you do not need the most updated and strong specifications in terms of hardware.



