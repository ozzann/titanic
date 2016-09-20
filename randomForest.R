
# This script trains a Random Forest model based on the data,
# saves a sample submission, and plots the relative importance
# of the variables in making predictions

# Download 1_random_forest_r_submission.csv from the output below
# and submit it through https://www.kaggle.com/c/titanic-gettingStarted/submissions/attach
# to enter this getting started competition!

library(ggplot2)
library(randomForest)

set.seed(1)
sourceSet <- read.csv("train.csv", stringsAsFactors=FALSE)

isChild <- function(x){
  f <- 1
  if( is.na(x) || x >= 18) f <- 0
  return(f)
}

defaultTitle <- function(x){
  res <- "Mrs."
  if( x == "male") res <- "Mr."
  return(res) 
}

sourceSet$Title <- sub("(.*), ([^\\.]*)\\.(.*)", "\\2", sourceSet$Name, perl=TRUE)
sourceSet$Child <- sapply(sourceSet$Age, isChild)
sourceSet$NameLength <- sapply(sourceSet$Name, nchar)
sourceSet$Title[is.na(sourceSet$Title)] <- sapply(sourceSet$Sex, defaultTitle)
sourceSet$Title[is.nan(sourceSet$Title)] <- sapply(sourceSet$Sex, defaultTitle)
sourceSet$Title <- factor(sourceSet$Title)

#index <- sample(1:nrow(sourceSet),round(0.7*nrow(sourceSet)))
train <- sourceSet; #[index,]
test  <- sourceSet; #[-index,]
#test  <- read.csv("test.csv",  stringsAsFactors=FALSE)

extractFeatures <- function(data) {
    features <- c("Pclass",
                  "Age",
                  "Sex",
                  "Parch",
                  "SibSp",
                  "Fare",
                  "Embarked",
                  "Title",
                  "Child",
                  "NameLength")
    fea <- data[,features]
    fea$Age[is.na(fea$Age)] <- -1
    fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
    fea$Embarked[fea$Embarked==""] = "S"
    fea$Sex      <- as.factor(fea$Sex)
    fea$Embarked <- as.factor(fea$Embarked)
    fea$Child <- as.factor(fea$Child)
    return(fea)
}

rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree=50, importance=TRUE)


submission <- data.frame(PassengerId = test$PassengerId)
submission$Predicted <- predict(rf, extractFeatures(test))
submission$Survived <- test$Survived
# write.csv(submission, file = "1_random_forest_r_submission.csv", row.names=FALSE)


test.TP <- sum((submission$Predicted == "1") & (submission$Survived == T))
test.FP <- sum((submission$Predicted == "1") & (submission$Survived == F))
test.TN <- sum((submission$Predicted == "0") & (submission$Survived == F))
test.FN <- sum((submission$Predicted == "0") & (submission$Survived == T))


 imp <- importance(rf, type=1)
 featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

 ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
     geom_bar(stat="identity", fill="#53cfff") +
     coord_flip() +
     theme_light(base_size=20) +
     xlab("") +
     ylab("Importance") +
     ggtitle("Random Forest Feature Importance\n") +
     theme(plot.title=element_text(size=18))
# 
# ggsave("2_feature_importance.png", p)
cat("precision: ", test.TP/(test.TP + test.FP), "\n")
cat("recall: ", test.TP/(test.TP + test.FN), "\n")


