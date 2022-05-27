rm(list=ls())
train <- read.csv("withnameTrain2.csv")
test <- read.csv("withnameTest2.csv")

library(tidyverse)
library(caret)



train$Transported = as.factor(train$Transported)

#This removes variables with 0 or near 0 variance. Duts about 4/5 of the cols, and they are the least key cols. You can use this or the real big data set
trainwithvar <- train[-nearZeroVar(train)]

preProcValues <- preProcess(train, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, train)

trainTransformed$Transported = as.factor(trainTransformed$Transported)

set.seed(5)
vec <- createDataPartition(
  y=trainTransformed$Transported,
  p=.75,
  list=FALSE
)


sum(is.na(trainTransformed))
faketrain <- trainTransformed[vec,]
faketest <- trainTransformed[-vec,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  ## repeated ten times
  repeats = 3)

gbmFit1 <- train(Transported ~ ., data = faketrain, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = TRUE)


ggplot(gbmFit1)
plsProbs <- predict(gbmFit1, newdata = faketest, type = "prob")

eldata <- plsProbs %>% mutate(prediction = True>False)

eldata$cake = ifelse(eldata$prediction==TRUE, "True", "False")

eldata$cake <- as.factor(eldata$cake)

merged <- mutate(eldata, Transported = faketest$Transported)

y <- confusionMatrix(data = merged$cake, reference = merged$Transported)
y
fileConn<- file("outputs\\MPLoutput.txt")
writeLines(toString(y), fileConn)
close(fileConn)

save.image(file='Models\\MPLModel.RData')