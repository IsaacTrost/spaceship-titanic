rm(list=ls())
train <- read.csv("nonameTrain.csv")
test <- read.csv("nonameTest.csv")

library(tidyverse)
library(caret)


train$Transported = as.factor(train$Transported)
summary(train)

#This is a problem we want to come back to later. These wont have much of an effect on the result
train <- train %>% select(-deck, -sideTrashCompactor)
#finding corralations, there are none
descrCor <-  cor(select(train, -Transported))
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)

preProcValues <- preProcess(train, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, train)

set.seed(1623)
vec <- createDataPartition(
  y=trainTransformed$Transported,
  p=.75,
  list=FALSE
)

faketrain <- trainTransformed[vec,]
faketest <- trainTransformed[-vec,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

gbmFit1 <- train(Transported ~ ., data = faketrain, 
                 method = "nnet", 
                 trControl = fitControl,
                 tunelength = 20
              )



plsProbs <- predict(gbmFit1, newdata = faketest, type = "prob")

eldata <- plsProbs %>% mutate(prediction = True>False)

eldata$cake = ifelse(eldata$prediction==TRUE, "True", "False")

eldata$cake <- as.factor(eldata$cake)

merged <- mutate(eldata, Transported = faketest$Transported)

y <- confusionMatrix(data = merged$cake, reference = merged$Transported)
y
fileConn<- file("outputs\\nnetoutput.txt")
writeLines(toString(y), fileConn)
close(fileConn)

save.image(file='Models\\MPLModel.RData')