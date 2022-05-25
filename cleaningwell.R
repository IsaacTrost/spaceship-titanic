rm(list=ls())
setwd("./spaceship-titanic/")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

library(tidyverse)
library(caret)
train1 <- train %>% separate(PassengerId, into=c("firstID", "secondID"), sep = "_") %>% 
  separate(Name, into = c("firstname", "lastname"), sep=" ") %>% 
  mutate(firstnamelength = nchar(firstname), lastnamelength = nchar(lastname)) %>% 
  mutate(hadRS = is.na(RoomService), hadFC = is.na(FoodCourt), hadSPA = is.na(Spa), hadMALL = is.na(ShoppingMall), hadVR = is.na(VRDeck)) %>% 
  mutate(RoomService = ifelse(is.na(RoomService), 0, RoomService), FoodCourt = ifelse(is.na(FoodCourt), 0, FoodCourt), Spa = ifelse(is.na(Spa), 0, Spa), ShoppingMall = ifelse(is.na(ShoppingMall), 0, ShoppingMall), VRDeck = ifelse(is.na(VRDeck), 0, VRDeck), Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age)) %>% 
  separate(Cabin, into = c("deck", "num", "side"), sep = "/") %>% 
  mutate(firstID = as.numeric(firstID), secondID = as.numeric(secondID), num = as.numeric(num)) %>% 
  mutate(is_homeless = ifelse(is.na(num), "True", "False")) %>% 
  mutate(is_homeless = as.factor(is_homeless), num = ifelse(is.na(num), -1, num), side = ifelse(is.na(side), "TrashCompactor", side)) %>% 
  mutate(lastnamelength = ifelse(is.na(lastnamelength), 0, lastnamelength))

summary(train1)

train2 <- train1 %>% select(-lastname, -firstname, -Transported)


dmy <- dummyVars(" ~ .", data = train2)
train3 <- data.frame(predict(dmy, newdata = train2))

train3$Transported = as.factor(train1$Transported)



test1 <- test %>% separate(PassengerId, into=c("firstID", "secondID"), sep = "_") %>% 
  separate(Name, into = c("firstname", "lastname"), sep=" ") %>% 
  mutate(firstnamelength = nchar(firstname), lastnamelength = nchar(lastname)) %>% 
  mutate(hadRS = is.na(RoomService), hadFC = is.na(FoodCourt), hadSPA = is.na(Spa), hadMALL = is.na(ShoppingMall), hadVR = is.na(VRDeck)) %>% 
  mutate(RoomService = ifelse(is.na(RoomService), 0, RoomService), FoodCourt = ifelse(is.na(FoodCourt), 0, FoodCourt), Spa = ifelse(is.na(Spa), 0, Spa), ShoppingMall = ifelse(is.na(ShoppingMall), 0, ShoppingMall), VRDeck = ifelse(is.na(VRDeck), 0, VRDeck), Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age)) %>% 
  separate(Cabin, into = c("deck", "num", "side"), sep = "/") %>% 
  mutate(firstID = as.numeric(firstID), secondID = as.numeric(secondID), num = as.numeric(num)) %>% 
  mutate(is_homeless = ifelse(is.na(num), "True", "False")) %>% 
  mutate(is_homeless = as.factor(is_homeless), num = ifelse(is.na(num), -1, num), side = ifelse(is.na(side), "TrashCompactor", side)) %>% 
  mutate(lastnamelength = ifelse(is.na(lastnamelength), 0, lastnamelength))


test2 <- test1 %>% select(-lastname, -firstname)

dmy <- dummyVars(" ~ .", data = test2)
test3 <- data.frame(predict(dmy, newdata = test2))

write.csv(train3,"nonameTrain.csv", row.names = FALSE)
write.csv(test3,"nonameTest.csv", row.names = FALSE)
