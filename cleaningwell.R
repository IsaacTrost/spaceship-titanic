rm(list=ls())
setwd("./spaceship-titanic/")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

library(tidyverse)
library(caret)
y = c("aaaaaaa aaaaaaaaaaa", "bbbbbbb bbbbbbbbbbb", "ccccccc ccccccccccc", "ddddddd ddddddddddd", "eeeeeee eeeeeeeeeee", "fffffff fffffffffff", "ggggggg ggggggggggg", "hhhhhhh hhhhhhhhhhh", "iiiiiii iiiiiiiiiii", "jjjjjjj jjjjjjjjjjj", "kkkkkkk kkkkkkkkkkk", "lllllll lllllllllll", "mmmmmmm mmmmmmmmmmm", "nnnnnnn nnnnnnnnnnn", "ooooooo ooooooooooo", "ppppppp ppppppppppp", "qqqqqqq qqqqqqqqqqq", "rrrrrrr rrrrrrrrrrr", "sssssss sssssssssss", "ttttttt ttttttttttt", "uuuuuuu uuuuuuuuuuu", "vvvvvvv vvvvvvvvvvv", "wwwwwww wwwwwwwwwww", "xxxxxxx xxxxxxxxxxx", "yyyyyyy yyyyyyyyyyy", "zzzzzzz zzzzzzzzzzz")
for (name in y){
  train[nrow(train) + 1,] <- train[nrow(train),]
  train$Name[nrow(train)] <- name
}
train1 <- train %>% separate(PassengerId, into=c("firstID", "secondID"), sep = "_") %>% 
  separate(Name, into = c("firstname", "lastname"), sep=" ") %>% 
  mutate(firstnamelength = nchar(firstname), lastnamelength = nchar(lastname)) %>% 
  mutate(hadRS = is.na(RoomService), hadFC = is.na(FoodCourt), hadSPA = is.na(Spa), hadMALL = is.na(ShoppingMall), hadVR = is.na(VRDeck)) %>% 
  mutate(RoomService = ifelse(is.na(RoomService), 0, RoomService), FoodCourt = ifelse(is.na(FoodCourt), 0, FoodCourt), Spa = ifelse(is.na(Spa), 0, Spa), ShoppingMall = ifelse(is.na(ShoppingMall), 0, ShoppingMall), VRDeck = ifelse(is.na(VRDeck), 0, VRDeck), Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age)) %>% 
  separate(Cabin, into = c("deck", "num", "side"), sep = "/") %>% 
  mutate(firstID = as.numeric(firstID), secondID = as.numeric(secondID), num = as.numeric(num)) %>% 
  mutate(is_homeless = ifelse(is.na(num), "True", "False")) %>% 
  mutate(is_homeless = as.factor(is_homeless), num = ifelse(is.na(num), -1, num), side = ifelse(is.na(side), "TrashCompactor", side)) %>% 
  mutate(lastnamelength = ifelse(is.na(lastnamelength), 0, lastnamelength)) %>% 
  group_by(firstID) %>% mutate(group_size = max(secondID)) %>% 
  separate(firstname, into=c("firstspace","first1", "first2", "first3", "first4", "first5", "first6", "first7"), sep = "") %>% 
  separate(lastname, into=c("lastspace", "last1", "last2", "last3", "last4", "last5", "last6", "last7", "last8", "last9", "last10", "last11"), sep = "") %>% 
  mutate(firstspace = NULL, lastspace = NULL) 
  

summary(train1)

train2 <- train1 %>% select(-Transported)


dmy <- dummyVars(" ~ .", data = train2)
train3 <- data.frame(predict(dmy, newdata = train2))

train3$Transported = as.factor(train1$Transported)


for (name in y){
  test[nrow(train) + 1,] <- test[nrow(train),]
  test$Name[nrow(train)] <- name
}

test1 <- test %>% separate(PassengerId, into=c("firstID", "secondID"), sep = "_") %>% 
  separate(Name, into = c("firstname", "lastname"), sep=" ") %>% 
  mutate(firstnamelength = nchar(firstname), lastnamelength = nchar(lastname)) %>% 
  mutate(hadRS = is.na(RoomService), hadFC = is.na(FoodCourt), hadSPA = is.na(Spa), hadMALL = is.na(ShoppingMall), hadVR = is.na(VRDeck)) %>% 
  mutate(RoomService = ifelse(is.na(RoomService), 0, RoomService), FoodCourt = ifelse(is.na(FoodCourt), 0, FoodCourt), Spa = ifelse(is.na(Spa), 0, Spa), ShoppingMall = ifelse(is.na(ShoppingMall), 0, ShoppingMall), VRDeck = ifelse(is.na(VRDeck), 0, VRDeck), Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age)) %>% 
  separate(Cabin, into = c("deck", "num", "side"), sep = "/") %>% 
  mutate(firstID = as.numeric(firstID), secondID = as.numeric(secondID), num = as.numeric(num)) %>% 
  mutate(is_homeless = ifelse(is.na(num), "True", "False")) %>% 
  mutate(is_homeless = as.factor(is_homeless), num = ifelse(is.na(num), -1, num), side = ifelse(is.na(side), "TrashCompactor", side)) %>% 
  mutate(lastnamelength = ifelse(is.na(lastnamelength), 0, lastnamelength)) %>% 
  group_by(firstID) %>% mutate(group_size = max(secondID)) %>% 
  separate(firstname, into=c("firstspace","first1", "first2", "first3", "first4", "first5", "first6", "first7"), sep = "") %>% 
  separate(lastname, into=c("lastspace", "last1", "last2", "last3", "last4", "last5", "last6", "last7", "last8", "last9", "last10", "last11"), sep = "") %>% 
  mutate(firstspace = NULL, lastspace = NULL)



test2 <- test1 

dmy <- dummyVars(" ~ .", data = test2)
test3 <- data.frame(predict(dmy, newdata = test2))

write.csv(train3,"withnameTrain2.csv", row.names = FALSE)
write.csv(test3,"withnameTest2.csv", row.names = FALSE)
