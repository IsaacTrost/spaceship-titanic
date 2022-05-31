rm(list=ls())
library(tidyverse)

#loads a old workspace to get  plots from it
load("Models\\MPLModel.RData")

ggplot(gbmFit1)
rm(list=ls())

#loads a old workspace to get  plots from it
load("Models\\multinomModel.RData")

ggplot(gbmFit1)
rm(list=ls())



data <- read.csv("ModelsCompiled.csv")

#CHanging the names to make them more readable
names(data)[4:7] <- c("GoodFalse", "BadFalse", "BadTrue", "GoodTrue")

#Converting it to a long tibble to make it plottable
df_long <- pivot_longer(data, GoodFalse:GoodTrue, names_to = "State", values_to = "Time")  

#Creates a stacked bar chart
barchart <- ggplot(df_long, aes(x=X, fill=State)) + geom_bar(stat = "identity", aes(y= Time)) + ylab("Number in catagory") + xlab("models used")

barchart

#Loading in the original training dataset
spacy <- read.csv("nonameTrain.csv")

#getting a histogram
hist(spacy$Age)

#Creating a jitterplot
agevtrans <- ggplot(spacy, aes(x=secondID, y=Age)) + geom_jitter(aes(color = Transported), width = .2) + xlab("group position")


agevtrans


