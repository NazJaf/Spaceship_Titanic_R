install.packages("tidyverse")
install.packages("skimr")
install.packages("psych")
install.packages("dplyr")
install.packages("corrplot")

library(corrplot)
library(dplyr)
library(tidyverse)
library(skimr)
library(psych) 

#loading data
train <- read.csv("C:/Users/Zaur/Downloads/train.csv")
test <- read.csv("C:/Users/Zaur/Downloads/test.csv")

#Explore data
str(train)
summary(train)
head(train)
skim(train)
{
# Select only numeric columns
#numeric_columns_train <- train %>%
#  select_if(is.numeric)

#cor_matrix_train <- cor(numeric_columns_train, use = "complete.obs")
#print(cor_matrix_train)

#corrplot(cor_matrix_train, method = "color", type = "upper", 
 #        tl.col = "black", tl.srt = 45)
}


#data cleaning
colSums(is.na(train))
train$Age[is.na(train$Age)] <- median(train$Age, na.rm = TRUE)
train <- na.omit(train)

#Exploratory  Data Analysis(EDA)
describe(train)

#Visualize Distributions

ggplot(train, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = 'blue', color = 'white') +
  theme_minimal()


ggplot(train, aes(x = HomePlanet, fill = Transported)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(y = "Proportion", title = "Transported by HomePlanet")


ggplot(train, aes(x = CryoSleep, fill = Transported)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(y = "Proportion", title = "CryoSleep vs Transported")

#Create new features
#new column for total spend
train <- train %>%
  mutate(TotalSpend = RoomService + FoodCourt + ShoppingMall + Spa + VRDeck)

# binary column for high spenders
train <- train %>%
  mutate(HighSpender = ifelse(TotalSpend > median(TotalSpend, na.rm = TRUE), 1, 0))

#total spend
ggplot(train, aes(x = "", y = TotalSpend)) + 
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of Total Spend", y = "Total Spend")
#transported by vip status
ggplot(train, aes(x = VIP, fill = Transported)) + 
  geom_bar(position = "fill") +
  labs(title = "VIP vs. Transported", x = "VIP", y = "Proportion")


