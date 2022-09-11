#SHLOK KAMLESH GADIYA 
#ALY6080 - Integrated Experiential Learning
#Q1: Do merchants adjust their SKU purchases over time and if so, what are the different ways they adjust their purchases?

#1 Import libraries.

library(FSA)
library(FSAdata)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(corrplot)
library(scatterPlotMatrix)
library(Amelia)
library(readxl)
library(car)
library(leaps)
library(gginference)
library(UpSetR)
library(naniar)
library(lubridate)
library(Hmisc)

#2 Import the dataset and describe it.

awantunai<-read.csv("20220413_Northeastern_AwanTunai_Capstone_Data.csv")
dim(awantunai)
str(awantunai)
summary(awantunai)

#3 Find any duplicate values and discard them.

awantunai_duplicate <- awantunai[!duplicated(awantunai), ]

#4 Find the NA values and discard them.

missmap(awantunai, main = "Missing values in dataset")
sapply(awantunai,function(x) sum(is.na(x)))
gg_miss_upset(awantunai)
awantunai_clean <- awantunai[complete.cases(awantunai), ]

#5 Break the date column into the day, month, and year.

awantunai_updated <- awantunai_clean %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         day = as.integer(strftime(date, format = "%d")),
         month = as.integer(strftime(date, format = "%m")),
         year = as.integer(strftime(date, format = "%Y")))
head(awantunai_updated,5)
summary(awantunai_updated)

#6 Now we will sort the data in descending order from the price column.

awantunai_sorted <- awantunai_updated[order(-awantunai_updated$price),]

#7 Here we will create the mode function to calculate the mode for the column month.  

mode<-function(x){which.max(tabulate(x))}
mode(awantunai_sorted$month)

#8 Visualization

hist.data.frame(awantunai_sorted)
hist(awantunai_sorted$day)
