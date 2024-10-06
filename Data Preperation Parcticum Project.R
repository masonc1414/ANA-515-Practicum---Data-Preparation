#Title: ANA 515 Practicum, Data Preparation
#Author: Mason Collins
#Date Last Edited: 2024-10-06

# load packages that I think will be needed while cleaning the data and producing including visuals
library(tidyverse)
library(knitr)
library(bslib)
library(knitr)
library(plotly)
library(zoo)
library(dplyr)


# loading the data into r

GTResults1718 <- read.csv("~/Desktop/G&T Results 2017-18.csv")
View(GTResults1718)

GTResults1819 <- read.csv("~/Desktop/G&T Results 2018-19.csv")
View(GTResults1819)

# combining the 2 datasets into 1 

#We need to remove that last 2 columns of the 2018-2019 dataset as there is no description of what information
# this column holds and because these two column do not appear in the 2017-2018 dataset.

GTResults1819$X <- NULL
GTResults1819$X.1 <- NULL

GTResults <- rbind(GTResults1718,GTResults1819)

#Clean the Data

# No need to assign better column names as all are adequate and descriptive

# I removed the School.Assigned column because removing all of the rows that were empty would severely decrease the dataset size. 

GTResults$School.Assigned <- NULL

# I removed the School.Preference column and the Will.you.enroll.there column as most of the rows were empty and the rows that has response
# had no pattern and were mostly sentences which would not be useful during analysis. 

GTResults$School.Preferences <- NULL
GTResults$Will.you.enroll.there. <- NULL

#Convert months in numeric format and abbreviated format to month names 

GTResults$Birth.Month[GTResults$Birth.Month == '8'] <- 'August'
GTResults$Birth.Month[GTResults$Birth.Month == '11'] <- 'November'
GTResults$Birth.Month[GTResults$Birth.Month == '2'] <- 'February'
GTResults$Birth.Month[GTResults$Birth.Month == '12'] <- 'December'
GTResults$Birth.Month[GTResults$Birth.Month == 'Feb'] <- 'February'

# clean grade level column by assinging correct values

GTResults$Entering.Grade.Level[GTResults$Entering.Grade.Level == 'first'] <- '1'
GTResults$Entering.Grade.Level[GTResults$Entering.Grade.Level == 'kindergarten'] <- 'K'
GTResults$Entering.Grade.Level[GTResults$Entering.Grade.Level == 'Kindergarten'] <- 'K'
GTResults$Entering.Grade.Level[GTResults$Entering.Grade.Level == 'second'] <- '2'
GTResults$Entering.Grade.Level[GTResults$Entering.Grade.Level == 'kinder'] <- 'K'
GTResults$Entering.Grade.Level[GTResults$Entering.Grade.Level == 'Kinder'] <- 'K'

# clean OSLAT.Verbal.Percentile column by removing "%".

GTResults$OLSAT.Verbal.Percentile[GTResults$OLSAT.Verbal.Percentile == '91%'] <- '91'

# clean OSLAT.Verbal.Score column by converting fractions and percentages to correct format. 

GTResults$OLSAT.Verbal.Score[GTResults$OLSAT.Verbal.Score == '28/30'] <- '28'
GTResults$OLSAT.Verbal.Score[GTResults$OLSAT.Verbal.Score == '97%'] <- '29'
GTResults$OLSAT.Verbal.Score[GTResults$OLSAT.Verbal.Score == '24/30'] <- '24'
GTResults$OLSAT.Verbal.Score[GTResults$OLSAT.Verbal.Score == '23/30'] <- '23'
GTResults$OLSAT.Verbal.Score[GTResults$OLSAT.Verbal.Score == '17/30'] <- '17'
GTResults$OLSAT.Verbal.Score[GTResults$OLSAT.Verbal.Score == '24/30'] <- '24'
GTResults$OLSAT.Verbal.Score[GTResults$OLSAT.Verbal.Score == '19/30'] <- '19'
GTResults$OLSAT.Verbal.Score[GTResults$OLSAT.Verbal.Score == '97'] <- '29'
GTResults$OLSAT.Verbal.Score[GTResults$OLSAT.Verbal.Score == '25/30'] <- '25'
GTResults$OLSAT.Verbal.Score[GTResults$OLSAT.Verbal.Score == '83%'] <- '25'
GTResults$OLSAT.Verbal.Score[GTResults$OLSAT.Verbal.Score == '100%'] <- '30'

# clean NNAT.Non.Verbal.Raw.Score column by converting fractions to correct format.

GTResults$NNAT.Non.Verbal.Raw.Score[GTResults$NNAT.Non.Verbal.Raw.Score == '45/50'] <- '45'
GTResults$NNAT.Non.Verbal.Raw.Score[GTResults$NNAT.Non.Verbal.Raw.Score == '36/48'] <- '36'
GTResults$NNAT.Non.Verbal.Raw.Score[GTResults$NNAT.Non.Verbal.Raw.Score == '40/48'] <- '40'
GTResults$NNAT.Non.Verbal.Raw.Score[GTResults$NNAT.Non.Verbal.Raw.Score == '39/48'] <- '39'
GTResults$NNAT.Non.Verbal.Raw.Score[GTResults$NNAT.Non.Verbal.Raw.Score == '41/48'] <- '41'
GTResults$NNAT.Non.Verbal.Raw.Score[GTResults$NNAT.Non.Verbal.Raw.Score == '40/50'] <- '40'


# clean NNAT.Non.Verbal.Raw.Percentile column by converting percentages to correct format.

GTResults$NNAT.Non.Verbal.Percentile[GTResults$NNAT.Non.Verbal.Percentile == '99%'] <- '99'
GTResults$NNAT.Non.Verbal.Percentile[GTResults$NNAT.Non.Verbal.Percentile == '99%'] <- '99'
GTResults$NNAT.Non.Verbal.Percentile[GTResults$NNAT.Non.Verbal.Percentile == '71%'] <- '71'
GTResults$NNAT.Non.Verbal.Percentile[GTResults$NNAT.Non.Verbal.Percentile == '98%'] <- '98'


# Correct Timestamp Column 

GTResults$Timestamp[GTResults$Timestamp == '7-Apr'] <- '4/7/17' 

GTResults$Timestamp<-gsub(" .*","",as.character(GTResults$Timestamp)) # removed everything right of the date as the time taken was only
# included in about half of the rows.


# Replace missing values with NA

GTResults$District<- na_if(GTResults$District, "")
GTResults$OLSAT.Verbal.Score<- na_if(GTResults$OLSAT.Verbal.Score, "")
GTResults$Timestamp<- na_if(GTResults$Timestamp, "")
GTResults$NNAT.Non.Verbal.Raw.Score<- na_if(GTResults$NNAT.Non.Verbal.Raw.Score, "")
GTResults$Birth.Month<- na_if(GTResults$Birth.Month, "")

GTResults$District<- na_if(GTResults$District, "Anderson") # don't know what district number Anderson is so make it NA

GTResults$OLSAT.Verbal.Score<- na_if(GTResults$OLSAT.Verbal.Score, "")

#Remove columns with incorrect answers and unusable data.

GTResults <- GTResults[-c(85,105,106,109),]

# 3 of the columns in row 85 have unusable data. 2 of the columns in row 105, 106, and 109 have unusable data.
# row 27 and 47 have imput errors


# remove all rows with NAs

GTResults <- GTResults %>% drop_na()


# Remove outliers after identifying them through box plots

GTResults$OLSAT.Verbal.Score <- as.numeric(GTResults$OLSAT.Verbal.Score)
class(GTResults$OLSAT.Verbal.Score)
boxplot(GTResults$OLSAT.Verbal.Score)

GTResults$OLSAT.Verbal.Percentile <- as.numeric(GTResults$OLSAT.Verbal.Percentile)
class(GTResults$OLSAT.Verbal.Percentile)
boxplot(GTResults$OLSAT.Verbal.Percentile)

GTResults$NNAT.Non.Verbal.Raw.Score <- as.numeric(GTResults$NNAT.Non.Verbal.Raw.Score)
class(GTResults$NNAT.Non.Verbal.Raw.Score)
boxplot(GTResults$NNAT.Non.Verbal.Raw.Score)

GTResults$NNAT.Non.Verbal.Percentile <- as.numeric(GTResults$NNAT.Non.Verbal.Percentile)
class(GTResults$NNAT.Non.Verbal.Percentile)
boxplot(GTResults$NNAT.Non.Verbal.Percentile)

GTResults$Overall.Score <- as.numeric(GTResults$Overall.Score)
class(GTResults$Overall.Score)
boxplot(GTResults$Overall.Score)

detect_outlier <- function(x) { # create detect outlier function
  
  Quantile1 <- quantile(x, probs=.25) # calculate first quantile
  Quantile3 <- quantile(x, probs=.75) # calculate third quantile
  IQR = Quantile3 - Quantile1 # calculate inter quartile range
  x > Quantile3 + (IQR * 1.5) | x < Quantile1 - (IQR * 1.5)  # return true or false
}
remove_outlier <- function(dataframe, columns = names(dataframe)) { # create remove outlier function
  for (col in columns) {
    
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  print(dataframe)
}

remove_outlier(GTResults, c('OLSAT.Verbal.Score', 'OLSAT.Verbal.Percentile', 'NNAT.Non.Verbal.Raw.Score', 'NNAT.Non.Verbal.Percentile', 'Overall.Score'))

GTResults <- GTResults[-c(27,47),]

# Visuals 

ggplot(data = GTResults) + geom_point(mapping = aes(x = District,y = Overall.Score, color = District)) + ggtitle("Distribution of Overall Score for Each District")

hist(GTResults$Overall.Score, main = "Histogram of Overall Score", xlab = "Overall Score", ylab = "Number of Students", col = "green")

