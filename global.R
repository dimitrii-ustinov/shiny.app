library(ggrepel)
library(RColorBrewer)
library(shinycssloaders)
library(shiny)
library(shinyjs)
library(vcd)
library(GGally)
library(ggplot2)
library(dplyr)
library(corrgram)
library(seriation)
library(visdat)
library(DT)
library(car)
library(scales)
library(plotly)
library(gplots)
library(naniar)
library(rpart)
library(rpart.plot)
library(recipes)
library(tidyverse)
library(caret)
library(glmnet)
library(stringr)

#New

#Load data
#setwd("P:\\Downloads\\DATA423\\Assignment 2\\Assignment2")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
original_data  <- read.csv("Ass2Data.csv",na.strings = c("NA","N/A"), header = TRUE, stringsAsFactors = TRUE)



#_________________Data preprocessing___________
#Select factors
choicesA <- colnames(as.data.frame(original_data))[c(2,12,15)]
choicesA
factor_data <-  original_data[, choicesA, drop = FALSE]
num_data <- original_data %>% dplyr::select(-c(choicesA,CODE))


#For boxplot

criterion <- 1.5

#_________________Data Cleaning___________


# Standardize the missing values
original_data[original_data == -99] <- NA
original_data$CODE <- as.character(original_data$CODE)


#Missing proportion function
missing_proportion <- function(x){ sum(is.na(x))/length(x)*100 }

