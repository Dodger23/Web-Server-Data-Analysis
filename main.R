library(tidyverse) 
library(reshape2)  
library(stringr) 
library(lubridatae) 
library(colorspace) 
library(viridis)
library(hrbrthemes)
library(ggplot2)

#Reading in files
data = read.csv("Data/web_log_data.csv")

#summary of the data
summary(data)

## Find NA value
find_na = apply(is.na(data),2,sum)
print(find_na)


#Identical Column check
print(identical(data$session , data$user_id))


#Apply Date Format. Use day, month, year, and hour
timeline = dmy_h(substr(data$date_time , 1, 14) )


# Request value check
print(unique(data$request)[40:55] )

## Eliminate last of '/'
lastChar_substr = function(x){ 
    lastChar = str_sub(x,-1)
    if(lastChar=="/" & str_length(x) >1){
        str_sub(x,-1) = ""
    }
    return (x)
}

data$request = sapply(as.character(data$request), lastChar_substr,USE.NAMES = FALSE)


#Get preprcoessed data
data_timeline =  data %>%  mutate(timeline = timeline) %>% select(-c(date_time,session))














