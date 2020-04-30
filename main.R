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




## Period of Data
print(paste("Start Date :", min(data_timeline$timeline)))
print(paste("Last Date :", max(data_timeline$timeline)))



## User number analysis
session_n = data_timeline %>% group_by(user_id) %>% summarise(n=n())
print((paste("Total number of users :", nrow(session_n))))


## Visualize user number by total session
session_n %>%
    arrange(desc(n)) %>%
    ggplot(aes(n)) +
    geom_histogram(stat = "bin", binwidth=2 , fill = "#ffd62d" , col = "#ffd62d")+
    labs(title = "Number of User Count",x = "Total Session",y = "Count")+
    theme_ft_rc()+
    theme(plot.title = element_text(hjust = 0.5),legend.position = "None")



# Step number analysis
step_n <- data_timeline %>% group_by(step) %>% summarise(n=n())

## Visualize Step number
step_n %>%
    arrange(desc(n)) %>%
    ggplot(aes(x=step,y=n)) +
    geom_step(size=1.2, color="#ffd62d")+
    geom_point(size=1.5, color="#FFFFFF")+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
    coord_cartesian(xlim = c(1, 20))+
    theme_ft_rc()+
    labs(title = "Number of Step Count",x = "Step",y = "Count")+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "None")




## Aggregated by Day
day_n <- data %>% 
    group_by(day = floor_date(data_timeline$timeline, "day")) %>% 
    summarise(n_person=n_distinct(user_id),n_session = n())

## Visualize trend of user and session by day
melt(day_n,id.var = "day")  %>%
    ggplot(aes(x=as.Date(day),y=value)) +
    geom_line(size=1,color="#ffd62d") + 
    geom_smooth(method= 'loess',se = FALSE,color = "steelblue") +
    labs(title = "The Trend of Web site",x = "Date",y = "Count")+
    theme_ft_rc()




## Aggregated by hour
dat_hour <- data_timeline %>% group_by(hour = hour(timeline)) %>% summarise(n=n())

## Visualize hour distribution of Session 
dat_hour %>%
    arrange(desc(n)) %>%
    ggplot(aes(x=hour,y=n,fill=cut(hour,4))) +
    geom_bar(stat="identity") + 
    scale_fill_discrete_qualitative(palette = "Set 2")+
    theme_ft_rc() +
    labs(title = "The Hour Distribution of Session",x = "Hour",y = "Count")+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "None")



## Aggregate dy IP
ip_n <- data_timeline %>% group_by(ip) %>% summarise(n=n())

top_n(ip_n, n=20, n) %>%
    arrange(desc(n)) %>%
    ggplot(aes(x=reorder(ip, n),y=n )) +
    geom_segment( aes(x=reorder(ip, n), xend=reorder(ip, n), y=0, yend=n), color="#fbde0e") +
    geom_point( color="#ffac2d", size=4) +
    coord_flip() +
    theme_ft_rc()+
    labs(title = "Number of IP Count",x = "IP",y = "Count")+
    theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
    )



##Aggregate by Request
request_n <- data_timeline %>% group_by(request) %>% 
    filter(request!="/") %>% #exclude main page 
    summarise(n=n())

# Horizontal version
top_n(request_n, n=20, n) %>%
    arrange(desc(n)) %>%
    ggplot(aes(x=reorder(request, n),y=n)) +
    geom_segment( aes(x=reorder(request, n), xend=reorder(request, n), y=0, yend=n), color="#fb0e0e") +
    geom_point( color="red", size=4) +
    labs(title = "Number of Request Count",x = "Request",y = "Count")+
    coord_flip() +
    theme_ft_rc()+
    theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
    )














