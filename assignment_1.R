library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways


####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('data',full.names=T)


#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')


ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')

# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))

##### Question 1 #####
  #1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

## My code here
full_wide <- spread(full_long, key='data', value='value') %>%
  filter_if(is.numeric,all_vars(!is.na(.))) %>%
  mutate(month=month(DateTime),
         year=year(DateTime))

summer_wide <- filter(full_wide, month %in% c('6','7','8','9'))
ggplot(summer_wide,aes(x=ndvi,y=ndmi,color=site)) +
  geom_point() +
  theme_few() +
  scale_color_few() +
  theme(legend.position = c(0.8,0.8))
       
## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 


## My code here
#Creating an average ndsi
avg_ndsi <- full_wide %>%
  filter(month %in% c(1,2,3,4)) %>%
  group_by(site,year) %>%
  summarize(mean_NDSI=mean(ndsi))

#creating an average ndvi
avg_ndvi <- full_wide %>%
  filter(month %in% c(6,7,8)) %>%
  group_by(site,year) %>%
  summarize(mean_NDVI=mean(ndvi))

#combining avg ndvi and ndsi
avg_ndvi_ndsi <- avg_ndsi %>%
  inner_join(avg_ndvi, by=c('site','year'))

#plot of avg ndvi vs ndsi, by site
ggplot(avg_ndvi_ndsi,aes(x=mean_NDVI,y=mean_NDSI,color=site)) +
  geom_point() +
  theme_few() +
  scale_color_few() +
  theme(legend.position = c(.15,.85))

## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

## Your code here
#creating a new column to show if it is pre- or post-
avg_ndvi_ndsi$pre_post <- if_else(avg_ndvi_ndsi$year >= 2002, "post-burn", "pre-burn")

#Graph of pre- vs post-burn, and burned/unburned.
ggplot(avg_ndvi_ndsi,aes(x=mean_NDVI,y=mean_NDSI,color=site)) +
  geom_point() +
  theme_few() +
  scale_color_few() +
  theme(legend.position = c(.15,.85)) +
  facet_wrap(~pre_post)
## End code for question 3

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 

#Taking the mean NDVI per month,  per year. Then breaking it up pre- vs post-burn.
ndvi_avg <- full_wide %>%
  group_by(site,month,year) %>%
  summarize(mean_NDVI=mean(ndvi)) %>%
  mutate(pre_post = if_else(year >= 2002, "post-burn","pre-burn")) %>%
  group_by(site,month,pre_post) %>%
  summarize(mean_ndvi=mean(mean_NDVI))

#Plot of pre- vs post- burn of monthly avg NDVI
ggplot(ndvi_avg,aes(x=month,y=mean_ndvi,color=site))+
  geom_point() +
  theme_few() +
  scale_color_few() +
  theme(legend.position=c(.8,.2)) +
  facet_wrap(~pre_post) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
##### Question 5 ####
#What month is the snowiest on average?

#pre and post ndsi per month.
ndsi_avg <- full_wide %>%
  group_by(site,month,year) %>%
  summarize(mean_NDSI=mean(ndsi)) %>%
  mutate(pre_post = if_else(year >= 2002, "post-burn","pre-burn")) %>%
  group_by(site,month,pre_post) %>%
  summarize(mean_ndsi=mean(mean_NDSI))

ggplot(ndsi_avg,aes(x=month,y=mean_ndsi,color=site))+
  geom_point() +
  theme_few() +
  scale_color_few() +
  theme(legend.position=c(.8,.2)) +
  facet_wrap(~pre_post) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
