# load libraries
library(tidyverse)

# load data set
df<- read.csv("isolates.csv")

#=============== Collection.date =============
## Filtering the data to keep samples on or after the earliest create date (2010-05-12)
## Create two new features from collection date and create date

## New features:
## 1. collection.yr: year of collection
## 2. collection.mon: month of collection
##        if year(collection)=year(create) and month(collection) is missing
##            then impute collection month from create month
#==============================================

# get the earliest create date
min.create<- as.Date(min(df$Create.date))

# Apply filter and impute collection month
df<- df %>% mutate(
  # collection date, extract year and month
  collection.yr=substr(Collection.date, 1, 4),
  collection.mon=if_else(nchar(Collection.date)%in%c(7,10), substr(Collection.date, 6, 7), NA_character_),
  # combine collection year and month, this variable will be used in filtering
  collect=case_when(
    # if collection date is fully provided
    nchar(Collection.date)==10 ~ as.Date(Collection.date),
    # if collection year and month are provided, fix day as the 1st day of month
    nchar(Collection.date)==7 ~ as.Date(paste(collection.yr,collection.mon,1,sep="-"), "%Y-%m-%d"),
    # if only collection year is provided, month need to be imputed
    nchar(Collection.date)==4 ~ as.Date(paste(collection.yr,1,1,sep="-"), "%Y-%m-%d"))) %>%
  # apply filter, keeping only collection date after 2010-05-12 
  filter(collect>=min.create) %>% 
  # impute missing collection month from create month if year(collection)==year(create)
  mutate(collection.mon=if_else( 
    substr(Create.date,1,4)==collection.yr & is.na(collection.mon), 
    substr(Create.date,6,7), collection.mon)) %>%
  # remove intermediate variable
  select(-collect)


## double check
View(df%>%select(Create.date,Collection.date,collection.yr,collection.mon))
# collection month missing or month and day both missing
View(df%>%select(Create.date,
                  Collection.date,collection.yr,collection.mon)%>%
       filter(nchar(Collection.date) %in% c(7,4)))



