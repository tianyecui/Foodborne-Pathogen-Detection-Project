#Load Libraries
library(RecordLinkage)
library(lubridate)
library(tidyverse)
library(stringdist)
library(forcats)

#Load Dataset
###########Imputed Dataset
isolates <- read.csv("/Users/garyzhou/Downloads/isolates_filtered_collect_date_imputed.csv")

#Explore Isolation Source
str(isolates)
length(unique(isolates$Isolation.source))

#Sort by Freq of Isolation Source
freq_isol <- isolates %>% group_by(Isolation.source) %>% summarise(freq = n()) %>% arrange(desc(freq))
head(as.data.frame(freq_isol), n=30)

#Find string matches using grepl
unique_isol_source <- unique(isolates$Isolation.source)
test_iso <- isolates$Isolation.source

#Cases with just Beef and no Chicken or Pork
beef_new <- grep("beef", unique_isol_source, ignore.case=TRUE, value=TRUE)
beef_no_chick_pork <- c(grep("chicken", beef_new, ignore.case=TRUE, value=TRUE), grep("pork", beef_new, ignore.case=TRUE, value=TRUE))
beef_final <- beef_new[!beef_new %in% beef_no_chick_pork]

#Cases with just Chicken and no Beef or Pork
chicken_new <- grep("chicken", unique_isol_source, ignore.case=TRUE, value=TRUE)
chicken_no_beef_pork <- c(grep("beef", chicken_new, ignore.case=TRUE, value=TRUE), grep("pork", chicken_new, ignore.case=TRUE, value=TRUE))
chicken_final <- chicken_new[!chicken_new %in% chicken_no_beef_pork]

#Soil
soil_new <- grep("soil", unique_isol_source, ignore.case=TRUE, value=TRUE)[-1]

#Pork with no chicken or beef
pork <- grep("pork", unique_isol_source, ignore.case=TRUE, value=TRUE)
pork_no_chick_beef <- unique(c(grep("chicken", pork, ignore.case=TRUE, value=TRUE), grep("beef", pork, ignore.case=TRUE, value=TRUE)))
pork_final <- pork[!pork %in% pork_no_chick_beef]

#Egg
grep("egg", unique_isol_source, ignore.case=TRUE, value=TRUE)

#Change all NA to "missing"
test_iso[is.na(test_iso)] <- "missing"
testt_iso <- fct_collapse(test_iso, Missing = c("", "missing", "not provided", "not collected", "Not available", "other"),
                          Food = c("food", "Food products"),
                          Blood_Source = c("blood", "Blood, NOS"),
                          Food_Producing_Environment = c("food producing environment surface", "food producing environment", "Food processing environment", "food processing environment", "Food and food processing environment"),
                          Environ_Seafood_Processing_or_Equipment = c("Environmental - Seafood processing environment", "Environmental - Seafood Equipment"),
                          Environment = c("environment", "Environment", "Environ", "environmental", "environmental swab", "swab"),
                          Sponge = c("sponge", "environmental sponge", "environmental swab sponge"),
                          Clinical = c("clinical", "clinical isolate", "clinical sample", "clinical/host-associated"),
                          Milk = c("milk", "Milk filter", "raw milk", "milk filter"),
                          Meat = c("retail meat", "meat"),
                          Human_Source = c("human", "human listeriosis", "patient"),
                          RTE_Food = c("ready to eat food", "RTE Product"),
                          Ice_Cream = grep("ice cream", unique_isol_source, ignore.case=TRUE, value=TRUE),
                          Avocado_or_Guac = c(grep("avocado", unique_isol_source, ignore.case=TRUE, value=TRUE), "guacamole"),
                          Cheese = grep("cheese", unique_isol_source, ignore.case=TRUE, value=TRUE),
                          Beef = beef_final,
                          Soil = soil_new,
                          Pork = c(pork_final, "ham"),
                          Salmon = c("smoked salmon", "salmon", "Smoked salmon"),
                          Peach = c("peach", "white peach"),
                          Salami_Source = c("Salami", "salami paste production"),
                          Deli_Source = c("retail deli", "drain-deli", "deli meat"),
                          Cow_Feces = c("feces cow", "Cow feces"),
                          CSF = c(grep("CSF", unique_isol_source, ignore.case=TRUE, value=TRUE), "cerebrospinal fluid", "Cerebral spinal fluid"), #CSF and blood were grouped as CSF
                          Chicken = chicken_final)

#Look at unique isolation sources in descending order
head(sort(table(testt_iso), decreasing=TRUE), n=15)
sum(head(sort(table(testt_iso), decreasing=TRUE), n=15)) #75% of data in first 15 levels

#Put column back in dataset
isolates$Isolation_Source_Updated <- testt_iso

###Time Plot by Number of Isolates###
#Filter out missing data and get top 15 isolation sources
isol_names <- names(head(sort(table(testt_iso), decreasing=TRUE), n=16))
top_15 <- isol_names[!(isol_names == "Missing")]
length(testt_iso[testt_iso %in% top_15])/length(testt_iso)

# Create a date column using imputed month:
isolates <- isolates %>% 
  mutate(date.impute = paste0(paste(isolates$collection.yr, isolates$collection.mon, sep = '-'), '-01'),
         date.impute = as.Date(date.impute))


#Top 7 Sources
isol_names <- names(head(sort(table(testt_iso), decreasing=TRUE), n=8))
top_7 <- isol_names[!(isol_names == "Missing")]

#Create dataframe for top 7 isolation sources
df_timeseries <- as.data.frame(isolates %>% group_by(collection.mon, Isolation_Source_Updated) %>% summarise(N_Isolate = n()))
df_ts <- df_timeseries %>% filter(Isolation_Source_Updated %in% top_7)


#Time Plot by Isolation Source
#Plot by Collection Month
ggplot(df_ts, aes(x = collection.mon, y = N_Isolate)) + 
  geom_line(aes(color = Isolation_Source_Updated)) +
  theme_minimal() + 
  # Change x axis title
  labs(x = "Month") +
  # Set x breaks and the desired format for the date labels
  #scale_x_date(date_breaks = "1 month", date_labels = "%m%Y")+
  ylim(0, 50)




########## ########## ########## ########## ########## ########## ########## 
#Misc
isolates$Collect_Date <- as.Date(isolates$Collection.date)

#Plot Faceted by Year (Work in Progress)
df_timeseries <- as.data.frame(isolates %>% group_by(Collect_Date, Isolation_Source_Updated) %>% summarise(N_Isolate = n()))
df_ts <- df_timeseries %>% filter(Isolation_Source_Updated %in% top_15)
#Plot by Day
ggplot(df_ts, aes(x = Collect_Date, y = N_Isolate)) + 
  geom_line(aes(color = Isolation_Source_Updated)) +
  facet_wrap( ~ collection.yr)
theme_minimal() + 
  # Change x axis title
  labs(x = "Date (monthYear)") +
  # Set x breaks and the desired format for the date labels
  scale_x_date(date_breaks = "1 month", date_labels = "%m%Y")+
  ylim(0, 50)
