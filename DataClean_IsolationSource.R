#Load Libraries
library(RecordLinkage)
library(lubridate)
library(tidyverse)
library(stringdist)
library(forcats)

#Load Dataset
isolates <- read.csv("/Users/garyzhou/Downloads/isolates-2.csv")

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

testt_iso <- fct_collapse(test_iso, Missing = c("", "not provided", "not collected", "Not available", "other"),
                          Food = c("food", "Food products"),
                          Blood_Source = c("blood", "Blood, NOS"),
                          Food_Producing_Environment = c("food producing environment surface", "food producing environment", "Food processing environment", "food processing environment"),
                          Environ_Seafood_Processing_or_Equipment = c("Environmental - Seafood processing environment", "Environmental - Seafood Equipment"),
                          Environmental_Swab = c("environmental swab", "swab"),
                          Environment = c("environment", "Environment", "Environ", "environmental"),
                          Sponge = c("sponge", "environmental sponge", "environmental swab sponge"),
                          Clinical = c("clinical", "clinical isolate", "clinical sample"),
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
head(sort(table(testt_iso), decreasing=TRUE), n=100)
sum(head(sort(table(testt_iso), decreasing=TRUE), n=15)) #75% of data in first 15 levels

