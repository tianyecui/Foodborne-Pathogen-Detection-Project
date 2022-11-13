#Load Libraries
library(RecordLinkage)
library(lubridate)
library(tidyverse)
library(stringdist)
library(forcats)

#Load Dataset
###########Imputed Dataset
isolates <- read.csv("/Users/garyzhou/Downloads/isolates_filtered_collect_date_imputed.csv")
isolates$
sapply(isolates, function(x) length(unique(isolates)))
length(unique(isolates$Serovar))
length(unique(isolates$SNP.cluster))

summary(isolates$Min.same)
summary(isolates$Min.diff)

length(unique(isolates$Strain))
length(unique(isolates$Strain))
#Explore Isolation Source
str(isolates)
length(unique(isolates$Isolation.source))
length(unique(isolates$IFSAC.category))
#Sort by Freq of Isolation Source
freq_isol <- isolates %>% group_by(Isolation.source) %>% summarise(freq = n()) %>% arrange(desc(freq))
head(as.data.frame(freq_isol), n=30)

#Find string matches using grepl
unique_isol_source <- unique(isolates$Isolation.source)
test_iso <- isolates$Isolation.source

####################################################################################################################################################################################
#Cases with just Cow and no Chicken or Pork
beef_new <- grep("beef", unique_isol_source, ignore.case=TRUE, value=TRUE)
beef_no_chick_pork <- c(grep("chicken", beef_new, ignore.case=TRUE, value=TRUE), grep("pork", beef_new, ignore.case=TRUE, value=TRUE))
beef_final <- beef_new[!beef_new %in% beef_no_chick_pork]
bovine_bos_taurus <- intersect(grep("bovine|bos taurus", unique_isol_source, value=TRUE), grep("milk", unique_isol_source, invert=TRUE, value=TRUE)); bovine_bos_taurus #bovine/bos taurus without milk 
misc_beef <- c("oxtail", "rump steak", "spiced veal", "hamburger beef", "grass fed burger meat")
Beef <- c(beef_final, bovine_bos_taurus, misc_beef)

grep("burger", unique_isol_source, ignore.case=TRUE, value=TRUE)
####################################################################################################################################################################################


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

#Multi-ingredient Meat/Poultry
#"ham sausage veal"
meat_stick <- grep("stick", unique_isol_source, ignore.case=TRUE, value=TRUE)
Meat_stick <- meat_stick[!grepl("carrot|environmental|drum", meat_stick)]

turkey <- grep("turkey", unique_isol_source, ignore.case=TRUE, value=TRUE)
Turkey <- turkey[!grepl("ham|stick|swiss|beef|fish", turkey)]
#Egg
Egg_test <- grep("egg", unique_isol_source, ignore.case=TRUE, value=TRUE); Egg_test
Egg <- c(Egg_test[!grepl("sandwich|salad|cheese", Egg_test)], "egg white salad chive", "egg salad sandwich", "egg sandwich", "egg salad")


####################################################################################################################################################################################
###Root Veggies###
#Potato/Sweet Potato
potato_combo <- grep("potato", unique_isol_source, ignore.case=TRUE, value=TRUE)
Potato <- potato_combo[!grepl("egg", potato_combo)]
grep("root", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("beet", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("carrot", unique_isol_source, ignore.case=TRUE, value=TRUE) #peas and carrots
grep("celery", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("radish", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("artichoke", unique_isol_source, ignore.case=TRUE, value=TRUE)
onion <- grep("onion", unique_isol_source, ignore.case=TRUE, value=TRUE)
Onion <- onion[!grepl("cheese", onion)]
Root <- c(Potato, Onion, grep("root|beet|carrot|celery|radish|artichoke", unique_isol_source, ignore.case=TRUE, value=TRUE))

### Seeded Vegetables ###
tomato <- grep("tomato", unique_isol_source, ignore.case=TRUE, value=TRUE)
Tomato <- tomato[!grepl("cheese", tomato)]
pepper_or_chili <- grep("pepper|chili|chile", unique_isol_source, ignore.case=TRUE, value=TRUE)
Pepper_or_Chili <- pepper_or_chili[!grepl("cheese|stick|oni", pepper_or_chili)]

grep("cucumber", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("squash|zucchini", unique_isol_source, ignore.case=TRUE, value=TRUE) # included "raw refrigerated nonflex plastic squash" 
grep("pumpkin", unique_isol_source, ignore.case=TRUE, value=TRUE)
Seeded_veg <- c(Tomato, Pepper_or_Chili, grep("cucumber|squash|pumpkin|zucchini", unique_isol_source, ignore.case=TRUE, value=TRUE))

### Leafy Green/Flower/Stem Vegetables ###
cabbage <- grep("cabbage", unique_isol_source, ignore.case=TRUE, value=TRUE)
Cabbage <- cabbage[!grepl("meat", cabbage)]
lettuce <- grep("lettuce", unique_isol_source, ignore.case=TRUE, value=TRUE)
Lettuce <- lettuce[!grepl("sandwich", lettuce)]
basil <- grep("basil", unique_isol_source, ignore.case=TRUE, value=TRUE)
Basil <- basil[!grepl("microgreen|pasta", basil)] #remove "basil pesto pasta salad"
grep("green", unique_isol_source, ignore.case=TRUE, value=TRUE)
Green <- c("mixed green", "basil mixture microgreen", "green salad", "Microgreen Mix")
Vegetable_Row_Crop <- c(Lettuce, Green, Basil, Cabbage, grep("leaf|greens|spinach|kale|parsley|cilantro|broccoli|chard|sprout|asparagus|collard green|leek|cauliflower", 
                                                              unique_isol_source, ignore.case=TRUE, value=TRUE))
vegetable <- grep("vegetable", unique_isol_source, ignore.case=TRUE, value=TRUE)
Vegetable <- c(vegetable[!grepl("chicken|Lentil", vegetable)], "bagged salad mixture")

### Fungi ###
Fungi <- grep("mushroom|enoki", unique_isol_source, ignore.case=TRUE, value=TRUE)

### Legume ###
corn <- grep("corn", unique_isol_source, ignore.case=TRUE, value=TRUE)
Corn <- corn[!grepl("cheese|chicken|beef|cheddar|silage|corner", corn)]
Corn

################
### Beans ###
bean <- grep("bean", unique_isol_source, ignore.case=TRUE, value=TRUE)
bean 
grep("pea", unique_isol_source, ignore.case=TRUE, value=TRUE)

grep("cream cheese", unique_isol_source, ignore.case=TRUE, value=TRUE)

################################################################################


#meat, seafood, shrimp, fish, sushi, lamb, bacon, poultry, tuna, 

grep("pasta", unique_isol_source, ignore.case=TRUE, value=TRUE)




#water not sprout irrigation water

####################################################################################################################################################################################
##### Cheese #####
cheese <- grep("cheese", unique_isol_source, ignore.case=TRUE, value=TRUE)
Cheese <- cheese[!grepl("ham|chicken|popcorn", cheese)]
### Exclude ###
#"ham cheese sandwich", "BACON & CHEESE COOKED BRAT", "beef beans rice cheese burrito", "egg cheese salad", "ham green onion cheese salad"
#"ham and cheese sandwich", "ready to eat egg ham and cheese burrito", "chicken cheese chimichanga", "ham salami cheese lettuce and tomato sandwich",
#"ham egg cheese sandwich", "chili cheese corn dog", "egg sausage cheese burrito"

### Probable ###
#"ham and cheese spread", "ham green onion cheese spread", "cream cheese with red pepper",
#"sun dried tomato basil cheese curd", "red pepper feta cheese spread", "popped cheese popcorn"
#"red pepper cheese", "cheddar pear cheese"

### Included ###
#"sheep combined with cow cheese  product, cream cheese, and "
#"environmental swab sponge cold room floor under cheese rack", "Interior vent, cheese display cooler, zone 2", "Interior vent, cheese display cooler, zone 3"
#"environmental swab from a cheese manufacturing plant"

grep("cheese", unique_isol_source, value=TRUE)

##### Dairy #####
dairy <- grep("dairy", unique_isol_source, ignore.case=TRUE, value=TRUE)
milk <- c("milk", "Milk filter", "raw milk", "milk filter")
icecream <- grep("ice cream", unique_isol_source, ignore.case=TRUE, value=TRUE)
Dairy <- c(Cheese, milk, icecream, dairy)

##### Fruit #####
cherry <- grep("cherry", unique_isol_source, ignore.case=TRUE, value=TRUE)
Cherry <- cherry[!grepl("tomato", cherry)]
strawberry <- grep("strawberry", unique_isol_source, ignore.case=TRUE, value=TRUE)
Strawberry <- strawberry[!grepl("jelly", strawberry)]
Fruit <- c(grep("peach|nectarine|mango|kiwi|apple|cantaloupe|melon|blueberry|blackberry|plum", unique_isol_source, ignore.case=TRUE, value=TRUE), "mixed fruit", "fruit", "pear", Cherry, Strawberry, "orange")
grep("apricot", unique_isol_source, ignore.case=TRUE, value=TRUE)


####################################################################################################################################################################################

### Water ###
env_water_check <- grep("water", unique_isol_source, ignore.case=TRUE, value=TRUE)
env_water <- env_water_check[!grepl("watermelon|cabbage|shrimp|ham", env_water_check)]


### Environment ###
envir <- grep("envir", unique_isol_source, ignore.case=TRUE, value=TRUE)
Environment <- c(envir[!grepl("apple|Slaughterhouse|produce|food|Food|sausage|meat|chicken|grinder|dairy|deli|smoke|Mushroom|ice cream|Seafood|hobart|salad|jetter|parlor|eat|slicer|knife|grease tray|cheese|kitchen|fish|cabbage|spinner|Spinner|restaurant|seafood|soybean|pasta|Waffle", envir)], 
                 "environmental: non-food-contact surface", "environmental non-food-contact surface", "environmental swab sponge non food contact surface")
Environment


####################################################################################################################################################################################
##### Fish #####
grep("fish", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("salmon", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("seafood", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("trout", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("tilapia", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("bass", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("tuna", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("cod", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("sardine", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("herring", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("bream", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("yellowtail", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("pollock", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("flounder", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("caviar", unique_isol_source, ignore.case=TRUE, value=TRUE)

##### Shellfish #####
clam <- grep("clam", unique_isol_source, ignore.case=TRUE, value=TRUE)
Clam <- clam[!grepl("squash", clam)]
oyster <- grep("oyster", unique_isol_source, ignore.case=TRUE, value=TRUE)
Oyster <- oyster[!grepl("mushroom", oyster)]
Shellfish <- c(Clam, Oyster, grep("crab|shrimp|prawn|lobster|crayfish|crawfish|mussel|scallop|squid|octopus|urchin", unique_isol_source, ignore.case=TRUE, value=TRUE))

Seafood <- c(Clam, Oyster, grep("crab|shrimp|prawn|lobster|crayfish|crawfish|mussel|scallop|squid|octopus|fish|salmon|seafood|trout|tuna|herring|pollock|caviar", unique_isol_source, ignore.case=TRUE, value=TRUE))

##### Clinical #####
Ascites <- c(grep("ascites|ascitic", unique_isol_source, ignore.case=TRUE, value=TRUE), "Asitic Fluid")
Amniotic <- grep("amnio", unique_isol_source, ignore.case=TRUE, value=TRUE)
bile <- grep("bile", unique_isol_source, ignore.case=TRUE, value=TRUE)
Bile <- bile[!grepl("ham", bile)]
grep("blood", unique_isol_source, ignore.case=TRUE, value=TRUE)
CSF <- c(grep("CSF", unique_isol_source, ignore.case=TRUE, value=TRUE), 
  "cerebrospinal fluid", "Cerebral spinal fluid", "Cerebrospinal fluid, NOS", "blood cerebrospinal fluid", "Cerebrospianl fluid", "Cerebral Spinal Fluid")
####################################################################################################################################################################################
#Change all NA to "missing"
test_iso[is.na(test_iso)] <- "missing"
testt_iso <- fct_collapse(test_iso, Missing = c("", "missing", "not provided", "not collected", "Not available", "other"),
                          Food = c("food", "Food products"),
                          Food_Producing_Environment = c("food producing environment surface", "meat processing facility", "food producing environment", 
                                                         "food processing", "Food processing environment", "food processing environment", "Food and food processing environment",
                                                         "environmental swab from ready to eat facility", "Environmental: food-contact surface"),
                          Environment = c("swab", Environment, soil_new, "sponge", "envrionmental swab sponge", env_water, "manure"),
                          Clinical = c("clinical", "clinical isolate", "clinical sample", "clinical/host-associated", Ascites, Amniotic, Bile, c("blood", "Blood, NOS", "Blood culture"),
                                       CSF),
                          Meat = c("retail meat", "meat", "Raw Meat", Beef, "ovine", c(pork_final, "ham"), chicken_final, Egg, Turkey, "minced meat"),
                          Human_Source = c("human", "human listeriosis", "patient"),
                          RTE_Food = c("ready to eat food", "RTE Product"),
                          Dairy =  c(Dairy, "raw cow milk"),
                          Vegetables = c(Vegetable_Row_Crop, Seeded_veg, Root, Vegetable, Fungi, bean, "hummus"),
                          Fruit = c(Fruit, c(grep("avocado", unique_isol_source, ignore.case=TRUE, value=TRUE), "guacamole", "frozen guacamole")),
                          Seafood = Seafood,
                          Cured_Processed_Meat_Source = c(Meat_stick, "Salami", "salami paste production", "salami", "retail deli", "drain-deli", "deli meat", "Deli swab", "deli", "sausage"),
                          Cow_Feces = c("feces cow", "Cow feces")
                          )

#Look at unique isolation sources in descending order
head(sort(table(testt_iso), decreasing=TRUE), n=15)
sum(head(sort(table(testt_iso), decreasing=TRUE), n=15)) #93% of data in first 15 levels

sort(table(testt_iso), decreasing=TRUE)[1:50]
sort(table(testt_iso), decreasing=TRUE)[50:200]
sort(table(testt_iso), decreasing=TRUE)[300:600]

sum(head(sort(table(testt_iso), decreasing=TRUE), n=50))
length(unique(testt_iso))

#End



















#Misc Code - Not Useful
####################################################################################################################################################################################

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


testt_iso <- fct_collapse(test_iso, Missing = c("", "missing", "not provided", "not collected", "Not available", "other"),
                          Food = c("food", "Food products"),
                          Blood_Source = c("blood", "Blood, NOS", "Blood culture"),
                          Food_Producing_Environment = c("food producing environment surface", "food producing environment", "Food processing environment", "food processing environment", "Food and food processing environment"),
                          Environ_Seafood_Processing_or_Equipment = c("Environmental - Seafood processing environment", "Environmental - Seafood Equipment"),
                          Environment = c("environment", "Environment", "Environ", "environmental", "environmental swab", "swab", 
                                          soil_new, "sponge", "environmental sponge", "environmental swab sponge"),
                          Clinical = c("clinical", "clinical isolate", "clinical sample", "clinical/host-associated", Ascites),
                          Meat = c("retail meat", "meat"),
                          Human_Source = c("human", "human listeriosis", "patient"),
                          RTE_Food = c("ready to eat food", "RTE Product"),
                          Avocado_or_Guac = c(grep("avocado", unique_isol_source, ignore.case=TRUE, value=TRUE), "guacamole", "frozen guacamole"),
                          Dairy =  Dairy,
                          Beef = Beef,
                          Veggie_Row = Vegetable_Row_Crop,
                          Seeded_Veg = Seeded_veg,
                          Root_Veg = Root,
                          Pork = c(pork_final, "ham"),
                          Bean = bean,
                          Fruit = Fruit,
                          Misc_Veg = Vegetable,
                          Salmon = c("smoked salmon", "salmon", "Smoked salmon"),
                          Shellfish = Shellfish,
                          Cured_Processed_Meat_Source = c("Salami", "salami paste production", "salami", "retail deli", "drain-deli", "deli meat", "Deli swab", "deli", "sausage"),
                          Cow_Feces = c("feces cow", "Cow feces"),
                          CSF = c(grep("CSF", unique_isol_source, ignore.case=TRUE, value=TRUE), "cerebrospinal fluid", "Cerebral spinal fluid", "Cerebrospinal fluid, NOS", "blood cerebrospinal fluid"), #CSF and blood were grouped as CSF
                          Chicken = c(chicken_final, Egg))
