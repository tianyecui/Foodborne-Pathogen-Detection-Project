#Load Libraries
library(lubridate)
library(tidyverse)
library(forcats)

#Load Dataset
isolates <- read.csv("/Users/garyzhou/Downloads/isolates_filtered_collect_date_imputed.csv")
isolates_NN <- isolates

#Take USA Only
isolates_NN <- subset(isolates_NN, !(location_new %in% c("Asia", "Europe", "North America", "South America", "Oceania", "Africa")))

#Find unique levels of isolation source
unique_isol_source <- unique(isolates_NN$Isolation.source)

########################################################################################
#Cases with just Cow
beef_new <- grep("beef|burger", unique_isol_source, ignore.case=TRUE, value=TRUE)
Beef <- beef_new[!grepl("STICK|stick|pork|cheese", beef_new)]
bovine <- grep("bovine|bos taurus|Bos taurus", unique_isol_source, value=TRUE)
Bovine_bos_taurus <- bovine[!grepl("milk|feces|manure", bovine)]
misc_beef <- c("oxtail", "rump steak", "spiced veal")
Cow <- c(Beef, Bovine_bos_taurus, misc_beef)
grep("veal", unique_isol_source, ignore.case=TRUE, value=TRUE) #ham sausage veal excluded

####################################################################################################################################################################################
#Chicken
chicken_new <- grep("chicken", unique_isol_source, ignore.case=TRUE, value=TRUE) #chicken salad and chicken sandwich (no other meat specified) are included;
Chicken <- chicken_new[!grepl("STICK|stick|pork|cheese|nut|vegetable|corn|pasta|cashew", chicken_new)] # nut/walnut excluded

####################################################################################################################################################################################
#Pork 
pork <- grep("pork", unique_isol_source, ignore.case=TRUE, value=TRUE)
Pork <- pork[!grepl("beef", pork)]
ham <- grep("ham", unique_isol_source, ignore.case=TRUE, value=TRUE)
Ham <- ham[!grepl("egg|cheese|Collars|turkey|cheddar|veal|swiss|beef|omelette", ham)]
bacon <- grep("bacon", unique_isol_source, ignore.case=TRUE, value=TRUE)
Bacon <- bacon[!grepl("CHEESE", bacon)]
Pig <- c(Pork, Ham, Bacon)

####################################################################################################################################################################################
#Multi-ingredient Meat/Poultry
#"ham sausage veal"
meat_stick <- grep("stick", unique_isol_source, ignore.case=TRUE, value=TRUE)
Meat_stick <- meat_stick[!grepl("carrot|environmental|drum", meat_stick)]

#Turkey
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
grep("carrot", unique_isol_source, ignore.case=TRUE, value=TRUE) #peas and carrots
onion <- grep("onion", unique_isol_source, ignore.case=TRUE, value=TRUE)
Onion <- onion[!grepl("cheese", onion)]
Root <- c(Potato, Onion, grep("root|beet|carrot|celery|radish|artichoke", unique_isol_source, ignore.case=TRUE, value=TRUE))

### Seeded Vegetables ###
tomato <- grep("tomato", unique_isol_source, ignore.case=TRUE, value=TRUE)
Tomato <- tomato[!grepl("cheese", tomato)]
pepper_or_chili <- grep("pepper|chili|chile", unique_isol_source, ignore.case=TRUE, value=TRUE)
Pepper_or_Chili <- pepper_or_chili[!grepl("cheese|stick|oni", pepper_or_chili)]
grep("squash|zucchini", unique_isol_source, ignore.case=TRUE, value=TRUE) # included "raw refrigerated nonflex plastic squash" 
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

################
### Beans ###
bean <- grep("bean", unique_isol_source, ignore.case=TRUE, value=TRUE)
bean 
grep("pea", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("cream cheese", unique_isol_source, ignore.case=TRUE, value=TRUE)

####################################################################################################################################################################################
##### Cheese #####
cheese <- grep("cheese", unique_isol_source, ignore.case=TRUE, value=TRUE)
Cheese <- cheese[!grepl("ham|chicken|popcorn|corn|BACON|egg|sausage", cheese)]

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

#Animal Feces
animal_rectal <- grep("rectal", unique_isol_source, ignore.case=TRUE, value=TRUE)
Animal_rectal <- animal_rectal[!grepl("Rectal", animal_rectal)]

animal_feces <- grep("feces", unique_isol_source, ignore.case=TRUE, value=TRUE)
Animal_feces <- animal_feces[!grepl("poultry|blood", animal_feces)]
Animal_Feces <- Animal_feces[-which(Animal_feces == "feces")]

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

####################################################################################################################################################################################
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
test_iso <- isolates_NN$Isolation.source
test_iso[is.na(test_iso)] <- "missing"

#Regroup levels, some levels directly added based on prevalence as a top category
testt_iso <- fct_collapse(test_iso, Missing = c("", "missing", "not provided", "not available", "not collected", "Not available", "other"),
                          Food = c("food", "Food products"),
                          Food_Producing_Environment = c("food producing environment surface", "meat environment", "meat processing facility", "food producing environment", 
                                                         "food processing", "food contact surface", "Food production environment","Food processing environment", "food processing environment", "Food and food processing environment",
                                                         "environmental swab from ready to eat facility", "Environmental: food-contact surface", "Food contact surface in food processing environment",
                                                         "Slaughterhouse-environment", "environmental swab sponge food contact surface", "environmental swab sponge produce facility"),
                          Environment = c("swab", "Silage", "Factory", Environment, "Drain", "soil", "sponge", "silage", "envrionmental swab sponge", env_water, "manure", "surface wipe"),
                          Clinical = c("clinical", "clinical isolate", "neonate pharyngeal smear", "clinical sample", "clinical/host-associated", Ascites, Amniotic, Bile, "body fluid", "Pleural Fluid", "urine",
                                       c("blood", "Blood, NOS", "Blood culture"), "wound", "stool", "feces", "placenta", "whole blood", "Tissue", "tissue", "external ear canal",
                                       CSF, "cervico-vaginal swab", "Fluid", "Abscess", "Cerebrospinal fluid (meningitis)", "Fluid Pleural", "Ear swab", "cystic fluid"),
                          Meat = c("retail meat", "raw meat", "mixed meats", "meat", "Raw Meat", "mixed ground meat", Cow, "ovine", Pig, "poultry", Chicken, Egg, Turkey, "minced meat", "ground meat", "lamb", "bacon"),
                          Human_Source = c("human", "human listeriosis", "patient"),
                          RTE_Food = c("ready to eat food", "RTE Product"),
                          Dairy =  c(Dairy, "raw cow milk", "milk (Bos taurus)", "milk product", "bulk tank milk", "butter", "Milking system", "teat swab","Udders", "Raw Milk", "milk-raw bovine", "raw bovine milk", "Pasteurized Milk"),
                          Vegetables = c(Vegetable_Row_Crop, "produce", "packaged pickles", "Food - Horticulture", Seeded_veg, Root, Vegetable, Fungi, bean, "hummus", "salad"),
                          Fruit = c(Fruit, "corn", "frozen corn", c(grep("avocado", unique_isol_source, ignore.case=TRUE, value=TRUE), "guacamole", "frozen guacamole",
                                                                    "fresh guacamole", "fresh raw guacamole")),
                          Seafood = c(Seafood, "sushi"),
                          Cured_Processed_Meat_Source = c(Meat_stick, "cooked ham", "ready to eat deli meat ham", "deli ham","Salami", "salami paste production", "salami", "mixed meat sausage",
                                                          "tea sausage", "retail deli", "drain-deli", "deli meat", "Deli swab", "deli", "sausage", "pork beef sausage"),
                          Animal_Feces = c(Animal_Feces, Animal_rectal)
)

#Put back into dataset
isolates_NN$isolation_group <- testt_iso

#% of data in first 5 levels (Including Missing Category)
sum(head(sort(table(isolates_NN$isolation_group), decreasing=TRUE))/length(isolates_NN$isolation_group))

#Group into 5 categories
isolates_USA_only <- isolates_NN %>% mutate(
  Isolation_Group=case_when(
    str_detect(isolation_group, 'Meat|Dairy|Cured_Processed_Meat_Source|animal|Animal_Feces') ~ 'Land_Animal',
    str_detect(isolation_group, 'Seafood') ~ 'Aquatic_Animal',
    str_detect(isolation_group, 'Vegetables|Fruit|Agaricus bisporus production chain') ~ 'Plants',
    str_detect(isolation_group, 'Clinical|Human_Source') ~ 'Clinical/Human',
    isolation_group=='Environment' ~ 'Environment'))

#Check dimensions and freq table
dim(isolates_USA_only)   
table(isolates_USA_only$Isolation_Group)

#Complete Case for Isolation Group
isolates_cc_USA <- isolates_USA_only[!is.na(isolates_USA_only$Isolation_Group),]

#Min SNP Calculation
isolates_cc_USA$min_SNP <- pmin(isolates_cc_USA$Min.same, isolates_cc_USA$Min.diff, na.rm=TRUE)
summary(isolates_cc_USA$min_SNP)

#Add Season
isolates_cc_USA <- isolates_cc_USA %>% mutate (season = ifelse(collection.mon %in% c(3,4,5), 'spring',
                                                 ifelse(collection.mon %in% c(6,7,8), 'summer',
                                                        ifelse(collection.mon %in% c(9,10,11), 'autumn',
                                                               ifelse(collection.mon %in% c(12,1,2), 'winter', NA)))))

#Add Region
northeast <- c('USA:CT', 'USA:ME', 'USA:VT', 'USA:NH', 'USA:MA', 'USA:RI','USA:NY', 'USA:NJ', 'USA:PA')
midwest <- c('USA:IL', 'USA:IN', 'USA:MI', 'USA:OH', 'USA:WI',  'USA:IA', 'USA:KS', 'USA:MO', 'USA:MN','USA:NE','USA:ND','USA:SD')
south <- c('USA:DE', 'USA:FL', 'USA:GA', 'USA:MD', 'USA:NC', 'USA:SC', 'USA:VA', 'USA:WV', 'USA:DC',
           'USA:AL', 'USA:MS', 'USA:TN', 'USA:KY',
           'USA:OK', 'USA:AR','USA:TX', 'USA:LA')
west <- c('USA:AZ','USA:NM','USA:NV', 'USA:UT', 'USA:CO', 'USA:ID', 'USA:WY', 'USA:MT',
          'USA:WA', 'USA:OR', 'USA:CA','USA:AK', 'USA:HI')

isolates_cc_USA <- isolates_cc_USA %>% mutate(USregion = ifelse(location_new %in% northeast, 'northeast',
                                                  ifelse(location_new %in% midwest, 'midwest',
                                                         ifelse(location_new %in% south, 'south',
                                                                ifelse(location_new %in% west, 'west', 'other')))))

#Generate Freq Table
table(isolates_cc_USA$Isolation_Group)

#Output dataset as csv
write.csv(isolates_cc_USA, "/Users/garyzhou/Downloads/isolates_cc_USA.csv", row.names = FALSE)

