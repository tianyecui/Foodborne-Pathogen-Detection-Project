#Load Libraries
library(lubridate)
library(tidyverse)
library(forcats)

#Load Imputed Dataset
isolates <- read.csv("/Users/garyzhou/Downloads/isolates_filtered_collect_date_imputed.csv")
isolates_NN <- isolates
#Take USA Only
isolates_NN <- subset(isolates_NN, !(location_new %in% c("Asia", "Europe", "North America", "South America", "Oceania", "Africa")))
#Find unique levels of isolation source
unique_isol_source <- unique(isolates_NN$Isolation.source)

########################################################################################
####Meat####
#Cases with just Cow and no Chicken or Pork
beef_new <- grep("beef|burger", unique_isol_source, ignore.case=TRUE, value=TRUE)
Beef <- beef_new[!grepl("STICK|stick|pork|cheese", beef_new)]
Beef #54

bovine <- grep("bovine|bos taurus|Bos taurus", unique_isol_source, value=TRUE)
Bovine_bos_taurus <- bovine[!grepl("milk|feces|manure", bovine)]
Bovine_bos_taurus #29

misc_beef <- c("oxtail", "rump steak", "spiced veal") #steak already covered by other categories
Cow <- c(Beef, Bovine_bos_taurus, misc_beef)
Cow#86

########################################################################################
#Pig 
pork <- grep("pork", unique_isol_source, ignore.case=TRUE, value=TRUE)
Pork <- pork[!grepl("beef", pork)]
Pork #38

ham <- grep("ham", unique_isol_source, ignore.case=TRUE, value=TRUE)
Ham <- ham[!grepl("egg|cheese|Collars|turkey|cheddar|veal|swiss|beef|omelette|pork", ham)]
Ham #33

bacon <- grep("bacon", unique_isol_source, ignore.case=TRUE, value=TRUE)
Bacon <- bacon[!grepl("CHEESE", bacon)]
Bacon #4

grep("pig", unique_isol_source, ignore.case=TRUE, value=TRUE) #None
grep("swine", unique_isol_source, ignore.case=TRUE, value=TRUE) #None
Pig <- c(Pork, Ham, Bacon)
Pig #75
########################################################################################

#Goat, Lamb, Sheep
lamb <- grep("lamb", unique_isol_source, ignore.case=TRUE, value=TRUE)
lamb #4

misc_animal <- c("sheep brain", "raw Bison Tripe", "frozen tripe treat nibblet")
ovis_aries <- grep("ovis", unique_isol_source, ignore.case=TRUE, value=TRUE)
Ovis_aries <- ovis_aries[!grepl("lamb", ovis_aries)]

capra_aegagrus_hircus <- grep("capra", unique_isol_source, ignore.case=TRUE, value=TRUE)

################################################################################################
#Multi-ingredient Meat
meat_categ <- grep("meat", unique_isol_source, ignore.case=TRUE, value=TRUE)
meat <- meat_categ[!grepl("STICK|stick|pork|beef|chicken|turkey|crab|Non meat|Nonmeat|sausage|ham|Beef|Combination|deli|lobster|Poultry|duck|burger", meat_categ)]
meat #36

meat_stick <- grep("stick", unique_isol_source, ignore.case=TRUE, value=TRUE)
Meat_stick <- meat_stick[!grepl("carrot|environmental|drum", meat_stick)]
Meat_stick #15

sausage <- grep("sausage", unique_isol_source, ignore.case=TRUE, value=TRUE)
Sausage <- c(sausage[!grepl("beef|pork|Pork|chicken|egg", sausage)], "pork beef sausage", "beef and pork sausage")
Sausage #36

hot_dog <- grep("hot dog", unique_isol_source, ignore.case=TRUE, value=TRUE)
Hot_dog <- hot_dog[!grepl("Beef", hot_dog)]
Hot_dog #6

salami <- grep("salami", unique_isol_source, ignore.case=TRUE, value=TRUE)
Salami <- salami[!grepl("ham|pork", salami)]
Salami #2

Multi_Meat <- c(meat, Meat_stick, Sausage, Hot_dog); Multi_Meat #93
################################################################################################
#Group all meat
Meat_General <- c(Cow, Pig, Multi_Meat, lamb, Salami, Ovis_aries, capra_aegagrus_hircus, misc_animal, "goat brain stem", "goat brain tissue")
Meat_General #274

#Deli
deli <- grep("deli", unique_isol_source, ignore.case=TRUE, value=TRUE)
Deli <- deli[!grepl("ham|turkey|chicken|beef|tuna|Turkey|fish", deli)]
Deli


#Left out egg sausage cheese burrito
#"Product-RTE-Fully Cooked, Meat/Nonmeat Combination"
####################################################################################################################################################################################
#Chicken
chicken_new <- grep("chicken", unique_isol_source, ignore.case=TRUE, value=TRUE) #chicken salad and chicken sandwich (no other meat specified) are included;
Chicken <- chicken_new[!grepl("STICK|stick|pork|cheese|nut|vegetable|corn|pasta|cashew", chicken_new)] # nut/walnut excluded
Chicken #59

#Poultry
poultry <- grep("poultry", unique_isol_source, ignore.case=TRUE, value=TRUE) 
poultry #8

#Turkey
turkey <- grep("turkey", unique_isol_source, ignore.case=TRUE, value=TRUE)
Turkey <- turkey[!grepl("ham|stick|swiss|beef|fish", turkey)]
Turkey #38

#Egg
Egg_test <- grep("egg", unique_isol_source, ignore.case=TRUE, value=TRUE); Egg_test
Egg <- c(Egg_test[!grepl("sandwich|salad|cheese", Egg_test)], "egg white salad chive", "egg salad sandwich", "egg sandwich", "egg salad")
Egg #8

#duck
duck <- grep("duck", unique_isol_source, ignore.case=TRUE, value=TRUE) #3

#Group Poultry
Poultry <- c(Chicken, poultry, Turkey, Egg, "quail", duck); Poultry #117
####################################################################################################################################################################################
###Root Veggies###
potato_combo <- grep("potato", unique_isol_source, ignore.case=TRUE, value=TRUE)
Potato <- potato_combo[!grepl("egg", potato_combo)]
grep("carrot", unique_isol_source, ignore.case=TRUE, value=TRUE) #peas and carrots
onion <- grep("onion", unique_isol_source, ignore.case=TRUE, value=TRUE)
Onion <- onion[!grepl("cheese", onion)]
Root <- c(Potato, Onion, grep("root|beet|carrot|celery|radish|artichoke", unique_isol_source, ignore.case=TRUE, value=TRUE))
Root #26

####################################################################################################################################################################################
### Seeded Vegetables ###
tomato <- grep("tomato", unique_isol_source, ignore.case=TRUE, value=TRUE)
Tomato <- tomato[!grepl("cheese", tomato)]
pepper_or_chili <- grep("pepper|chili|chile", unique_isol_source, ignore.case=TRUE, value=TRUE)
Pepper_or_Chili <- pepper_or_chili[!grepl("cheese|stick|oni", pepper_or_chili)]
grep("squash|zucchini", unique_isol_source, ignore.case=TRUE, value=TRUE) # included "raw refrigerated nonflex plastic squash" 
Seeded_veg <- c(Tomato, Pepper_or_Chili, grep("cucumber|squash|pumpkin|zucchini", unique_isol_source, ignore.case=TRUE, value=TRUE))
Seeded_veg #35

###################################################################################################
### Leafy Green/Flower/Stem Vegetables (Vegetable Row Crop) ###
cabbage <- grep("cabbage", unique_isol_source, ignore.case=TRUE, value=TRUE)
Cabbage <- cabbage[!grepl("meat", cabbage)]
lettuce <- grep("lettuce", unique_isol_source, ignore.case=TRUE, value=TRUE)
Lettuce <- lettuce[!grepl("sandwich", lettuce)]
basil <- grep("basil", unique_isol_source, ignore.case=TRUE, value=TRUE)
Basil <- basil[!grepl("microgreen|pasta", basil)] #remove "basil pesto pasta salad"
grep("green", unique_isol_source, ignore.case=TRUE, value=TRUE)
Green <- c("mixed green", "basil mixture microgreen", "green salad", "Microgreen Mix")

#Group Vegetable Row Crop
Vegetable_Row_Crop <- c(Lettuce, Green, Basil, Cabbage, grep("leaf|greens|spinach|kale|parsley|cilantro|broccoli|chard|sprout|asparagus|collard green|leek|cauliflower", 
                                                             unique_isol_source, ignore.case=TRUE, value=TRUE))
Vegetable_Row_Crop #81

#Unspecified Vegetable
vegetable <- grep("vegetable", unique_isol_source, ignore.case=TRUE, value=TRUE)
Vegetable <- c(vegetable[!grepl("chicken|Lentil", vegetable)], "bagged salad mixture")
Vegetable #14

#Group all veggies
Veggies <- c(Vegetable, Vegetable_Row_Crop)
Veggies #95

####################################################################################################################################################################################
### Fungi ###
Fungi <- grep("mushroom|enoki", unique_isol_source, ignore.case=TRUE, value=TRUE)
Fungi #7

### Corn ###
corn <- grep("corn", unique_isol_source, ignore.case=TRUE, value=TRUE)
Corn <- corn[!grepl("cheese|chicken|beef|cheddar|silage|corner", corn)]
Corn #8

### Beans ###
bean <- grep("bean", unique_isol_source, ignore.case=TRUE, value=TRUE)
Bean <- bean[!grepl("beef|chicken|pork|cheese", bean)]
Bean #19

pea <- grep("pea", unique_isol_source, ignore.case=TRUE, value=TRUE)
Pea <- pea[!grepl("peach|pear|peanut|Peach", pea)]
Pea #10

#Combine Fungi, Starch, Legume, Veggies
Veggies_Starch <- c(Bean, Pea, Corn, Fungi, Veggies, Seeded_veg)
Veggies_Starch #174

####################################################################################################################################################################################
##### Dairy #####
cheese <- grep("cheese", unique_isol_source, ignore.case=TRUE, value=TRUE)
Cheese <- cheese[!grepl("ham|chicken|popcorn|corn|BACON|egg|sausage|cream", cheese)]
Cheese #63

milk <- grep("milk", unique_isol_source, ignore.case=TRUE, value=TRUE)
Milk <- milk[!grepl("cheese|Cheese", milk)]
Milk #30

icecream <- grep("ice cream", unique_isol_source, ignore.case=TRUE, value=TRUE)
IceCream <- icecream[!grepl("Dairy", icecream)]
IceCream #21

dairy <- grep("dairy", unique_isol_source, ignore.case=TRUE, value=TRUE)
dairy_complete <- dairy[!grepl("manure", dairy)]
dairy_complete #4

cream_cheese <- grep("cream cheese", unique_isol_source, ignore.case=TRUE, value=TRUE)
cream_cheese #4

cream <- grep("cream", unique_isol_source, ignore.case=TRUE, value=TRUE)
Cream <- cream[!grepl("ice|cheese|ICE|Ice|forklift", cream)]
Cream #2
#removed "cream/orange Endure forklift with 3 on it" 
#ice cream flavors:chocolate/cookie/guava/mint/ginger/orange/sherbert; all included

grep("yogurt", unique_isol_source, ignore.case=TRUE, value=TRUE)
grep("butter", unique_isol_source, ignore.case=TRUE, value=TRUE)

#Group Dairy Products
Dairy <- c(Cheese, Milk, IceCream, dairy_complete, cream_cheese, Cream, "Butter", "butter", "regular butter", "yogurt", "Frozen yogurt mix")
Dairy #129

####################################################################################################################################################################################
##### Fruit #####
cherry <- grep("cherry", unique_isol_source, ignore.case=TRUE, value=TRUE)
Cherry <- cherry[!grepl("tomato", cherry)]
strawberry <- grep("strawberry", unique_isol_source, ignore.case=TRUE, value=TRUE)
Strawberry <- strawberry[!grepl("jelly", strawberry)]
Fruit <- c(grep("peach|nectarine|mango|kiwi|apple|cantaloupe|melon|blueberry|blackberry|plum", unique_isol_source, ignore.case=TRUE, value=TRUE), "mixed fruit", "pear", Cherry, Strawberry, "orange")
Fruit #50

Avocado <- grep("avocado|guacamole", unique_isol_source, ignore.case=TRUE, value=TRUE)
Avocado #14

####################################################################################################################################################################################
### Water ###
env_water_check <- grep("water", unique_isol_source, ignore.case=TRUE, value=TRUE)
env_water <- env_water_check[!grepl("watermelon|cabbage|shrimp|ham", env_water_check)]
env_water #33

### Environment ###
envir <- grep("envir", unique_isol_source, ignore.case=TRUE, value=TRUE)
Environment <- c(envir[!grepl("apple|Slaughterhouse|produce|food|Food|sausage|meat|chicken|grinder|dairy|deli|smoke|Mushroom|ice cream|Seafood|hobart|salad|jetter|parlor|eat|slicer|knife|grease tray|cheese|kitchen|fish|cabbage|spinner|Spinner|restaurant|seafood|soybean|pasta|Waffle", envir)], 
                 "environmental: non-food-contact surface", "environmental non-food-contact surface", "environmental swab sponge non food contact surface")
Environment #80

####################################################################################################################################################################################
##### Fish #####
#Test
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

#Group Seafood
Seafood <- c(Clam, Oyster, grep("crab|shrimp|prawn|lobster|crayfish|crawfish|mussel|scallop|squid|octopus|fish|salmon|seafood|trout|tuna|herring|pollock|caviar|anchovy", unique_isol_source, ignore.case=TRUE, value=TRUE))
Seafood #86

####################################################################################################################################################################################
##### Clinical #####
Ascites <- c(grep("ascites|ascitic", unique_isol_source, ignore.case=TRUE, value=TRUE), "Asitic Fluid")
Amniotic <- grep("amnio", unique_isol_source, ignore.case=TRUE, value=TRUE)
bile <- grep("bile", unique_isol_source, ignore.case=TRUE, value=TRUE)
Bile <- bile[!grepl("ham", bile)]
blood <- grep("blood", unique_isol_source, ignore.case=TRUE, value=TRUE)
Blood <- blood[!grepl("CSF", blood)]
Blood #5
CSF <- grep("CSF", unique_isol_source, ignore.case=TRUE, value=TRUE)
CSF #3
fluid <- grep("fluid", unique_isol_source, ignore.case=TRUE, value=TRUE)
Fluid <- fluid[!grepl("taurus|origin", fluid)]
Fluid #50

clinical <- grep("clinical|clinic", unique_isol_source, ignore.case=TRUE, value=TRUE)
clinical #3

urine <- grep("urine", unique_isol_source, ignore.case=TRUE, value=TRUE)
urine #1

tissue <- grep("tissue", unique_isol_source, ignore.case=TRUE, value=TRUE)
human_tissue <- tissue[!grepl("taurus|lamb|goat|Capra", tissue)]
human_tissue #5

#Group Clinical Source
Clinical_source <- c("Ascites", Fluid, "Amniotic membrane", "bile", Blood, CSF, clinical, urine, human_tissue)
Clinical_source #70

####################################################################################################################################################################################
#Group feces, manure, animal feces in separate category
#Animal Feces
animal_rectal <- grep("rectal", unique_isol_source, ignore.case=TRUE, value=TRUE)
Animal_rectal <- animal_rectal[!grepl("Rectal", animal_rectal)]

animal_feces <- grep("feces", unique_isol_source, ignore.case=TRUE, value=TRUE)
Animal_feces <- animal_feces[!grepl("poultry|blood", animal_feces)]
Animal_Feces <- Animal_feces[-which(Animal_feces == "feces")]

manure <- grep("manure", unique_isol_source, ignore.case=TRUE, value=TRUE) #cattle and poultry and misc

###########LEVEL 2##################################################################################################################################
#Change all NA to "missing"
test_iso <- isolates_NN$Isolation.source
test_iso[is.na(test_iso)] <- "missing"

#Regroup levels, some levels directly added based on prevalence as a top category
testt_iso <- fct_collapse(test_iso, Missing = c("", "missing", "not provided", "not available", "not collected", "Not available", "other"),
                          Food = c("food", "Food products"),
                          Food_Producing_Environment = c("food producing environment surface", "food producing environment", 
                                                         "food processing", "food contact surface", "Food production environment","Food processing environment", "food processing environment", "Food and food processing environment",
                                                         "environmental swab from ready to eat facility", "Environmental: food-contact surface", "Food contact surface in food processing environment",
                                                         "environmental swab sponge food contact surface"),
                          Environment = c("swab", "Silage", "Factory", Environment, "holes in floor in front of 4th section of finished part of racking",
                                          "Floor Drain", "corn silage", "Drain", "soil", "sponge", "silage", "envrionmental swab sponge", env_water, "surface wipe",
                                          "Floor/Wall Under 1-Basin Sink (NFCS)", "new plastic bucket interior", "Product-Swab-Non meat",
                                          "Pasture 2 Mill Creek on Hargrove Lake Rd, btw Myers Farm and Dudley Farm Rds"),
                          Clinical = c("patient", "Vaginal", "cervico-vaginal swab", "neonate pharyngeal smear", Clinical_source,
                                       "Referred Culture", "wound", "stool", "feces", "placenta", "external ear canal",
                                       "Abscess", "Ear swab"),
                          Meat = c(Deli, Meat_General, "ovine", "caprine", "barbacoa", "beef pork", "bologna"),
                          Poultry = c(Poultry, "drag swab"),
                          Human_Source = c("human", "human listeriosis"),
                          RTE_Food = c("ready to eat food", "RTE Product", "ready meal", "RTE-Product", "Product-RTE"),
                          Pet_Food = c("pet food", "raw pet dog food", "raw pet food", "pet food", "raw dog food", "frozen dog food"),
                          Dairy = Dairy,
                          Vegetables = c("produce", "environmental swab sponge produce facility", "frozen hash brown", Veggies_Starch, Root, "hummus", "salad", "Agaricus bisporus production chain"),
                          Fruit = c(Fruit, Avocado),
                          Seafood = c(Seafood, "sushi"),
                          Animal_Nasal_Swab = c("black bear nasal swab (Ursus americanus)", "nasal swab (Ursus americanus)", "animal nasal swab"),
                          Animal_Feces = c(Animal_Feces, Animal_rectal, manure)
)

#Look at unique isolation sources in descending order
head(sort(table(testt_iso), decreasing=TRUE), n=50)
sum(head(sort(table(testt_iso), decreasing=TRUE), n=11))/length(testt_iso) #94% of data in first 11 levels (Including Missing Category)

#Put back into dataset
isolates_NN$isolation_group <- testt_iso

#Group into 10 categories
isolates_USA_only <- isolates_NN %>% mutate(
  Isolation_Group=case_when(
    str_detect(isolation_group, 'Meat') ~ 'Meat',
    str_detect(isolation_group, 'Poultry') ~ 'Poultry',
    str_detect(isolation_group, 'Dairy') ~ 'Dairy',
    str_detect(isolation_group, 'Seafood') ~ 'Aquatic_Animal',
    str_detect(isolation_group, 'Vegetables') ~ 'Vegetables',
    str_detect(isolation_group, 'Fruit') ~ 'Fruit',
    str_detect(isolation_group, 'Clinical|Human_Source') ~ 'Clinical/Human',
    str_detect(isolation_group, 'Food|Food_Producing_Environment|Pet_Food|RTE_Food') ~ 'Food/RTE_Food/Food Producing Environment',
    str_detect(isolation_group, 'Animal_Feces|Animal_Nasal_Swab') ~ 'Animal_Feces_or_Nasal_Swab',
    isolation_group=='Environment' ~ 'Environment'))

#Check
dim(isolates_USA_only)   
table(isolates_USA_only$Isolation_Group)

#Number of observations not  in the 10 categories
sum(is.na(isolates_USA_only$Isolation_Group))

#Create dataset
isolates_USA_only_level2 <- isolates_USA_only
dim(isolates_USA_only_level2)
write.csv(isolates_USA_only_level2, "/Users/garyzhou/Downloads/isolates_cc_USA_lvl2.csv", row.names=FALSE)
