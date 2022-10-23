# load libraries
library(tidyverse)

# load dataset
df<- read.csv("/Users/tianyecui/Desktop/2550 Practical Data Analysis/final proj/isolates.csv")

df<- df %>% mutate(
  # rename the levels of U.S. states as format USA:RI
  location_new=case_when(
    Location %in% c('USA:missing','USA: n/a','USA') ~ 'USA',
    str_detect(Location, 'NY|New York|NYC') ~ 'USA:NY',
    str_detect(Location, 'VA') ~ 'USA:VA',
    str_detect(Location, 'OK|Oklahoma') ~ 'USA:OK',
    str_detect(Location, 'CA|LA|Los Angeles') ~ 'USA:CA',
    str_detect(Location, 'NC|North Carolina') ~ 'USA:NC',
    str_detect(Location, 'NJ') ~ 'USA:NJ',
    str_detect(Location, 'OH') ~ 'USA:OH',
    str_detect(Location, 'WI') ~ 'USA:WI',
    str_detect(Location, 'IL|Peoria|Illinois') ~ 'USA:IL',
    str_detect(Location, 'MO') ~ 'USA:MO',
    str_detect(Location, 'AZ|Arizona') ~ 'USA:AZ',
    str_detect(Location, 'TX') ~ 'USA:TX',
    str_detect(Location, 'GA|Georgia') ~ 'USA:GA',
    str_detect(Location, 'MA|Massachusetts|Boston') ~ 'USA:MA',
    str_detect(Location, 'OR|Oregon') ~ 'USA:OR',
    str_detect(Location, 'PA|Pennsylvania') ~ 'USA:PA',
    str_detect(Location, 'MD') ~ 'USA:MD',
    str_detect(Location, 'CT|Connecticut') ~ 'USA:CT',
    str_detect(Location, 'DE') ~ 'USA:DE',
    str_detect(Location, 'MN') ~ 'USA:MN',
    str_detect(Location, 'MI|Michigan') ~ 'USA:MI',
    str_detect(Location, 'AL') ~ 'USA:AL',
    str_detect(Location, 'NH') ~ 'USA:NH',
    str_detect(Location, 'North Dakota') ~ 'USA:ND',
    str_detect(Location, 'South Dakota') ~ 'USA:SD',
    str_detect(Location, 'Nebraska') ~ 'USA:NE',
    str_detect(Location, 'CO') ~ 'USA:CO',
    str_detect(Location, 'PA') ~ 'USA:PA',
    str_detect(Location, 'Washington') ~ 'USA:WA',
    str_detect(Location, 'KY|Kentucky') ~ 'USA:KY',
    str_detect(Location, 'Puerto Rico') ~ 'USA:PR',
    str_detect(Location, 'USA:Fl') ~ 'USA:FL',
    
    # continents 
    str_detect(Location, "Canada|Mexico|Guatemala|Guadeloupe|Jamaica") ~ "North America", # excluding U.S.
    str_detect(Location, "South Africa|Chile|Brazil|Colombia|Peru|Argentina|Venezuela|Ecuador|Paraguay|Uruguay|French Guiana") ~ "South America",
    str_detect(Location, "Russia|Germany|Netherlands|France|Austria|United Kingdom|Italy|Ireland|Sweden|Denmark|Switzerland|Spain|Poland|Latvia|Belgium|Finland|Norway|Bulgaria|Iceland|Romania|Slovenia|Luxembourg|Portugal|Lithuania|Greece|Liechtenstein|Hungary|Cyprus|Moldova|Ukraine|Estonia|Turkey|Belarus|Slovakia|USSR") ~ "Europe",
    str_detect(Location, "Korea|Viet Nam|Philippines|China|Japan|Thailand|India|Mumbai|Malaysia|Taiwan|Qatar|Cambodia|Indonesia|Singapore|Israel|Lebanon") ~ "Asia",
    str_detect(Location, "Australia|New Zealand") ~ "Oceania",
    str_detect(Location, "Algeria|Reunion|Morocco|Senegal|Mali|Ethiopia|Egypt") ~ "Africa",
    
    ## NAs
    Location %in% c("not available","Not available","not collected","not provided") ~ NA_character_,
    TRUE ~ Location))
