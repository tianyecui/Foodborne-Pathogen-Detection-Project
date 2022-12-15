############### Create Season variable: (based on collection.mon) ###############
isolates <- isolates %>% 
  mutate (season = ifelse(collection.mon %in% c(3,4,5), 'spring',
                          ifelse(collection.mon %in% c(6,7,8), 'summer',
                                 ifelse(collection.mon %in% c(9,10,11), 'autumn',
                                        ifelse(collection.mon %in% c(12,1,2), 'winter', NA)))))

# sanity check:
# tmp1 <- isolates %>% select(season, collection.mon) %>% unique()

isolates$season <- as.factor(isolates$season)
isolates$season <- relevel(isolates$season, ref='spring')

############### Create US region: ###############
northeast <- c('USA:CT', 'USA:ME', 'USA:VT', 'USA:NH', 'USA:MA', 'USA:RI','USA:NY', 'USA:NJ', 'USA:PA')
midwest <- c('USA:IL', 'USA:IN', 'USA:MI', 'USA:OH', 'USA:WI',  'USA:IA', 'USA:KS', 'USA:MO', 'USA:MN','USA:NE','USA:ND','USA:SD')
south <- c('USA:DE', 'USA:FL', 'USA:GA', 'USA:MD', 'USA:NC', 'USA:SC', 'USA:VA', 'USA:WV', 'USA:DC',
           'USA:AL', 'USA:MS', 'USA:TN', 'USA:KY',
           'USA:OK', 'USA:AR','USA:TX', 'USA:LA')
west <- c('USA:AZ','USA:NM','USA:NV', 'USA:UT', 'USA:CO', 'USA:ID', 'USA:WY', 'USA:MT',
          'USA:WA', 'USA:OR', 'USA:CA','USA:AK', 'USA:HI')

isolates <- isolates %>% mutate(USregion = ifelse(location_new %in% northeast, 'northeast',
                                                  ifelse(location_new %in% midwest, 'midwest',
                                                         ifelse(location_new %in% south, 'south',
                                                                ifelse(location_new %in% west, 'west', 'other')))))
isolates$USregion <- as.factor(isolates$USregion)
isolates$USregion <- relevel(isolates$USregion, ref='northeast')
