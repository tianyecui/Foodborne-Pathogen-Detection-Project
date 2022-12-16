#########Level 2#################
isolates_lvl2 <- read.csv("/Users/garyzhou/Downloads/isolates_cc_USA_lvl2.csv")

############### Create Season variable: (based on collection.mon) ###############
isolates_lvl2 <- isolates_lvl2 %>% 
  mutate (season = ifelse(collection.mon %in% c(3,4,5), 'spring',
                          ifelse(collection.mon %in% c(6,7,8), 'summer',
                                 ifelse(collection.mon %in% c(9,10,11), 'autumn',
                                        ifelse(collection.mon %in% c(12,1,2), 'winter', NA)))))

# sanity check:
# tmp1 <- isolates_lvl2 %>% select(season, collection.mon) %>% unique()


############### Create US region: ###############
northeast <- c('USA:CT', 'USA:ME', 'USA:VT', 'USA:NH', 'USA:MA', 'USA:RI','USA:NY', 'USA:NJ', 'USA:PA')
midwest <- c('USA:IL', 'USA:IN', 'USA:MI', 'USA:OH', 'USA:WI',  'USA:IA', 'USA:KS', 'USA:MO', 'USA:MN','USA:NE','USA:ND','USA:SD')
south <- c('USA:DE', 'USA:FL', 'USA:GA', 'USA:MD', 'USA:NC', 'USA:SC', 'USA:VA', 'USA:WV', 'USA:DC',
           'USA:AL', 'USA:MS', 'USA:TN', 'USA:KY',
           'USA:OK', 'USA:AR','USA:TX', 'USA:LA')
west <- c('USA:AZ','USA:NM','USA:NV', 'USA:UT', 'USA:CO', 'USA:ID', 'USA:WY', 'USA:MT',
          'USA:WA', 'USA:OR', 'USA:CA','USA:AK', 'USA:HI')

isolates_lvl2 <- isolates_lvl2 %>% mutate(USregion = ifelse(location_new %in% northeast, 'northeast',
                                                            ifelse(location_new %in% midwest, 'midwest',
                                                                   ifelse(location_new %in% south, 'south',
                                                                          ifelse(location_new %in% west, 'west', 'other')))))

isolates_lvl2$USregion <- as.factor(isolates_lvl2$USregion)
isolates_lvl2$USregion <- relevel(isolates_lvl2$USregion, ref='northeast')

############### Create SNP Clusters ###############
#Take Top 20 Most Freq SNP Clusters
names_SNP_cluster <- names(head(sort(table(isolates_lvl2$SNP.cluster), decreasing=TRUE), n=20))
#Put all other SNP Clusters into "Other"
isolates_lvl2$SNP_cluster_group <- case_when(!(isolates_lvl2$SNP.cluster %in% names_SNP_cluster) ~ "other", TRUE ~ as.character(isolates_lvl2$SNP.cluster)) 

#Double Check
head(sort(table(isolates_lvl2$SNP.cluster), decreasing=T), 20)
sum(table(case_when(!(isolates_lvl2$SNP.cluster %in% names_SNP_cluster) ~ "other", TRUE ~ as.character(isolates_lvl2$SNP.cluster))) [-1])
sort(table(case_when(!(isolates_lvl2$SNP.cluster %in% names_SNP_cluster) ~ "other", TRUE ~ as.character(isolates_lvl2$SNP.cluster))) [-1], decreasing = T)
dim(isolates_lvl2)
table(isolates_lvl2$SNP_cluster_group)

#Export as Dataset
write.csv(isolates_lvl2, "/Users/garyzhou/Downloads/isolates_lvl2_cc_USA_SNP.csv", row.names=FALSE)
