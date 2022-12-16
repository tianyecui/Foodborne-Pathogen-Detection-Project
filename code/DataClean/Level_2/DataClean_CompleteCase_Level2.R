#Level 2
isolates_df <- read.csv('/Users/garyzhou/Downloads/isolates_lvl2_cc_USA_SNP.csv')
colnames(isolates_df)

#Create Min SNP Diff Variable
isolates_df$min_SNP <- pmin(isolates_df$Min.same, isolates_df$Min.diff, na.rm=TRUE)

#Select relevant variables
isolates_df <- isolates_df[,c('AMR.genotypes' , 'SNP_cluster_group', 'USregion', 'season', 'collection.yr', 'min_SNP', 'Isolation_Group')]

#Factorize categorical vars
isolates_df$AMR.genotypes <- as.factor(isolates_df$AMR.genotypes)
isolates_df$SNP_cluster_group <- as.factor(isolates_df$SNP_cluster_group)
isolates_df$USregion <- as.factor(isolates_df$USregion)
isolates_df$season <- as.factor(isolates_df$season)
isolates_df$collection.yr <- as.factor(isolates_df$collection.yr)

#Take complete cases
isolates <- isolates_df[complete.cases(isolates_df),]
dim(isolates)

#Create Dataset
write.csv(isolates, "/Users/garyzhou/Downloads/isolates_cc_USA_SNP_lvl2.csv", row.names=FALSE)

