# number of clusters that we want to keep; and group others as 'other'
num_snp_cluster <- 20

top_clusters <- isolates%>% group_by(SNP.cluster) %>% summarise(n = n()) %>% arrange(desc(n))  %>% drop_na() %>% head(num_snp_cluster) %>%
  select(SNP.cluster)
top_clusters <- droplevels(top_clusters$SNP.cluster) # drop unused levels
top_clusters

isolates$SNP.cluster.group <- ifelse(isolates$SNP.cluster %in% top_clusters, as.character(isolates$SNP.cluster), 'other')
isolates$SNP.cluster.group <- factor(isolates$SNP.cluster.group)
