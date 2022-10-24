########### Setup ########### 
library(tidyverse)
library(knitr)
library(gridExtra)
library(grid)
library(tableone)
library(lubridate) 
library(table1)
library(kableExtra)

setwd('~/downloads/php2550/pda_project')
isolates <- read.csv('isolates_filtered_collect_date_imputed.csv')
dim(isolates)

# Replace missing values with NA
for (i in 1:dim(isolates)[2]) {
  isolates[isolates[,i] %in% c('',' ', '  '), i] <- NA
}


##################################################################
############ Genotypes Data Clean and Exploratory ################
##################################################################
extractGene <- function(genestr){
  #' Function to clean up and extract individual Gene from genotypes variable
  #'  
  #'  @param genestr string genotype string 
  #'  @return individual gene strings extracted from genotype variable
  
  # Split strings by comma ',':
  str_split = unlist(strsplit(genestr,',',fixed=TRUE))
  # Get rid of special characters ( ) - in the string:
  str_split = str_replace_all(str_split, "[[()-]]", "")
  
  # extract anything string before '=COMPLETE':
  gene_regex <- ".+(?==COMPLETE)"
  str_extracted <- str_extract_all(str_split, gene_regex)
  str_vec = c()
  str_vec = sapply(1: length(str_split), function(i) str_vec[i] = str_extracted[i])
  return(unlist(str_vec))
}

# example genotype strings:
str1 = 'dfrG=COMPLETE,fosX=COMPLETE,tet(M)=COMPLETE'
str2 = 'abc-f=HMM,fosX=COMPLETE,lin=COMPLETE'
extractGene(str1)
extractGene(str2)



# Function to extract distinct values from column: e.g. AMR.genotypes
seeCol <- function(col){
  isolates %>% select(col) %>% distinct()
}
AMR.core <- seeCol('AMR.genotypes.core')



# Function to add Gene dummy variables:
geneDummy <- function(col){
  #' Create dummy variables for genotypes from specified column
  #'  
  #'  @param col string column to conver to dummy variable
  #'  @return isolates table with extra columns for dummy variables
  
  # Extract distinct values from column: e.g. AMR.genotypes
  gene_table <- seeCol(col)
  
  # Add cleaned up complete gene column:
  for (i in 1:dim(gene_table)[1]){
    gene_table[i,'complete_gene'] <- paste(extractGene(as.character(gene_table[,col][i])), collapse = ',')
  }
  
  # Create dummy variable for genes:
  core_genes = unique(unlist(strsplit(as.character(gene_table$complete_gene),',',fixed=TRUE)))
  gene_table = cbind(gene_table, sapply(core_genes, function(x) as.integer(grepl(x, gene_table$complete_gene))))
  
  isolates_gene_dummy <- isolates %>% left_join(gene_table, by = col)
  return(isolates_gene_dummy)
}

# Create dummy variables for genes in 'AMR.genotypes':
dummy_amr <- geneDummy('AMR.genotypes')

# column index:
AMR_idx <- which(names(dummy_amr)=='AMR.genotypes')
dummy_example <- dummy_amr[,c(AMR_idx, (dim(isolates)[2]+1):dim(dummy_amr)[2])] %>% distinct()
dummy_example[1:10, 1:8] %>% kable(caption = 'Example table for original genotypes variable and dummy variables')




########### EDA for gene variables: ###########
# extract column index for gene indicator:
ind <- which(names(dummy_amr)=='complete_gene') + 1

# Create summary frequencies for genes:
n_complete <- colSums(dummy_amr[,ind:dim(dummy_amr)[2]])
pct_complete <- round(colSums(dummy_amr[,ind:dim(dummy_amr)[2]]) / dim(isolates)[1],4)
gene_summary <- data.frame(n_complete, pct_complete) 
gene_summary <- gene_summary %>% 
  mutate(AMR_gene = rownames(gene_summary)) %>% arrange(desc(n_complete)) %>% filter(pct_complete >= 0.001)

# reorder columns:
gene_summary <- gene_summary[,c('AMR_gene','n_complete', 'pct_complete')]
rownames(gene_summary) <- NULL
kable(gene_summary, caption = 'Complete AMR Genes Frequency') 







