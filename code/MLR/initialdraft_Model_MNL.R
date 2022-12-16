setwd('/Users/yvonne/Downloads/PHP2550/pda_project')


source('eda_proj.R') # pre-processing code was included there
isolates <- isolates_USA_only

# Complete cases for isolation group:
isolates <- isolates[!is.na(isolates$Isolation_Group), ]
dim(isolates)
# 17795    60

############### Create Season variable: (based on collection.mon) ###############
isolates <- isolates %>% mutate (season = ifelse(collection.mon %in% c(3,4,5), 'spring',
                                     ifelse(collection.mon %in% c(6,7,8), 'summer',
                                            ifelse(collection.mon %in% c(9,10,11), 'autumn',
                                                   ifelse(collection.mon %in% c(12,1,2), 'winter', NA)))))

# sanity check:
# tmp1 <- isolates %>% select(season, collection.mon) %>% unique()
# remove NA season:
isolates <- isolates %>% filter(!is.na(season))
dim(isolates)
# 15668    60

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
# sanity check:
# tmp2 <- isolates %>% select(USregion, location_new) %>% unique()


# Just look at US regions where state information is known:
isolates <- isolates %>% filter(USregion != 'other')
dim(isolates)
# 11332    61


############### Just keep some certain sources for now: ###############
# !!!Replace Isolation source with Gary's refined category:
length(isolates$Isolation.source) == length(isolates$Isolation_Group)

isolates$Isolation.source <- isolates$Isolation_Group
isolates$Isolation.source <- as.factor(isolates$Isolation.source)


# keep complete cases for mindiff/same:
isolates_sub_comp <- isolates %>% 
  # drop those with both missing in min.diff and min.same
  filter(!is.na(Min.diff) | !is.na(Min.same)) %>%
  # update min.diff
  mutate(Min.diff.new=pmin(Min.diff, Min.same, na.rm=T))

# !!!Replace Min.diff with Tianye's refined mindiff:
isolates_sub_comp$Min.diff <- isolates_sub_comp$Min.diff.new
dim(isolates_sub_comp)
# 10373    62


################################ Only keep several variables to use##########################################################
features <- c('SNP.cluster', 'Contigs','AMR.genotypes', 'location_new', 'USregion', 'Isolation.source', 'season', 'Min.diff')

df <- isolates_sub_comp[,features]
dim(df)
# [1] 10373     8

# Now, Final variables were selected. Set categorical var as factor: 
df$SNP.cluster <- as.factor(df$SNP.cluster)
df$AMR.genotypes <- as.factor(df$AMR.genotypes)
df$location_new <- as.factor(df$location_new)
df$Isolation.source <- as.factor(df$Isolation.source)
df$Isolation.source <- relevel(df$Isolation.source, ref='Environment')

df$USregion <- as.factor(df$USregion)
df$season <- as.factor(df$season)
df$season <- relevel(df$season, ref='spring')

# drop unused levels:
df$Isolation.source <- droplevels(df$Isolation.source)
df$USregion <- droplevels(df$USregion)
df$AMR.genotypes <- droplevels(df$AMR.genotypes)


################################################################
########## Multinomial Logistic Regression ################
################################################################
require(nnet)
library(lmtest)
library(mlogit)
######################## R: multinom #################################### 
# Data structure for multinom function: frequency tables:
df <- df %>% select(USregion, Isolation.source, AMR.genotypes, season, Min.diff)
df_multi <- df %>%
  select(USregion, Isolation.source, AMR.genotypes, season, Min.diff) %>%
  group_by(USregion, Isolation.source, AMR.genotypes, season) %>% 
  summarise(freq = n(), avg_diff = mean(Min.diff))

# gm0: only main effects:
gm0 <- multinom(Isolation.source ~ USregion + avg_diff + season , data = df_multi, weights = freq)
summary(gm0)
BIC(gm0)

# 95% CI:
lower_ci <- summary(gm0)$coefficients - 1.96 * summary(gm0)$standard.errors  
lower_ci %>%
  data.frame() %>%  round(2) %>%
  kbl(caption = "95% CI - lower bound") %>%
  kable_styling

upper_ci <- summary(gm0)$coefficients + 1.96 * summary(gm0)$standard.errors 
upper_ci %>%
  data.frame() %>% round(2) %>%
  kbl(caption = "95% CI - upper bound") %>%
  kable_styling

write.csv(lower_ci, 'lower_ci.csv')
write.csv(upper_ci, 'upper_ci.csv')

# gm1: with interaction
gm1 <- multinom(Isolation.source ~ avg_diff + season*USregion , data = df_multi, weights = freq)
summary(gm1)

# gm2: include AMR genotypes
gm2 <- multinom(Isolation.source ~ USregion + avg_diff + season + AMR.genotypes, data = df_multi, weights = freq)
summary(gm2)
exp(coef(gm2))

# Compare BIC
BIC(gm0)
BIC(gm1) # best
BIC(gm2)

# 
lrtest(gm0, gm1) #gm1 better
lrtest(gm0, gm2) #gm2 better, but lower BIC

plot(df$Isolation.source, df$Min.diff)
plot(df$Isolation.source, df$season)
plot(df$Isolation.source, df$USregion)



########################## Sanity check: ##########################
obs <- df_multi %>%
  pivot_wider(names_from = Isolation.source, values_from = freq, values_fn = sum, values_fill = 0)
dim(df_multi)
dim(obs)

# somehow we lost 1 row after pivoting to wider... so we did left join below to ensure that we don't miss any records
obsTmp <- df_multi %>% left_join(obs, by=c('USregion', 'season', 'AMR.genotypes', 'avg_diff'))
dim(obsTmp)

# Just keep the frequency columns:
obs <- t(obsTmp[, (ncol(obsTmp)-4):ncol(obsTmp)])
obs

# Expected (probabilities)
exp <- t(fitted(gm0))
exp

for (i in 1:ncol(exp)) {
  exp[, i] <- exp[, i] * sum(obs[, i])
}

seeResult <- round(cbind(t(exp), t(obs)))
# rename columns:
newnames <- c()
for (i in 1:5) newnames[i] <- paste0('Exp.',  colnames(seeResult)[i])
for (i in 6:10) newnames[i] <- paste0('Obs.',  colnames(seeResult)[i])
newnames
colnames(seeResult) <- newnames

# Initial visualization for expected vs observed:
seeResult <- cbind(obsTmp[,1:6], seeResult)
ggplot(data=seeResult) +
  geom_point(aes(Obs.Environment, Exp.Environment)) +
  geom_point(aes(Obs.Land_Animal, Exp.Land_Animal), col='red') +
  geom_point(aes(Obs.Aquatic_Animal, Exp.Aquatic_Animal), col='blue') +
  geom_point(aes(Obs.Plants, Exp.Plants), col='green')

head(seeResult, 8) %>% kbl(caption = "Observed vs Expected Frequencies") %>% kable_styling




######################## Another R func: Mlogit #################################### 
# data structure for Mlogit:
mdata <- mlogit.data(df, varying = NULL, choice='Isolation.source', shape='wide')

# can also add ipw weights later
mlogit0 <- mlogit(Isolation.source ~ 1| Min.diff + USregion + season, data = mdata, reflevel = 'Environment')
summary(mlogit0)
# exp(coef(mlogit0))

mlogit_fit <- fitted(mlogit0, outcome = FALSE) # ref: http://www2.uaem.mx/r-mirror/web/packages/mlogit/vignettes/mlogit.pdf

# plot(mlogit_fit[,1], type='l')
# hist(mlogit_fit[,4])


mlogit1 <- mlogit(Isolation.source ~ 1| Min.diff + season, data = mdata, reflevel = 'Environment')

mlogit2 <- mlogit(Isolation.source ~ 1| Min.diff, data = mdata, reflevel = 'Environment')

mlogit3 <- mlogit(Isolation.source ~ 1| Min.diff + USregion + season + AMR.genotypes, data = mdata, reflevel = 'Environment')

lrtest(mlogit1, mlogit2)
lrtest(mlogit1, mlogit0)
lrtest(mlogit2, mlogit0)
lrtest(mlogit3, mlogit0)


mlogit4 <- mlogit(Isolation.source ~ 1| Min.diff + USregion * season, data = mdata, reflevel = 'Environment')
summary(mlogit4)
lrtest(mlogit4, mlogit0)

AIC(mlogit3)







#######################################################################################
########### Code for preparing initial Neural network data (Unused for now) ###########

################################ Balance Class ######################################## 
idx1 = which(df$Isolation.source =='food') #2552
idx0 = which(df$Isolation.source =='blood') #1266

set.seed(99)
resample1 = sample(idx1, size = nrow(df)/2, replace = TRUE)
resample0 = sample(idx0, size = nrow(df)/2, replace = TRUE)

d1 = df[resample1,]
d0 = df[resample0,]
dfbal = rbind(d1, d0)
table(dfbal$Isolation.source)
dim(dfbal)

df <- dfbal

######## Data matrix for modeling ######## 
df_mat <- model.matrix(Isolation.source~., df)

# Split into test and train 
set.seed(10)
ids_train <- sample.int(nrow(df_mat), size = 0.75*nrow(df_mat))
train_x <- df_mat[ids_train,]
train_y <- df$Isolation.source[ids_train]
test_x <- df_mat[-ids_train,] 
test_y <- df$Isolation.source[-ids_train]


# Scale: standardize
train_means <- apply(train_x, 2, mean)
train_sds <- apply(train_x, 2, sd)
train_x <- sweep(sweep(train_x, 2L, train_means), 2, train_sds, "/")
test_x <- sweep(sweep(test_x, 2L, train_means), 2, train_sds, "/")

# see which column has NA:
seeNA_tr<- sapply(1: ncol(train_x), function(i) sum(is.na(train_x[,i])))
seeNA_tr
# remove NA columns:
rem_col_idx <- which(seeNA_tr !=0)

# seeNA_test <- sapply(1: ncol(test_x), function(i) sum(is.na(test_x[,i])))
# rem2 <- which(seeNA_test !=0)


train_x <- train_x[, -rem_col_idx]
test_x <- test_x[, - rem_col_idx]

dim(train_x)



write.csv(train_x, file='train_x.csv')
write.csv(train_y, file='train_y.csv')
write.csv(test_x, file='test_x.csv')
write.csv(test_y, file='test_y.csv')



