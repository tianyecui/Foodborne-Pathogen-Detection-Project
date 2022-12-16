########################### Set up #####################################
require(nnet)
library(lmtest)
library(mlogit)
library(caret)
library(pROC)
library(gtsummary) # source: https://cran.r-project.org/web/packages/gtsummary/vignettes/tbl_regression.html

setwd('/Users/yvonne/Downloads/PHP2550/pda_project')
source('code/step2_preprocess_2.R')
dim(df) # [1] 13312    11

##########################################################################
# This R file was used to build multinomial logistic regression model,
# conduct model check and diagnostics, calculate CI's, 
# and generate summary statistics for the coefficient estimates
##########################################################################



############################ Train/test split ###########################
set.seed(999)
ii <- sample(1:nrow(df), nrow(df)*0.75)
df_train_org <- df[ii,]
df_test <- df[-ii,]
head(ii) #[1] 9115 5444 6205 6727 2409 7822

########################### Adding weights: ###########################
# Note we should only use training set to set up the weights:
ipw_weights <- df_train_org %>% group_by(Isolation.source) %>% summarise(ipw = 1/(n()/nrow(df_train_org)))
ipw_weights

df_train <- df_train_org %>% inner_join(ipw_weights, by='Isolation.source')
dim(df_train)
names(df_train)



# EDA Plot to show high-level proportion:
plot(df$season, df$Isolation.source, ylab='Source', xlab='Season', main='Distribution of Source by Season')
plot(df$USregion, df$Isolation.source, ylab='Source', xlab='US Region', main='Distribution of Source by US Region')

########## Weighted, individual level multinomial:############
mn_indiv_wgtfullseason3 <- multinom(Isolation.source ~ USregion*season + USregion*collection.yr
                                    + SNP.cluster.group + Min.diff
                                    , data = df_train, weights = ipw)
# Stepwise selection
mn_step5 <- step(mn_indiv_wgtfullseason3, direction = "backward")
mn_step5$call # best model so far 
mn_step5_sum <- summary(mn_step5)

# Model Checks
sum(mn_step5_sum$residuals)
mn_step5_sum$deviance
mn_step5_sum$anova
AIC(mn_step5) # 81171.23

# LRT:
lrtest(mn_step5, "SNP.cluster.group")

# predict using test set:
mn_step5_pred <- predict(mn_step5, newdata=df_test[,c('USregion','Min.diff','season', 'collection.yr', 'AMR.genotypes','SNP.cluster.group')], 'class')

# Confusion Matrix:
tb_step5 = table(df_test$Isolation.source, mn_step5_pred)
tb_step5
# Accuracy:
sum(diag(tb_step5))/sum(tb_step5) # 0.6192909

# Predict probability:
mn_step5_pred_prob <- predict(mn_step5, newdata=df_test[,c('USregion','Min.diff','season', 'collection.yr', 'AMR.genotypes','SNP.cluster.group')], 'prob')

# Confusion matrix:
cm_mnstep5 <- confusionMatrix(mn_step5_pred, df_test$Isolation.source) # https://rpubs.com/beane/n4_2
cm_mnstep5byclass = data.frame(cm_mnstep5$byClass)
cm_mnstep5byclass
# write.csv(cm_mnstep5byclass, 'data/MN_level1.csv')
# write.csv(cm_mnstep5$overal, 'data/MN_level1_Overall.csv')

# List of AUCs:
auc_mn <- sapply(1:5, function(i) multiclass.roc(df_test$Isolation.source, mn_step5_pred_prob[,i])$auc)
auc_mn 
mean(auc_mn) 

# write.csv(auc_mn, 'data/MN_level1_AUC.csv')

# Plot ROC:
plot(roc(df_test$Isolation.source, mn_step5_pred_prob[,1]))
plot(roc(df_test$Isolation.source, mn_step5_pred_prob[,2]), add=TRUE, col=2)
plot(roc(df_test$Isolation.source, mn_step5_pred_prob[,3]), add=TRUE, col=3)
plot(roc(df_test$Isolation.source, mn_step5_pred_prob[,4]), add=TRUE, col=4)
plot(roc(df_test$Isolation.source, mn_step5_pred_prob[,5]), add=TRUE, col=5)



########## CI calculation ##########
# CI for coefficients:
mn_step5_coef <- mn_step5_sum$coefficients
mn_step5_se <- mn_step5_sum$standard.errors
hist(mn_step5_se <- mn_step5_sum$standard.errors)
mn_step5_se_df = data.frame(mn_step5_se)
mn_step5_coef_df = data.frame(mn_step5_coef)

# Calculate CI manually:
mn_step5_lowerci <- mn_step5_coef - 1.96 * mn_step5_se
mn_step5_upperci <- mn_step5_coef + 1.96 * mn_step5_se

# Table for coefficient CI:
ci_result_all <- data.frame(cbind(t(mn_step5_coef), t(mn_step5_lowerci), t(mn_step5_upperci)) %>% round(2))
ci_result_all <- ci_result_all[,c(1,5,9,2,6,10,3,7,11,4,8,12)]
ci_result_all$var <- rownames(ci_result_all)


# Indicator for significant or not:
ci_result_all <- ci_result_all %>% 
  mutate(aqsig = ifelse(Aquatic_Animal.1*Aquatic_Animal.2>0, 1, 0),
         clsig = ifelse(Clinical.Human.1*Clinical.Human.2>0, 1, 0),
         lasig = ifelse(Land_Animal.1*Land_Animal.2>0, 1, 0),
         plsig = ifelse(Plants.1*Plants.2>0, 1, 0))

# Odds Ratio and CI
ci_result_all_OR <- cbind(round(exp(ci_result_all[,1:12]),2), ci_result_all[,13:ncol(ci_result_all)])
# write.csv(ci_result_all_OR, 'data/ci_result_all_OR.csv')


######## Gt Output ##########
# Create gtsummary:
mn_step5_output <- tbl_regression(mn_step5, exponentiate = TRUE)
mn_step5_output_tbl <- mn_step5_output$table_body

# wide table for selected variables to save space
mn_step5_output_wide <- tbl_regression(mn_step5, include = c('USregion', 'season', 'Min.diff', 'collection.yr', 'USregion:season'), exponentiate = TRUE) %>% multinom_pivot_wider() # 'USregion:season'

save(mn_step5_output_wide, file = 'data/mn_step5_output_wide')

# wide table for all variables
mn_step5_output_wide_all <- tbl_regression(mn_step5, exponentiate = TRUE) %>% multinom_pivot_wider() 

save(mn_step5_output_wide_all, file = 'data/mn_step5_output_wide_all')

