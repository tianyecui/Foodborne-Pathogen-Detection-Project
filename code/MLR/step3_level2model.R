
##########################################################################
# This R file was used to implement MLR for level 2 outcome
##########################################################################
lv2 = read.csv('data/isolates_cc_USA_SNP_lvl2.csv')
dim(lv2) #  14269

# Replace with newly defined outcome
lv2$Isolation.source = lv2$Isolation_Group
# Exclude Food/RTE_Food/Food Producing Environment:
lv2 <- lv2 %>% filter(Isolation.source != 'Food/RTE_Food/Food Producing Environment')

############################ Train/test split ###########################
set.seed(999)
ii <- sample(1:nrow(lv2), nrow(lv2)*0.75)
lv2_train <- lv2[ii,]
lv2_test <- lv2[-ii,]
head(ii) #[1] 9115 5444 6205 6727 2409 7822


########################### Adding weights: ###########################
ipw_weights_lv2 <- lv2_train %>% group_by(Isolation.source) %>% summarise(ipw = 1/(n()/nrow(lv2_train)))

lv2_train <- lv2_train %>% inner_join(ipw_weights_lv2, by='Isolation.source')

mn_indiv_wgtfullseason3_lv2 <- multinom(Isolation.source ~ USregion*season + USregion*collection.yr
                                     + min_SNP 
                                    + SNP_cluster_group
                                    , data = lv2_train, weights = ipw)

dim(lv2_train)
mn_step5_lv2 <-mn_indiv_wgtfullseason3_lv2
mn_step5_lv2$call # best model so far

AIC(mn_step5_lv2) # 329055.2

# Sum of Square
sum(mn_step5_lv2_sum$residuals)
mn_step5_lv2_sum$deviance
mn_step5_lv2_sum$anova

# predict using test set:
mn_step5_pred_lv2 <- predict(mn_step5_lv2, newdata=lv2_test[,c('USregion','min_SNP','season', 'collection.yr'
                                                               , 'SNP_cluster_group'
                                                               )], 'class')
# Confusion Matrix:
tblv2 = table(lv2_test$Isolation.source, mn_step5_pred_lv2)

# Accuracy:
sum(diag(tblv2))/sum(tblv2) # 0.279148 # exclude foodenv: 0.3181054

# pred probability:
mn_step5_pred_prob_lv2 <- predict(mn_step5_lv2, newdata=lv2_test[,c('USregion','min_SNP','season', 'collection.yr', 'SNP_cluster_group')], 'prob')

# Confusion matrix:
library(caret)
cm_mnstep5_lv2 <- confusionMatrix(mn_step5_pred_lv2, lv2_test$Isolation.source) # https://rpubs.com/beane/n4_2
cm_mnstep5_lv2$overal
cm_mnstep5_lv2byclass = data.frame(cm_mnstep5_lv2$byClass)
cm_mnstep5_lv2byclass
# write.csv(cm_mnstep5_lv2byclass, 'data/MN_level2_10class.csv')

# AUC:
library(pROC)
lvauc <- sapply(1:10, function(i) multiclass.roc(lv2_test$Isolation.source, mn_step5_pred_prob_lv2[,i])$auc)
lvauc
mean(lvauc)
# write.csv(cm_mnstep5_lv2$overal, 'data/MN_level2_10classOverall.csv')

plot(roc(lv2_test$Isolation.source, mn_step5_pred_prob_lv2[,1]))
plot(roc(lv2_test$Isolation.source, mn_step5_pred_prob_lv2[,2]), add=TRUE, col=2)
plot(roc(lv2_test$Isolation.source, mn_step5_pred_prob_lv2[,3]), add=TRUE, col=3)
plot(roc(lv2_test$Isolation.source, mn_step5_pred_prob_lv2[,4]), add=TRUE, col=4)
plot(roc(lv2_test$Isolation.source, mn_step5_pred_prob_lv2[,5]), add=TRUE, col=5)



