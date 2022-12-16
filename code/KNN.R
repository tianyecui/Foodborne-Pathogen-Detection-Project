library(caret)
library(tidyverse)
library(pROC)
#interation source, time
df<- read.csv("/Users/tianyecui/Desktop/2550 Practical Data Analysis/final proj/data/data_for_KNN.csv")

# weights
#wt<- 1/(table(df$Isolation_Group)/nrow(df))
#wt<- 1/(table(df$Isolation_Group)/nrow(df))/10
wt<- 1/table(df$Isolation_Group)*100

# complete case analysis
df<- df %>% filter(cc==1) %>%
  mutate(Isolation_Group=as.factor(Isolation_Group),
         AMR.genotypes=as.factor(AMR.genotypes),
         collection.mon=as.factor(collection.mon),
         collection.yr=as.factor(collection.yr),
         season=as.factor(season),
         state=as.factor(state),
         USregion=as.factor(USregion),
         SNP.cluster.group=as.factor(SNP.cluster.group),
         # normalization
         Min.diff.new=(Min.diff.new - min(Min.diff.new)) / 
           (max(Min.diff.new) - min(Min.diff.new)),
         # get the weights for each source group
         weights=case_when(Isolation_Group=='Aquatic_Animal' ~ wt[1],
                           Isolation_Group=='Clinical/Human' ~ wt[2],
                           Isolation_Group=='Environment' ~ wt[3],
                           Isolation_Group=='Land_Animal' ~ wt[4],
                           Isolation_Group=='Plants' ~ wt[5])) %>%
  select(-cc,-state,-collection.mon,-collection.yr,-SNP.cluster.group) 
anyNA(df)

table(df$Isolation_Group)

# train test split
set.seed(1)
sample.ind<- sample(1:nrow(df), size=nrow(df)*0.7,replace=FALSE) #random selection of 70% data
train<- df[sample.ind,] # 70% training data
test<- df[-sample.ind,] # remaining 30% test data

table(train$Isolation_Group)

trctrl<- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1)
knn_fit<- train(Isolation_Group~., data=train, method="knn",
                 trControl=trctrl, weights=weights, 
                 #preProcess=c("center", "scale"),
                 tuneLength = 10)
knn_fit

#Plotting yields Number of Neighbors Vs accuracy (based on repeated cross validation)
ggplot(knn_fit)

# get prediction on testing set
knnPredict<- predict(knn_fit, newdata=test)
test.prob<- cbind(test$Isolation_Group,predict(knn_fit, newdata=test, type="prob"))
View(test.prob%>%filter(test$Isolation_Group=="Plants"))
# overall accuracy
mean(knnPredict == test$Isolation_Group)

# ============== Confusion matrix ==============
cm<- confusionMatrix(knnPredict, test$Isolation_Group)
cm
cm_byclass<- data.frame(cm$byClass)
#write.csv(cm_byclass, '/Users/tianyecui/Desktop/2550 Practical Data Analysis/final proj/result_knn.csv')

## =========== plot =============
tbl<- as.data.frame(cm$table) %>% 
  group_by(Reference) %>%
  summarise(Prediction=Prediction,
            pct=Freq/sum(Freq)*100) %>%
  # format the labels for percentages
  mutate(pct.label=paste0(sprintf("%.0f", pct), "%"))

ggplot(tbl, aes(x=Reference, y=pct, fill=Prediction)) + 
  geom_bar(position=position_stack(), stat="identity", width=0.8) +
  geom_text(aes(label=pct.label), position=position_stack(vjust = 0.5), size=3) +
  scale_fill_brewer(palette="RdYlGn") +
  xlab("Isolation Source") + ylab("Percentage") 

## =========== auc ================
test.prob<- predict(knn_fit, newdata=test, type="prob")
auc_knn<- sapply(1:5, function(i) multiclass.roc(test$Isolation_Group, test.prob[,i])$auc)
auc_knn # 
mean(auc_knn) #



## ================== level 2 ===========================
# load the complete case dataset for level2 of isolation source from github
df<- read_csv(url("https://raw.githubusercontent.com/tianyecui/Foodborne-Pathogen-Detection-Project/main/data/isolates_cc_USA_SNP_lvl2.csv"))
df<- df %>% filter(Isolation_Group!='Food/RTE_Food/Food Producing Environment') 
wt<- 1/table(df$Isolation_Group)*100

#=============== removing "Food/RTE_Food/Food Producing Environment" ===============
df<- df %>%
  mutate(Isolation_Group=as.factor(Isolation_Group),
         AMR.genotypes=as.factor(AMR.genotypes),
         collection.yr=as.factor(collection.yr), # not include in KNN model
         season=as.factor(season),
         USregion=as.factor(USregion),
         SNP_cluster_group=as.factor(SNP_cluster_group),
         # normalization
         min_SNP=(min_SNP - min(min_SNP)) / (max(min_SNP) - min(min_SNP)),
         # get the weights for each source group
         weights=case_when(Isolation_Group=='Animal_Feces_or_Nasal_Swab' ~ wt[1],
                           Isolation_Group=='Aquatic_Animal' ~ wt[2],
                           Isolation_Group=='Clinical/Human' ~ wt[3],
                           Isolation_Group=='Dairy' ~ wt[4],
                           Isolation_Group=='Environment' ~ wt[5],
                           Isolation_Group=='Fruit' ~ wt[6],
                           Isolation_Group=='Meat' ~ wt[7],
                           Isolation_Group=='Poultry' ~ wt[8],
                           Isolation_Group=='Vegetables' ~ wt[9])) %>%
  select(-SNP_cluster_group,-collection.yr) 
anyNA(df)

# train test split
set.seed(1)
sample.ind<- sample(1:nrow(df), size=nrow(df)*0.7,replace=FALSE) #random selection of 70% data
train<- df[sample.ind,] # 70% training data
test<- df[-sample.ind,] # remaining 30% test data

#table(train$Isolation_Group)

trctrl<- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1)
knn_fit<- train(Isolation_Group~., data=train, method="knn",
                trControl=trctrl, weights=weights, 
                #preProcess=c("center", "scale"),
                tuneLength = 10)
knn_fit

#Plotting yields Number of Neighbors Vs accuracy (based on repeated cross validation)
ggplot(knn_fit)

# get prediction on testing set
knnPredict<- predict(knn_fit, newdata=test)

# overall accuracy
mean(knnPredict == test$Isolation_Group)

#Get the confusion matrix to see accuracy value and other parameter values
cm<- confusionMatrix(knnPredict, test$Isolation_Group)
cm
tbl<- as.data.frame(cm$table) %>% 
  group_by(Reference) %>%
  summarise(Prediction=Prediction,
            pct=Freq/sum(Freq)*100) %>%
  # format the labels for percentages
  mutate(pct.label=paste0(sprintf("%.0f", pct), "%"))

# plot
ggplot(tbl, aes(x=Reference, y=pct, fill=Prediction)) + 
  geom_bar(position=position_stack(), stat="identity", width=0.8) +
  geom_text(aes(label=pct.label), position=position_stack(vjust = 0.5), size=3) +
  scale_fill_brewer(palette="RdYlGn") +
  xlab("Isolation Source") + ylab("Percentage") 

# prediction probabilities 
test.prob<- data.frame(test$Isolation_Group,predict(knn_fit, newdata=test, type="prob"))
View(test.prob%>%filter(test.Isolation_Group=="Clinical/Human"))


## auc
test.prob<- predict(knn_fit, newdata=test, type="prob")
auc_knn<- sapply(1:9, function(i) multiclass.roc(test$Isolation_Group, test.prob[,i])$auc)
auc_knn # 
mean(auc_knn) #0.6093463


