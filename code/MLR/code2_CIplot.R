setwd('/Users/yvonne/Downloads/PHP2550/pda_project')
source('code/code1_MLR.R')

##########################################################################
# This R file was used plot 95% CI of coeffcient estimates
##########################################################################
########## CI calc1 Visual: ##########
library(ggplot2)
library(gridExtra)
# All CI:
ggplot(ci_result_all, aes(1:nrow(ci_result_all), Clinical.Human)) +        
  geom_point() +
  geom_errorbar(aes(ymin = Clinical.Human.1, ymax = Clinical.Human.2)) +
  labs(title='95% CI for all coefficients', x='Covariate')


###########  by US region: ########## 
ci_result_region <- ci_result_all[2:5,]

level_order_region <- c('USregionmidwest','USregionwest', 'USregionsouth','USregionother')
lb = -5.5
up = 8

# Aquatic and clinic source by US region:
Aq_region <- ggplot(ci_result_region, aes(var, Aquatic_Animal)) +        
  geom_point() +
  geom_errorbar(aes(ymin = Aquatic_Animal.1, ymax = Aquatic_Animal.2)) +
  geom_hline(yintercept=0, linetype="dashed",  color = "red") +
  labs(x=NULL, y=NULL, title=NULL) +
  scale_x_discrete(limits = rev(level_order_region), labels=rev(c('Midwest', 'West', 'South', 'Other'))) +
  coord_flip(ylim=c(lb, up))
Aq_region


Cl_region <- ggplot(ci_result_region, aes(var, Clinical.Human)) +        
  geom_point() +
  geom_errorbar(aes(ymin = Clinical.Human.1, ymax = Clinical.Human.2)) +
  geom_hline(yintercept=0, linetype="dashed",  color = "red") +
  labs(x=NULL, y=NULL,  title=NULL) +
  scale_x_discrete(limits = rev(level_order_region), labels=rev(c('Midwest', 'West', 'South', 'Other'))) +
  coord_flip(ylim=c(lb, up))


land_region <- ggplot(ci_result_region, aes(var, Land_Animal)) +        
  geom_point() +
  geom_errorbar(aes(ymin = Land_Animal.1, ymax = Land_Animal.2)) +
  geom_hline(yintercept=0, linetype="dashed",  color = "red") +
  labs(x=NULL, y=NULL, y=NULL, title=NULL) +
  scale_x_discrete(limits = rev(level_order_region), labels=rev(c('Midwest', 'West', 'South', 'Other'))) +
  coord_flip(ylim=c(lb, up))

plant_region <- ggplot(ci_result_region, aes(var, Plants)) +        
  geom_point() +
  geom_errorbar(aes(ymin = Plants.1, ymax = Plants.2)) +
  geom_hline(yintercept=0, linetype="dashed",  color = "red") +
  labs(x=NULL, y=NULL,title=NULL) +
  scale_x_discrete(limits = rev(level_order_region), labels=rev(c('Midwest', 'West', 'South', 'Other'))) +
  coord_flip(ylim=c(lb, up))

grid.arrange(Aq_region, Cl_region,land_region, plant_region, nrow=4, top=textGrob("95% CI for Region Coefficients"))


########## by Season ##########
ci_result_season <- ci_result_all[6:8,]
head(ci_result_season)

level_order_season <- rev(c('seasonsummer','seasonautumn', 'seasonwinter'))
lb = -1.4
up = 2.5

head(ci_result_season)

Aq_season <- ggplot(ci_result_season, aes(var, Aquatic_Animal)) +
  geom_point() +
  geom_errorbar(aes(ymin = Aquatic_Animal.1, ymax = Aquatic_Animal.2))  +
  geom_hline(yintercept=0, linetype="dashed",  color = "red")+
  labs(x=NULL, y=NULL,title=NULL) +
  scale_x_discrete(limits = level_order_season, labels=rev(c('Summer', 'Autumn', 'Winter'))) +
  coord_flip(ylim=c(lb, up))  
Aq_season



Cl_season <- ggplot(ci_result_season, aes(var, Clinical.Human)) +        
  geom_point() +
  geom_errorbar(aes(ymin = Clinical.Human.1, ymax = Clinical.Human.2))  +
  geom_hline(yintercept=0, linetype="dashed",  color = "red")+
  labs(x=NULL, y=NULL,title=NULL) +
  scale_x_discrete(limits = level_order_season, labels=rev(c('Summer', 'Autumn', 'Winter'))) +
  coord_flip(ylim=c(lb, up))  

land_season <- ggplot(ci_result_season, aes(var, Land_Animal)) +        
  geom_point() +
  geom_errorbar(aes(ymin = Land_Animal.1, ymax = Land_Animal.2))  +
  geom_hline(yintercept=0, linetype="dashed",  color = "red")+ 
  labs(x=NULL, y=NULL,title=NULL) +
  scale_x_discrete(limits = level_order_season, labels=rev(c('Summer', 'Autumn', 'Winter'))) +
  coord_flip(ylim=c(lb, up))  

plant_season <- ggplot(ci_result_season, aes(var, Plants)) +        
  geom_point() +
  geom_errorbar(aes(ymin = Plants.1, ymax = Plants.2)) +
  geom_hline(yintercept=0, linetype="dashed",  color = "red")+
  labs(x=NULL, y=NULL,title=NULL) +
  scale_x_discrete(limits = level_order_season, labels=rev(c('Summer', 'Autumn', 'Winter'))) +
  coord_flip(ylim=c(lb, up))  

grid.arrange(Aq_season, Cl_season,land_season, plant_season, nrow=4, top=textGrob("95% CI for Season Coefficients"))

head(ci_result_season)


###################### 8 Panel plot ###############
grid.arrange(Aq_season, Aq_region,
             Cl_season, Cl_region,
             land_season, land_region,
             plant_season, plant_region, 
             nrow=4, 
             top=textGrob("Season and Region Coefficients 95% CI"))
grid.text("Aquatic Animal", x = c(0.1), 
          y = unit(.97, "npc"),gp = gpar(fontsize=12))
grid.text("Clinical/Human",  x = c(0.1), 
          y = unit(.73, "npc"),gp = gpar(fontsize=12))
grid.text("Land Animal", x = c(0.08), 
          y = unit(.49, "npc"),gp = gpar(fontsize=12))
grid.text("Plants",  x = c(0.05), 
          y = unit(.24, "npc"),gp = gpar(fontsize=12))


