rm(list = ls())
library(tidyverse)
library(readr)
library(emmeans)
library(ggplot2)
library(multcompView)
library(lmerTest)


#read in file


plant<-read_csv('data/BiologicalTrialData1.csv')
str(plant)
plant$Plot<-factor(plant$Plot)
plant$Transect<-factor(plant$Transect)
plant$MAP_kg_ha<-factor(plant$MAP_kg_ha)
plant$Nitrogen_kg_ha<-factor(plant$Nitrogen_kg_ha)
plant$Week<-as.numeric(plant$Week)
plant$Treatment<-factor(plant$Treatment, levels=c("Untreated Control",
                                                  "Endomax 10 g/ha drench",
                             "Ultrafine Endo390 g/ha", "Ultrafine Endo780 g/ha"))
levels(plant$Treatment)

with(plant,table(Plot,Transect))
with(plant,table(MAP_kg_ha,Nitrogen_kg_ha,Treatment))

plant2<- gather(plant, key = plantNo, value = height, 9:15)
 str(plant2)

#Visulaise Data

ggplot(plant2,aes(x=Week,y=height,colour=Treatment))+
  geom_point()+
  geom_smooth(alpha=.2)

ggplot(plant2,aes(x=Week,y=height,colour=Treatment))+
  geom_point()+
  geom_smooth(alpha=.2)+
  facet_wrap(~MAP_kg_ha*Nitrogen_kg_ha)

#not a line join it with a non-linear model. model each mean

##10 mean one for each Week, 160 means, 

#model data
lm4<-lmer(height~factor(Week)*(MAP_kg_ha+Nitrogen_kg_ha+Treatment)+
            (1|Transect)+(1|Plot),data=plant2)
anova(lm4)
plot(lm4)
emmeans(lm4,pairwise~Treatment|factor(Week))

emmeans(lm4,pairwise~Nitrogen_kg_ha|factor(Week))
levels(plant2$Nitrogen_kg_ha)

#plot the means for treatment by week

results1<-summary(emmeans(lm4,~Treatment*factor(Week)))
results1$Treatment<-factor(results1$Treatment, 
                        levels=c("Untreated Control","Endomax 10 g/ha drench",
                            "Ultrafine Endo390 g/ha", "Ultrafine Endo780 g/ha"))
View(results1)

ggplot(results1, aes(x=factor(Week),y=emmean, colour=Treatment))+
  geom_point(position=position_dodge(width=.75))+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.2,
                position = position_dodge(width = .75))


