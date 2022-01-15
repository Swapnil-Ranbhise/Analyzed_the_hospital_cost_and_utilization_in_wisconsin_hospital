setwd("C:/Users/ranbh/OneDrive/Desktop/simplilearn data analytics course/R programming language for data science by simplilearn")

hospitalcost<- read.csv("HospitalCosts.csv")

View(hospitalcost)
head(hospitalcost)

str(hospitalcost)
summary(hospitalcost)



library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(caTools)
library(MLmetrics)
library(reshape2)

### Age category of people who friquently hospitalised ###

qplot(hospitalcost$AGE,
       geom = "histogram",
       binwidth = 1,
       colour = I("black"),
       fill = I("red"),
       xlab = "Age",
       ylab = "Record of petient",
       main = "HISTOGRAM FOR HOSPITALIZED PETIENT")

summary(as.factor(hospitalcost$AGE))

### WHICH AGE CATEGORY HAS HIGHEST HOSPITAL TOTAL EXPENDITURE ###

ggplot(hospitalcost, aes(x= hospitalcost$AGE, y= sum(hospitalcost$TOTCHG),
                         fill = sum(hospitalcost$TOTCHG))) +
   geom_bar(stat = "identity") +
   labs(x= "Age", y= "Total expenditure") +
   ggtitle("BAR CHART FOR AGE AND TOTAAL EXPENDITURE")


Total_expenditure_of_age_group<- group_by(hospitalcost, AGE)
TEOAG<-summarise(Total_expenditure_of_age_group, sum(TOTCHG, na.rm = TRUE))
View(TEOAG)
which.max(TEOAG$`sum(TOTCHG, na.rm = TRUE)`)
TEOAG
TEOAG[1,]


### which diagnosis related group has maximum record of hspitalization##

qplot(hospitalcost$APRDRG,
      geom = "histogram",
      xlab = "APRDRG",
      ylab = "record of petient",
      colour = I("black"),
      fill = I("red"),
      main = "HISTOGRAM FOR APRDRG HOSPITALIZATION")

summary(as.factor(hospitalcost$APRDRG))



### which APRDRG has highest expenditure ###

ggplot(hospitalcost, aes(x= hospitalcost$APRDRG, y= sum(hospitalcost$TOTCHG),
       fill = sum(hospitalcost$TOTCHG))) +
   geom_bar(stat = "identity") +
   labs(x= "APRDRG", y= "Total expenditure")

Total_expenditure_of_APRDRG<- group_by(hospitalcost, APRDRG)
TEOA<-summarise(Total_expenditure_of_APRDRG, sum(TOTCHG, na.rm = TRUE))
View(TEOA)
head(TEOA)
tail(TEOA)
which.max(TEOA$`sum(TOTCHG, na.rm = TRUE)`)
TEOA[44,]


#### find if the race of patient is related to hospital cost and 
#### analyze is there any malpractic amoung race


levels( as.factor(hospitalcost$RACE))

unique(hospitalcost$RACE)

group_by(hospitalcost, RACE)%>%
   summarise(
      count = n(),
      mean = mean(TOTCHG, na.rm = TRUE),
      sd = sd(TOTCHG, na.rm = TRUE)
   )


ggplot(hospitalcost, aes(x= hospitalcost$RACE, y= hospitalcost$TOTCHG)) +
   geom_boxplot() +
   labs(x= "Race", y= "TOTCHG")


aov_11<- aov(TOTCHG ~ RACE, data = hospitalcost)
aov_11
summary(aov_11)

p_aov<-plot(aov_11)

p_2<-plot(aov_11, 2)

boxplot(TOTCHG ~ RACE, data = hospitalcost)




#### find the severity of hospital cost by age and genser 
#### for allocation of resources 

gender<- as.factor(hospitalcost$FEMALE)
dummy_gender<- data.frame(model.matrix(~gender))
table(gender)
hospitalcost<- cbind(hospitalcost, dummy_gender)
names(hospitalcost)
na.omit(hospitalcost)
nrow(hospitalcost)

### 0 = male
### 1 = female

regression_of_categorical_variable<- lm(data = hospitalcost, TOTCHG ~ AGE + gender)

regression_of_categorical_variable

summary(regression_of_categorical_variable)

Spltr<-plot(regression_of_categorical_variable)
pltr

anova(regression_of_categorical_variable)
   

TOTCHG= 2718.63+86.28*(17)+-748.19
TOTCHG
TOTCHG= 2718.63+86.28*(17)
TOTCHG
TOTCHG= 2718.63+86.28*(hospitalcost$AGE)+-748.19
TOTCHG
TOTCHG_M<- 2718.63+86.28*(hospitalcost$AGE)
TOTCHG_M

GENDER<- if(hospitalcost$FEMALE==1){
   print(-748.19)
}else{
   print(0)
}
GENDER

pred_TOTCHG<-data.frame(TOTCHG)
final_df<-cbind.data.frame(hospitalcost, pred_TOTCHG)
view(final_df)


pairs(hospitalcost[,1:6])

round(cor(hospitalcost, method = "pearson"),2)



### predict the length of stay by using age gender and race ###

hospitalcost_2<- data.frame(hospitalcost[,1:4])
ncol(hospitalcost_2)

split = sample.split(hospitalcost_2$LOS, SplitRatio = 0.7)
training_df<- subset(hospitalcost_2, split == TRUE)
test_df<- subset(hospitalcost_2, split == FALSE)

nrow(training_df)
nrow(test_df)

round(cor(training_df),2)

ggplot(training_df, aes(x= AGE, y= LOS)) + geom_point() + geom_smooth(method = "lm")

ggplot(training_df, aes(x= FEMALE, y= LOS)) +geom_point() + geom_smooth(method = "lm")

ggplot(training_df, aes(x= RACE, y= LOS)) +geom_point() + geom_smooth(method = "lm")


model_11<- lm(data= training_df, LOS~AGE+FEMALE+RACE)
summary(model_11)

prediction<- predict(model_11, newdata = test_df)

prediction
summary(prediction)
pred_df<- data.frame(prediction)
pred_df
nrow(pred_df)



mod_11<-lm(data= hospitalcost_2, LOS~AGE+FEMALE+RACE)
summary(mod_11)

pred_LOS<-predict(mod_11, newdata = hospitalcost_2)

pred_los<-data.frame(pred_LOS)

nrow(pred_los)

final_df<- cbind.data.frame(hospitalcost, pred_los)

view(final_df)

head(final_df)

tail(final_df)


### which veriable is most effecting the totchg ###

cor_df<-round(cor(hospitalcost, method = "pearson"),2)
cor_df
melt_cor_df<- melt(cor_df)
head(melt_cor_df)

ggplot(data = melt_cor_df, aes(x= Var1, y= Var2, fill = value))+
   geom_tile()

model_12<- lm(data = hospitalcost, TOTCHG~AGE+FEMALE+LOS+RACE+APRDRG)
model_12
summary(model_12)

install.packages("GGally")
library(GGally)
ggpairs(hospitalcost)

