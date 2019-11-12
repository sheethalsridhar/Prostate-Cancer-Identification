library(readr)
cancer <- read_csv("Downloads/participant_files/participant_files/training_data.csv")
View(cancer)
#cancer is the actual training data 

data<-cancer
cancer<-data
#copy of the actual data

str(cancer)
summary(cancer)

#Grouping observations of the variables into different categories
cancer$age<- ifelse(cancer$age <= 45, 1, ifelse(cancer$age <= 65, 2, ifelse(cancer$age<=85,3,4)))
cancer$tea<- ifelse(cancer$tea==0,0,ifelse(cancer$tea<2,1,ifelse(cancer$tea<4,2,3)))

summary(cancer$age)
summary(cancer$tea)
cancer$survival_1_year<-as.factor(cancer$survival_1_year)
cols<-c("stage","race","family_history","first_degree_history","previous_cancer","smoker"
        ,"side","rd_thrpy","h_thrpy","chm_thrpy","cry_thrpy","brch_thrpy","rad_rem","multi_thrpy","survival_7_years")
cancer[,cols] <-  data.frame(apply(cancer[cols],2, as.factor))
str(cancer)
#d<-as.Date(cancer$diagnosis_date)

cancer$gleason_score<-ifelse(cancer$gleason_score<=6,1,
                             ifelse(cancer$gleason_score==7,2,ifelse(cancer$gleason_score==8,3,ifelse(cancer$gleason_score<=10,4,5))))
#cancer$gleason_score<-ifelse(cancer$gleason_score==3||cancer$gleason_score==4||cancer$gleason_score==5||cancer$gleason_score==6,1,
#ifelse(cancer$gleason_score==7,2,ifelse(cancer$gleason_score==8,3,ifelse(cancer$gleason_score==9||cancer$gleason_score==10,4,5))))

cancer$t_score<-as.factor(cancer$t_score)
cancer$n_score<-as.factor(cancer$n_score)
cancer$m_score<-as.factor(cancer$m_score)
cancer$age<-as.factor(cancer$age)
cancer$gleason_score<-as.factor(cancer$gleason_score)
summary(cancer)
str(cancer)

temp<-cancer
cancer<-cancer[!is.na(cancer$gleason_score),]
cancer<-cancer[!is.na(cancer$t_score),]
cancer<-cancer[!is.na(cancer$n_score),]
cancer<-cancer[!is.na(cancer$m_score),]
cancer<-cancer[!is.na(cancer$age),]
cancer<-cancer[!is.na(cancer$race),]
cancer<-cancer[!is.na(cancer$family_history),]
cancer<-cancer[!is.na(cancer$first_degree_history),]
cancer<-cancer[!is.na(cancer$previous_cancer),]
cancer<-cancer[!is.na(cancer$smoker),]
cancer<-cancer[!is.na(cancer$side),]
cancer<-cancer[!is.na(cancer$tumor_diagnosis),]
cancer<-cancer[!is.na(cancer$psa_diagnosis),]
cancer<-cancer[!is.na(cancer$psa_1_year),]
cancer<-cancer[!is.na(cancer$symptoms),]
cancer<-cancer[!is.na(cancer$rd_thrpy),]
cancer<-cancer[!is.na(cancer$h_thrpy),]
cancer<-cancer[!is.na(cancer$chm_thrpy),]
cancer<-cancer[!is.na(cancer$cry_thrpy),]
cancer<-cancer[!is.na(cancer$brch_thrpy),]
cancer<-cancer[!is.na(cancer$rad_rem),]
cancer<-cancer[!is.na(cancer$multi_thrpy),]
cancer<-cancer[!is.na(cancer$survival_1_year),]
cancer<-cancer[!is.na(cancer$survival_7_years),]
cancer<-cancer[!is.na(cancer$tumor_1_year),]
cancer<-cancer[!is.na(cancer$tea),]
str(cancer)

library(tidyverse)
library(stringr)
library(splitstackshape)
library(dplyr)
#t<-cancer
#t$symp<-str_count(t$symptoms)
#t$results<-t%>%separate("symptoms",t,",")
#t <-t %>% cSplit("symptoms", ",")
#t$symptoms<-cancer$symptoms
#t$word<-wordcount(t$symptoms)
#t$word<-str_count(t$symptoms,",")+1
library(ngram)

#t$symptoms<-as.factor(t$symptoms)

cancer$n_symp<-str_count(cancer$symptoms,",")+1

str(cancer)
cancer$n_symp<-as.factor(cancer$n_symp)
#Univariate Analysis
cancer1<-cancer
cancer1$tumor_6_months<-NULL
cancer1$psa_6_months<-NULL
is.na(cancer1)
install.packages("writexl")
library(writexl)
write_xlsx(x=cancer1,path="cancer1.xlsx",col_names=TRUE)
is.na(cancer1$family_history)

cancer_final<-cancer
cancer$tea<-as.factor(cancer$tea)


##########################
##########################

#Anova Testing- Numeric vs Factor

cancer.aov <- aov(cancer$height~cancer$survival_7_years, data=cancer)
cancer.aov
summary(cancer.aov) # Null accepted....not important

cancer.aov <- aov(cancer$weight~cancer$survival_7_years, data=cancer)
cancer.aov
summary(cancer.aov)# Null accepted....not important

cancer.aov <- aov(cancer$tumor_diagnosis~cancer$survival_7_years, data=cancer)
cancer.aov
summary(cancer.aov)# Null rejected.... important

cancer.aov <- aov(cancer$tumor_1_year~cancer$survival_7_years, data=cancer)
cancer.aov
summary(cancer.aov)# Null rejected.... important

cancer.aov <- aov(cancer$psa_diagnosis~cancer$survival_7_years, data=cancer)
cancer.aov
summary(cancer.aov)# Null rejected.... important

cancer.aov <- aov(cancer$psa_1_year~cancer$survival_7_years, data=cancer)
cancer.aov
summary(cancer.aov)# Null rejected.... important

######################
######################

chisq.test(cancer$gleason_score,cancer$survival_7_years,correct = FALSE)# significant
chisq.test(cancer$t_score,cancer$survival_7_years,correct = FALSE)#significant
chisq.test(cancer$n_score,cancer$survival_7_years,correct = FALSE)
chisq.test(cancer$m_score,cancer$survival_7_years,correct = FALSE)
chisq.test(cancer$stage,cancer$survival_7_years,correct = FALSE)
chisq.test(cancer$age,cancer$survival_7_years,correct = FALSE)#NOT significant
chisq.test(cancer$race,cancer$survival_7_years,correct = FALSE)#NOT significant
chisq.test(cancer$family_history,cancer$survival_7_years,correct = FALSE)
chisq.test(cancer$first_degree_history,cancer$survival_7_years,correct = FALSE)
chisq.test(cancer$previous_cancer,cancer$survival_7_years,correct = FALSE)#NOT significant
chisq.test(cancer$smoker,cancer$survival_7_years,correct = FALSE)#NOT much
chisq.test(cancer$side,cancer$survival_7_years,correct = FALSE)#NOT significant
chisq.test(cancer$tea,cancer$survival_7_years,correct = FALSE)#NOT significant
chisq.test(cancer$rd_thrpy,cancer$survival_7_years,correct = FALSE)
chisq.test(cancer$h_thrpy,cancer$survival_7_years,correct = FALSE)#NOT significant
chisq.test(cancer$chm_thrpy,cancer$survival_7_years,correct = FALSE)
chisq.test(cancer$cry_thrpy,cancer$survival_7_years,correct = FALSE)
chisq.test(cancer$brch_thrpy,cancer$survival_7_years,correct = FALSE)
chisq.test(cancer$rad_rem,cancer$survival_7_years,correct = FALSE)#NOT significant
chisq.test(cancer$multi_thrpy,cancer$survival_7_years,correct = FALSE)
chisq.test(cancer$n_symp,cancer$survival_7_years,correct = FALSE)

######################
######################
CrossTable(cancer$rd_thrpy,cancer$h_thrpy)
CrossTable(cancer$rd_thrpy,cancer$h_thrpy)

######################
######################
library(car) # for detailed correlation plot 
library(corrplot) # for correlation plot
library(Hmisc)
library(gmodels)
library(caret)
cnum <- cancer[,c("height", "weight", "tumor_diagnosis","tumor_1_year","psa_diagnosis","psa_1_year")]
cormat <- cor(cnum) # Select only numeric variables. Otherwise, you'd get 
pairs(cnum)
#scatterplotMatrix(~Age+Weight+Height+BMI+Waist+SBP+DBP+HDL+LDL+`Total Chol`, data=chronic, main="Correlations of Numeric Variables in the Cars Data")
corrplot(cormat, method="circle")
corrplot(cormat, method="circle", addCoef.col="black") # With correlation 

#####################
#####################
attach(cancer)
train<-cancer
val<-train[6301:8955,]
training<-train[1:6300,]
l1<-glm(survival_7_years~tumor_diagnosis+tumor_1_year+psa_diagnosis+psa_1_year+gleason_score+
          n_score+m_score+stage+rd_thrpy+chm_thrpy+brch_thrpy+multi_thrpy+n_symp+race+smoker+
          rad_rem,data = training,family =binomial(link="logit"))
summary(l1)
exp(coef(l1))
ci(l1)
options(scipen = 999)

l1<-glm(survival_7_years~tumor_1_year+gleason_score+
          n_score+m_score+stage+rd_thrpy+chm_thrpy+brch_thrpy+multi_thrpy+race+smoker+
          rad_rem,data = training,family =binomial(link="logit"))
summary(l1)

lm<-glm(survival_7_years~t_score+age+tea+weight+tumor_diagnosis+tumor_1_year+psa_diagnosis+psa_1_year+gleason_score+
          n_score+m_score+stage+h_thrpy+cry_thrpy+rd_thrpy+chm_thrpy+brch_thrpy+multi_thrpy+n_symp+race+smoker+
          rad_rem,data = training,family =binomial(link="logit"))
summary(lm)


predTrain<-predict(l1, newdata=val[,-33],type = "response")
results <- ifelse(predTrain > 0.5,1,0)
results <-as.factor(results)
levels(results) <- c("0", "1")
#val$survival_7_years<-as.factor(val$survival_7_years)
#levels(val$CKD) <- c("No CKD", "CKD")
#val$CKD <- as.integer(val$CKD)
library(caret)
summary(results)
install.packages("e1071")
library(e1071)
confusionMatrix(results,val$survival_7_years,positive = "1")

###########
#########################################
library(readr)
test <- read_csv("Downloads/participant_files/participant_files/(name)_score.csv")
str(test_initial)
summary(test_initial)
View(test)

test$age<- ifelse(test$age <= 45, 1, ifelse(test$age <= 65, 2, ifelse(test$age<=85,3,4)))
test$tea<- ifelse(test$tea==0,0,ifelse(test$tea<2,1,ifelse(test$tea<4,2,3)))

test$survival_1_year<-as.factor(test$survival_1_year)
cols<-c("stage","race","family_history","first_degree_history","previous_cancer","smoker"
        ,"side","rd_thrpy","h_thrpy","chm_thrpy","cry_thrpy","brch_thrpy","rad_rem","multi_thrpy","survival_7_years")
test[,cols] <-  data.frame(apply(test[cols],2, as.factor))

test$gleason_score<-ifelse(test$gleason_score<=6,1,
                             ifelse(test$gleason_score==7,2,ifelse(test$gleason_score==8,3,ifelse(test$gleason_score<=10,4,5))))

test$t_score<-as.factor(test$t_score)
test$n_score<-as.factor(test$n_score)
test$m_score<-as.factor(test$m_score)
test$age<-as.factor(test$age)
test$gleason_score<-as.factor(test$gleason_score)

test<-test[!is.na(test$gleason_score),]
#test<-test[!is.na(test$t_score),]
test<-test[!is.na(test$n_score),]
test<-test[!is.na(test$m_score),]
test<-test[!is.na(test$stage),]
#test<-test[!is.na(test$age),]
test<-test[!is.na(test$race),]
#test<-test[!is.na(test$family_history),]
#test<-test[!is.na(test$first_degree_history),]
#test<-test[!is.na(test$previous_cancer),]
test<-test[!is.na(test$smoker),]
#test<-test[!is.na(test$side),]
test<-test[!is.na(test$tumor_diagnosis),]
test<-test[!is.na(test$psa_diagnosis),]
test<-test[!is.na(test$psa_1_year),]
#test<-test[!is.na(test$symptoms),]
test<-test[!is.na(test$rd_thrpy),]
#test<-test[!is.na(test$h_thrpy),]
test<-test[!is.na(test$chm_thrpy),]
test<-test[!is.na(test$cry_thrpy),]
test<-test[!is.na(test$brch_thrpy),]
test<-test[!is.na(test$rad_rem),]
test<-test[!is.na(test$multi_thrpy),]
test<-test[!is.na(test$symptoms),]
#test<-test[!is.na(test$survival_1_year),]
test<-test[!is.na(test$tumor_1_year),]
#test<-test[!is.na(test$tea),]

test$n_symp<-str_count(test$symptoms,",")+1

str(test)
test$n_symp<-as.factor(test$n_symp)
test$tea<-as.factor(test$tea)

predTest<-predict(object=l1,newdata = test, type="response")
results_test<-ifelse(predTest > 0.5,1,0)
results_test<-as.factor(results_test)
levels(results_test)<-c("0","1")
summary(results_test)
test$results<-results_test
levels(results)
levels(val$survival_7_years)
typeof(results)
typeof(val$survival_7_years)


play<-cancer
play$psa_diff<-play$psa_diagnosis-play$psa_1_year
hist(play$psa_diff, col=c("steelblue", "red"), freq=F) 
rug(jitter(play$psa_diff), col="darkgray")
lines(density(play$psa_diff), col="yellow", lwd=3) # Note how the lines function is used to overlay the density plot 
box()

write_xlsx(x=test,path="test_pc.xlsx",col_names=TRUE)
