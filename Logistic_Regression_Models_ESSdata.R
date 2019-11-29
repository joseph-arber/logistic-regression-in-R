#Set working directory
rm(list = ls())
setwd("~/PUBL0055")

library(readr)
#European Social Survey dataset
ess<- read_csv("ess.csv")

#Important packages for analysis and modelling
library(tidyverse)
library(tidyr)
library(dplyr)
library(texreg)
library(foreign)

#EDA of ESS dataset
str(ess)
summary(ess)
dim(ess)
names(ess)

library(ggplot2)
ggplot(ess,aes(x = leave, y = years_education, color = unemployed)) + geom_jitter(alpha = 0.5)

#How clean is the data?
head(ess,15)
tail(ess,15)

#Are there any missing values?
sum(is.na(ess))#No missing values

#Variable coercion
#Turn trade union into a factor variable
str(ess$trade_union)
table(ess$trade_union)

ess$trade_union <-factor(ess$trade_union, levels = c(0,1), labels = c("Not-Member", "Member"))
summary(ess$trade_union)
class(ess$trade_union)

#Turn unemployed into a factor variable  
str(ess$unemployed)
table(ess$unemployed)

ess$unemployed <-factor(ess$unemployed, levels = c(FALSE,TRUE), labels =c("Employed", "Unemployed"))
summary(ess$unemployed)
class(ess$trade_union)

#Take a look at the level of country attachment  
str(ess$country_attach)
summary(ess$country_attach)

attach_country<-seq(0,10, length.out = 100)
summary(ess$country_attach)
str(ess$country_attach)

#Turn leave into a factor variable
str(ess$leave)
table(ess$leave)

ess$leave <- factor(ess$leave, levels =c(0,1), labels = c("no","yes"))
summary(ess$leave)
table(ess$leave)
str(ess$leave)


#Logistic Regression Modelliing
#Model 1
logit_M1 <- glm(leave ~ trade_union + unemployed + years_education + country_attach + eu_integration,
                data = ess, family = binomial(link = "logit"))
screenreg(logit_M1)
summary(logit_M1)

#Model 2
logit_M2 <- glm(leave ~ years_education + immig_econ + trust_politicians,
                data = ess, family = binomial(link = "logit"))
summary(logit_M2)
screenreg(logit_M2)

#Model 3
logit_M3 <- glm(leave ~ years_education + country_attach,
                data = ess, family = binomial(link = "logit"))
summary(logit_M3)
screenreg(logit_M3)

logit_M4 <- glm(leave ~ years_education + eu_integration,
                data = ess, family = binomial(link = "logit"))
summary(logit_M4)
screenreg(logit_M4)

#All values are statistically significant - publish table to word 
htmlreg(list(logit_M1),file = "LogitModel.doc")  

#FitStatistics
mean(ess$leave)
summary(ess$leave)

#Fitted Values and Predicted Probabilities    
ess$pps1 <- predict(logit_M1, newdata = ess, type = "response")
ess$evs1 <- ifelse(ess$pps1 > 0.5, yes = 1, no = 0)

#Confusion matrix to find model fit - actual outcomes
confusion <- table(actual = ess$leave, expected.value = ess$evs1)
confusion #Expected values for leave and remain 

sum(diag(confusion)) / sum(confusion) #There is an 82% success of prediction so therefore a strong model

#PredictedProbabilities:
#Likelihood to vote 'leave'; EU integration and number of years education
eu_integration_0<- predict(
  logit_M4,
  newdata = data.frame(years_education = 13, eu_integration = 0),
  type = "response"
)
eu_integration_0 #Likelihood to vote leave is 52% given education only up to age 16

eu_integration_10<- predict(
  logit_M4,
  newdata = data.frame(years_education = 20, eu_integration = 5),
  type = "response"
)
eu_integration_10 #Liklihood to vote leave is 10% given education at university level
#However, it is clear the variable, eu_integration is far a more signficant predictor in        voting leave...

#Plot1
years_education_profiles <- data.frame(
  years_education = seq(from = 0, to = 54, by = .5),
  eu_integration = 0)
head(years_education_profiles)

years_education_profiles$predicted_probs <- predict(
  logit_M4, newdata = years_education_profiles, 
  type = "response"
)

#Plot 1: Voting Leave by Years of Education:
ggplot(years_education_profiles, aes(x = years_education, y = predicted_probs)) + 
  geom_line(alpha = 0.5) + ylab("Probability of Voting Leave") + xlab("Number of Year of Education") + ggtitle("Voting Leave by Years of Education")

#Predicted probabilities of voting leave for those who a strongly attatched to their country  
country_attatchment1 <- predict(
  logit_M3,
  newdata = data.frame(country_attach = 10, years_education = 13),
  type = "response"
)
country_attatchment1

country_attatchment2 <- predict(
  logit_M3,
  newdata = data.frame(country_attach = 0, years_education = 13),
  type = "response"
)
country_attatchment2#Those less emotionally attatched more likley to vote to remain
#Difference between the predicted probabilities
country_attatchment1 - country_attatchment2

#New model with better explanatory power
logit_M5 <- glm(leave~trade_union+unemployed+years_education+
                  country_attach+eu_integration+immig_econ+immig_culture,
                data = ess, family = binomial(link = "logit"))
screenreg(logit_M5)
summary(logit_M5)

#Fit statistic for new model 
mean(ess$leave)
summary(ess$leave)

ess$pps <- predict(logit_M5, newdata = ess, type = "response")
ess$evs <- ifelse(ess$pps > 0.5, yes = 1, no = 0)
#Confusion matrix to find model fit - actual outcomes
confusion <- table(actual = ess$leave, expected.value = ess$evs)
confusion
sum(diag(confusion)) / sum(confusion) #Comparing Model 1 and 5, we can see that the new model  is slightly better.

#PredictiveProbabilities for Model 6
logit_M6 <- glm(leave~immig_econ+immig_culture,
                data = ess, family = binomial(link = "logit"))

pred_prob_1 <- predict(
  logit_M6,
  newdata = data.frame(immig_culture = 0,immig_econ = 0),
  type = "response")
pred_prob_1 #Those who are who think immigration is bad for economy have a 46% chance of voting to leave.

pred_prob_2 <- predict(
  logit_M6,
  newdata = data.frame(immig_culture = 5,immig_econ = 5),
  type = "response")
pred_prob_2 #Those who are impartial have a 17% chance of voting to leave.

pred_prob_1-pred_prob_2 #Difference of 29%
