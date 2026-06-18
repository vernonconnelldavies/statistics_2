library(tidyverse)
library(lme4)
library(sjPlot)


patient_data<-read.csv("https://raw.githubusercontent.com/vernonconnelldavies/stats_2/refs/heads/main/ST1.csv")

head(patient_data,30)

#id: patient number
#death: whether a patient is alive or dead at the end of the treatment period
#loc: the hospital where the patient was admitted
#age: the patient's age in years
#sex: the patient's biological sex
#dur: the number of days a patient was in hospital

#obviously the response variable is whether the patient dies or is alive at the end of the treatment period
#since this parameter is binary (dead/alive) that would imply some sort of logistic regression analysis


#Firstly we will look at hospitals as this seems like a significant factor, different staff operate at different hospitals and there are different co-patients at each one, in the waiting room with the patient for example, this seems like a significant factor.
#We can note abbreviations like 'Edi' and 'Gla' refer to hospitals in Scotland's two largest cities. 'QMa' would probably be Queen Margaret Hospital in Dunfermline (Kingdom of Fife)
#There are of course uniform standards enforced by the National Health Service across Scotland and the United Kingdom but in practice we would expect a large organisation such as a hospital to vary in its cleanliness, health of patients admitted due to area, efficiency, timeliness etc
#A city like Edinburgh has a much higher percentage of university graduates (source: https://www.scotlandscensus.gov.uk/census-results/at-a-glance/education/) who we would reasonably expect to earn more money, have a more healthy lifestyle, be a bit younger in age aswell.
#Edinburgh has a higher overall GDP than Glasgow despite the city of Glasgow having more people living there #https://www.statista.com/statistics/1243834/scotland-gdp-by-local-area/  #https://www.statista.com/statistics/865968/scottish-regional-population-estimates/
#Compare this to a city like Glasgow which has among the lowest life expectancy rates for an area in the United Kingdom.
#https://www.bbc.co.uk/news/articles/cj3016nngrro
#Glasgow also has a drug death rate far higher (over double by the recent data see information source #https://www.gov.scot/publications/suspected-drug-deaths-scotland-january-march-2024/) than Edinburgh (though of course Greater Glasgow is a larger city than Edinburgh).
#Glasgow also has a higher smoking rate in 2018 at 19.1% while Edinburgh had a smoking rate of 10.8% #https://www.scotsman.com/health/the-three-scottish-areas-in-the-top-10-worst-for-smoking-in-the-uk-4805401
#The Obesity rate of Glasgow is higher than that of Edinburgh, Glasgow is 61% and Edinburgh is 51% #https://www.gov.scot/publications/scottish-health-survey-results-local-areas-2014-2015-2016-2017/pages/4/
#It's reasonable to expect that any patient admitted into a Glasgow hospital will be in contact with an average co patient of lower life expectancy, higher chance of drug use, higher chance of smoking, higher chance of being obese and just a generally weaker immune system as a result.
#It's reasonable to expect someone with a university education will more likely take a more proactive approach to their health, discussing with nurse/general practitioner friends, approaching the GP more proactively when suspecting of a problem, reading health magazines, discussing health problems with educated friends etc.
#Of course hospitals in places like Livingston and Hamilton and Dunfermline will have different parameters again, I focused on Glasgow and Edinburgh just to make a point
#Although that's not to say that the staff at Glasgow hospitals could be significantly better (harder working, more experienced, more intelligent and more enlightened to latest methods of medicine and health) than the staff at the Edinburgh hospitals and this factor could make up the difference to the co-patient factor (when the patient spends time in the waiting room with other patients)

#another point is that the duration of treatment period might be affected by which hospital a patient is in, since different hospitals have different staff and therefore different amounts of efficiency
#another point is that a younger patient will most likely recover from whatever illness faster than an elderley patient for example, so age is related to duration possibly

patient_data_by_location<-patient_data %>%
  group_by(loc) %>%
  count(loc) %>%
  mutate(total_number_of_patients=n)

head(patient_data_by_location) #here we can see we have 30 patient data points at each of the hospitals, except Edinburgh which has 60


patient_survival_rate_by_location<-patient_data %>%
  group_by(loc) %>%
  filter(death=="no") %>%
  count() %>%
  mutate(number_of_survived=n) %>%
  inner_join(patient_data_by_location,patient_surival_rate_by_location,by=join_by(loc)) %>%
  mutate(survival_rate=number_of_survived/total_number_of_patients) %>%
  ggplot(aes(reorder(loc,survival_rate),survival_rate))+geom_col(fill='lightblue')+#the reorder part was taken from code in skills test 1
  ggtitle('survival_rate_by_location')+
  coord_flip()
  
patient_survival_rate_by_location

#'Liv' has the best survival rate, with 'Ham' being the worst


#now we will copy and paste the same code for sex

patient_data_by_sex<-patient_data %>%
  group_by(sex) %>%
  count(sex) %>%
  mutate(total_number_of_patients=n)

patient_survival_rate_by_sex<-patient_data %>%
  group_by(sex) %>%
  filter(death=="no") %>%
  count() %>%
  mutate(number_of_survived=n) %>%
  inner_join(patient_data_by_sex,patient_surival_rate_by_sex,by=join_by(sex)) %>%
  mutate(survival_rate=number_of_survived/total_number_of_patients) %>%
  ggplot(aes(reorder(sex,survival_rate),survival_rate))+geom_col(fill='deeppink1')+#the reorder part was taken from code in skills test 1
  ggtitle('survival_rate_by_sex')+
  coord_flip()

patient_survival_rate_by_sex

#males have a better survival rate than females

#now we will look at duration, how does time spent in hospital affect the chances of whether a patient will survive the treatment or not?

ggplot(patient_data,aes(dur,fill=death))+geom_histogram()+ggtitle('histogram of duration of treatment')

#how does age affect the chance of death?

ggplot(patient_data,aes(age,fill=death))+geom_histogram()+ggtitle('histogram of age')

#Average age of deceased persons? Average time of treatment period of deceased person?

patient_data %>%
  filter(death=='yes') %>%
  summary()

#we can see our data has more deaths on the right side where the age is higher, which is what we would expect, younger people generally have stronger immune systems

#so our average age of the deceased persons is 58 (the median is 70) and the average time in hospital being 6.6 days (the median being 6)
  




#as for parameters, patient number seems fairly independent (its just someone assigning each person a number) so we will just ignore this parameter for our regression, of course its still useful as a reference column in our data
#we know female human beings typically have a longer life expectancy in general #https://ourworldindata.org/why-do-women-live-longer-than-men


str(patient_data)

patient_data$death<-as.factor(patient_data$death)

summary(patient_data)#death is now a factor but our levels are the wrong way around, we want 'no' to be 1 and 'yes' to be 0
str(patient_data)
head(patient_data)

patient_data$death<-factor(patient_data$death,levels=c("yes","no"))#https://stackoverflow.com/questions/47962255/change-factor-levels-to-custom-order-of-a-column
str(patient_data) #we can see now 'yes' is our zero value and 'no' is our one value which makes sense as patients not being dead is a better outcome than being dead 


#since our death rate for females is higher than our males we will assign factor levels where 'female' is zero and 'male' is one

patient_data$sex<-as.factor(patient_data$sex)
str(patient_data) #we can see from our output the factor levels is the way we want it, 'female' is at one and 'male' is at two


model_1<-glm(death~dur+age+sex,data=patient_data,family="binomial")#we use family="binomial" as our dependent variable is a binary output (there are only 2 outputs, dead or not dead, 0 or 1)
summary(model_1)#so we can see our 'dur' is statistically significant, but both 'age' and 'sex' are not statistically significant as the P value is above 0.05


#we can see here our adjusted R squared value is very low, it should be close to one for a good model, 
#we will ignore patient_id, its only a series of reference numbers basically
#from the week 3 notes: The difference is that the marginal R2 reflects the variance of the fixed effects only, whereas the conditional R2 reflects both the fixed and random effects

#now we have tried linear models with no luck we will move to multilevel models, different hospitals have different patients in them and different staff, obviously Paula uses 'species' in her 'Palmer Penguins' example lecture and also she hints heavily in her lecture that 'hospitals' and 'patients' will be used in the same way in one of her lectures
#finally, Paula also says the random effects are usually categorical variables as opposed to continuous, so 'loc' is a pretty obvious contender for one of our random effects.

model_2<-glmer(death~0+dur+(1|loc),data=patient_data,family="binomial")
tab_model(model_1,model_2)
isSingular(model_2)#we are not getting the Singular error upon adding the '+0' part

model_3<-glmer(death~0+dur*age+(1|loc),data=patient_data,family="binomial")
tab_model(model_1,model_2,model_3)#so we can see our Conditional R Squared value is 0.81 which is very good, this includes both random and fixed effects as stated previously
isSingular(model_3)
anova(model_2,model_3)#so our model 3 p value is less than 0.05 and so is statistically significant


#let's try some predictions with fabricated data

predict_data_1<-tibble(loc='Gla',age=32,sex='male',dur=3)
predict(model_3,newdata=predict_data_1,type='response')

predict_data_2<-tibble(loc='Liv',age=32,sex='male',dur=3)
predict(model_3,newdata=predict_data_2,type='response')#this result is what we would expect (a higher value than the Glasgow one) since Livingston has a better survival rate than Glasgow hospital, 

predict_data_3<-tibble(loc='Gla',age=72,sex='male',dur=3)
predict(model_3,newdata=predict_data_3,type='response')#in this one we compare to the 32 year old in Glasgow hospital and as we would expect, the 72 year old has a lower survival rate

predict_data_4<-tibble(loc='Gla',age=32,sex='female',dur=3)
predict(model_3,newdata=predict_data_4,type='response')#so our female and male in Glasgow both have the same survival rate which is slightly unexpected, from our original patient_survival_rate_by_sex we would expect the female to have a slightly worse survival rate, however 'sex' is not included in our model so perhaps this is the reason why it has not changed. Possibly our model is not entirely perfect at this stage still.

predict_data_5<-tibble(loc='Gla',age=32,sex='male',dur=12)
predict(model_3,newdata=predict_data_5,type='response')#again, this is what we expect, the survival rate of the patient who is in hospital in Glasgow for 3 days is much better than that of the patient who is in hospital for 12 days



model_4<-glmer(death~0+dur*age+(dur|loc),data=patient_data,family="binomial")
tab_model(model_1,model_2,model_3,model_4)#so model 4 has a more balanced marginal vs conditional R squared value, 0.263 vs 0.775, however the model 3 has the higher overall conditional R squared at 0.808
isSingular(model_4)
anova(model_2,model_3,model_4)

#let's try some more predictions this time with model_4

predict_data_6<-tibble(loc='Gla',age=32,sex='male',dur=3)
predict(model_4,newdata=predict_data_6,type='response')

predict_data_7<-tibble(loc='Liv',age=32,sex='male',dur=3)
predict(model_4,newdata=predict_data_7,type='response')#now this result doesn't make sense compared to our survival rate by hospital plot earlier, Livingston should have a higher survival rate which brings into question model_4

predict_data_8<-tibble(loc='Gla',age=72,sex='male',dur=3)
predict(model_4,newdata=predict_data_8,type='response')#this seems ok, it lowers from the 32 year old to the 72 year old

predict_data_9<-tibble(loc='Gla',age=32,sex='female',dur=3)
predict(model_4,newdata=predict_data_9,type='response')#no change, as per model_3

predict_data_10<-tibble(loc='Gla',age=32,sex='male',dur=12)
predict(model_4,newdata=predict_data_10,type='response')#increasing the duration from 3 to 12 days decreases survival rate as expected


#now we will see if we can include 'sex' to our model without causing singularity

model_5<-glmer(death~0+dur*age+sex+(1|loc),data=patient_data,family="binomial")
tab_model(model_1,model_2,model_3,model_4,model_5)#so model 4 has a more balanced marginal vs conditional R squared value, 0.263 vs 0.775, however the model 3 has the higher overall conditional R squared at 0.808
isSingular(model_5)
anova(model_2,model_3,model_4,model_5)

#since our isSingular output is coming up 'TRUE' we know we cannot add 'sex' without causing singularity. So I will leave out 'sex' from our regression model, model_5 is not a good model

#Overall this submission used trial and error and experimentation to find a correct multilevel regression model mainly, earlier attempts included frustration with the singularity error and problems with the final prediction results, however model_3 largely seems to work in this final submission, based on the prediction results.
#I note that my model_3 does have an error warning message and asks me to rescale my variables, unfortunately I have run out of time but I would look at this if I was to do this assignment again.