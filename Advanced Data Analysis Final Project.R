#retreive data from file location
setwd("C:/Users/apfit/OneDrive/Documents/Depaul/Spring 2022/Advanced Data Analysis/R datasources")
WHO = read.csv('who_life_exp.csv')

#display summary of data
summary(WHO)
WHO_Summary= WHO%>%
  group_by(country)%>%
  summarise(Mean_Life_Expectancy = mean(life_expect))%>%
  arrange(desc(Mean_Life_Expectancy))

#determine which columns have 'NA' values
colSums(is.na(WHO))

#review alcohol NAs
WHO%>%
  filter(is.na(alcohol))%>%
  group_by(country)%>%
  summarise()

WHO%>%
  filter(country=='Canada')%>%
  select(country, alcohol)

WHO%>%
  filter(country=='Afghanistan')%>%
  select(country, alcohol)

WHO%>%
  filter(country=='Montenegro')%>%
  select(country, alcohol)

WHO%>%
  filter(country=='Serbia')%>%
  select(country, alcohol)

WHO%>%
  filter(country=='Sudan')%>%
  select(country, alcohol)

WHO%>%
  filter(country=='South Sudan')%>%
  select(country, alcohol)

#replace alcohol NAs with avg of 'good' values by country 
#(remove south sudan from data set due to no data on many predictors)

x='Canada'
WHO$alcohol = ifelse(WHO$country== x & is.na(WHO$alcohol),
                     mean(WHO[WHO$country == x ,]$alcohol,na.rm=TRUE),WHO$alcohol)

x='Afghanistan'
WHO$alcohol = ifelse(WHO$country== x & is.na(WHO$alcohol),mean(WHO[WHO$country == x ,]$alcohol,na.rm=TRUE),WHO$alcohol)

x='Montenegro'
WHO$alcohol = ifelse(WHO$country== x & is.na(WHO$alcohol),mean(WHO[WHO$country == x ,]$alcohol,na.rm=TRUE),WHO$alcohol)

x='Serbia'
WHO$alcohol = ifelse(WHO$country== x & is.na(WHO$alcohol),mean(WHO[WHO$country == x ,]$alcohol,na.rm=TRUE),WHO$alcohol)

x='Sudan'
WHO$alcohol = ifelse(WHO$country== x & is.na(WHO$alcohol),mean(WHO[WHO$country == x ,]$alcohol,na.rm=TRUE),WHO$alcohol)

#remove south sudan
WHO = WHO%>%
  filter(country!='South Sudan')

#REVIEW BMI NAs
WHO%>%
  filter(is.na(bmi))%>%
  group_by(country)%>%
  summarise()

WHO%>%
  filter(country=='Sudan')%>%
  select(country, bmi)

#remove Sudan due to no data present in bmi

WHO = WHO%>%
  filter(country!='Sudan')

#reduce data set to remove variables with significant missing values across all entries
colSums(is.na(WHO))

WHO_Reduced = WHO%>%
  select(-une_school, -une_literacy, -une_edu_spend, -une_poverty, -hospitals,-doctors)

#reduce selection further by only reviewing complete entries in the reduced data set 
WHO_Reduced_CompleteCases = WHO_Reduced[complete.cases(WHO_Reduced),]
nrow(WHO_Reduced_CompleteCases)

WHO_Summary_Reduced= WHO_Reduced_CompleteCases%>%
  group_by(country)%>%
  summarise(Mean_Life_Expectancy = mean(life_expect))%>%
  arrange(desc(Mean_Life_Expectancy))

#correlation plot on reduced, complete case data set
library(corrplot)

WHOcor = cor(WHO_Reduced_CompleteCases%>%
               select(-country,-country_code,-region))
corrplot(WHOcor,order='hclust',addrect=2)

pairs(WHO_Reduced_CompleteCases%>%
        select(-country,-country_code,-region))

#export reduced data frame with complete cases
write.csv(WHO_Reduced_CompleteCases,"C:/Users/apfit/OneDrive/Documents/Depaul/Spring 2022/Advanced Data Analysis/R datasources/who_life_exp_reduced.csv", row.names = FALSE)

#remove multicolinearity from dataset
library(tidyverse)
library(dplyr)
WHO_multicolinearity_removed = WHO_Reduced_CompleteCases%>%
  select(country,region,year,life_expect,adult_mortality,infant_mort,age1.4mort,alcohol,bmi,age5.19thinness,age5.19obesity,hepatitis,measles,polio,diphtheria,basic_water,gni_capita,gghe.d,che_gdp,une_pop,une_hiv)

#review year data
summary(as.factor(WHO_multicolinearity_removed$year))

#LDA
library(MASS)
library(DAAG)
library(ggplot2)

#remove unused variables and select year from dataframe
WHO_LDA_Data = WHO_multicolinearity_removed%>%
  filter(year=='2013')%>%
  dplyr::select(-country,-year)

WHO_LDA_Data1 = WHO_multicolinearity_removed%>%
  filter(year=='2013')%>%
  dplyr::select(-year)

#generate eploratory LDA object
WHO.LDA = lda(region~., data=WHO_LDA_Data)
WHO.LDA.Values = predict(WHO.LDA)
WHO.LDA
WHO.LDA.Values

LD1 = WHO.LDA.Values$x[,1]
LD2 = WHO.LDA.Values$x[,2]
predRegion =WHO.LDA.Values$class
actualRegion = WHO_LDA_Data1$region
Country = WHO_LDA_Data1$country
WHO.LDA.Plot = data.frame(LD1,LD2,predRegion,actualRegion,Country)

ggplot(WHO.LDA.Plot,aes(x = LD1,y=LD2,col=actualRegion))+geom_point()+
  ggtitle('Plot of LD1 and LD2, Categorized by Region')+theme_bw()+geom_text(label=Country)


#normalize data
WHO_LDA_Data1_normalized = as.data.frame(scale(WHO_LDA_Data1%>%
                                                 dplyr::select(-country,-region)))

#add data labels
WHO_LDA_Data1_normalized$country=WHO_LDA_Data1$country
WHO_LDA_Data1_normalized$region=WHO_LDA_Data1$region

#pull all countries of 'non interest' into separate table
WHO_LDA_Data1_normalized_other = WHO_LDA_Data1_normalized%>%
  filter(country!='Bangladesh'&country!='Pakistan'&country!='Bhutan'&country!='Sri Lanka'&country!='Afghanistan'&country!='Yemen')

#transform 'other' data for graphing
WHO_other = stack(WHO_LDA_Data1_normalized_other%>%
                    dplyr::select(-country,-region))
WHO_other$country = 'other'

#create individual tables for countries of interest
WHO_interest1 = WHO_LDA_Data1_normalized%>%
  filter(country=='Bangladesh')%>%
  dplyr::select(-country,-region)%>%
  stack()%>%
  mutate(country='Bangladesh')

WHO_interest2 = WHO_LDA_Data1_normalized%>%
  filter(country=='Pakistan')%>%
  dplyr::select(-country,-region)%>%
  stack()%>%
  mutate(country='Pakistan')

WHO_interest3 = WHO_LDA_Data1_normalized%>%
  filter(country=='Bhutan')%>%
  dplyr::select(-country,-region)%>%
  stack()%>%
  mutate(country='Bhutan')

WHO_interest4 = WHO_LDA_Data1_normalized%>%
  filter(country=='Sri Lanka')%>%
  dplyr::select(-country,-region)%>%
  stack()%>%
  mutate(country='Sri Lanka')

WHO_interest5 = WHO_LDA_Data1_normalized%>%
  filter(country=='Afghanistan')%>%
  dplyr::select(-country,-region)%>%
  stack()%>%
  mutate(country='Afghanistan')

WHO_interest6 = WHO_LDA_Data1_normalized%>%
  filter(country=='Yemen')%>%
  dplyr::select(-country,-region)%>%
  stack()%>%
  mutate(country='Yemen')

WHO_interest_plotdata = rbind(WHO_interest1,WHO_interest2,WHO_interest3,WHO_interest4,WHO_interest5,WHO_interest6)


library(ggbeeswarm)
library(ggplot2)
ggplot()+geom_jitter(data=WHO_other,aes(x=ind,y=values,alpha=.3))+
  geom_jitter(data=WHO_interest_plotdata,aes(x=ind,y=values,col=country),size = 3)+
  theme_bw()+ggtitle('Plot of Interesting Countries Identified from LD2 parameter in LDA\nPlot by variable and distribution')+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
