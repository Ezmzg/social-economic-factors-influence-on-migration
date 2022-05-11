##TIDY DATA
# TODO 
#responsive var c42,43,44: 1-5 1:agree on migrants, 5:disagree (c43 needs recoding, cause atm 1:disagree, 5:agree)

## countries: dummy ##ita=0, swe=1

install.packages('tidyverse')
install.packages('janitor')
install.packages('foreign')
install.packages('broom')
install.packages('sjlabelled')
install.packages('sets')
library(tidyverse);library(janitor);library(foreign); library(broom);library(sjlabelled);library(haven);library(sets)

#setwd("~/")
setwd("C:/Users/Elena/Downloads/European Social Survey [round 8]")
ess_sur<- read_sav('ESS8e02_1.sav')
typeof(ess_sur$cntry)
ita_swe<-ita_swe<-filter(ess_sur, cntry=='SE'| cntry=='IT')# contries under analysis
nrow(filter(ita_swe, cntry== 'IT')) ; nrow(filter(ita_swe,cntry=='SE'))# swe=1551  ita= 2626 

#responsive var c42,43,44--> 
#C42 generousity by governments towords refugees applications
c42<-ita_swe$gvrfgap
c42_d<-as.data.frame(c42)
factor(c42_d$c42)
table(c42_d$c42)## 1:272, 2: 1174, 3: 1227, 4:859, 5: 450, NA: 195
colnames(c42_d)
c42_d$c42<-as.integer(c42_d$c42)
generousity_by_gov<-c42_d$c42

##C43 Most refugee applicants not in real fear of persecution own countries
#c43 1-5 1:agree on migrants, 5:disagree (this need recoding, atm 1:disagree, 5:agree)
c43<-ita_swe$rfgfrpc 
c43<-as.data.frame(c43)
c43$c43<-as.integer(c43$c43)
#RECODING c43 inverting the numbers
c43_new<-case_when(
  c43== 5~ 1,
  c43==4~ 2,
  c43==3~ 3,
  c43==2~ 4,
  c43==1~ 5
)
c43_new_df<-as.data.frame(c43_new)
factor(c43_new_df$c43_new)
table(c43_new_df$c43_new)## 1: 218, 2:863, 3:1325, 4:1039, 5:298, NA:434
not_in_fear<-c43_new_df$c43_new

##C44 Granted refugees should be entitled to bring close family members
c44<-ita_swe$rfgbfml
c44_d<-as.data.frame(c44)
factor(c44_d$c44)
table(c44_d$c44)## 1:375, 2:1673, 3:1075, 4:590, 5:247, NA:217
bringing_family<-as.integer(c44_d$c44)

# DF comprehensive of all the responsives
dep<-as.data.frame(cbind(generousity_by_gov,not_in_fear,bringing_family))
colnames(dep)
depend_df_nan<-filter(dep,generousity_by_gov>0 , not_in_fear>0, bringing_family>0) ##dependent vaiiable questions without NA 
sapply(depend_df_nan, class)
############################# merging the 3 dep vars in one column of mean. then adding the counry and then filtering it and se
for(i in 1:nrow(depend_df_nan)){
  result<-round(mean(as.integer(depend_df_nan[i,])))
  print(result)
}##checking the mean of each row of the dependents vars

#one column dependent var with means of c42, c43, c44 with the na values
dep_df<-as.data.frame(round(apply(dep, 1, mean)),)
factor(dep_df)
table(dep_df)## 1:111, 2:955, 3:1808, 4:645, 5: 141, NA:517

##adding countries
countr<-ita_swe$cntry
country_df<- as.data.frame(countr)
dep_cnt_df<-as.data.frame(cbind(as.integer(dep_df$`round(apply(dep, 1, mean))`),as.character(countr)))
colnames(dep_cnt_df)
dep_cnt_df_na<-filter(dep_cnt_df,dep_df>0)##dependent variable questions without NA
dep_cnt_df_na$V1<-as.integer(dep_cnt_df_na$V1)
sapply(dep_cnt_df_na,class)
attitude_towards_refugees<-rename(dep_cnt_df,attitude_refugees= V1, countries= V2)

# trying to do Cronbach alpha test
install.packages('ltm')
library(ltm)
cronbach.alpha(data_nan)### dataset with one dependent and all variebles 
cronbach.alpha(data_frequencies)## for dataset with controlled variables
c<-as.data.frame(cbind(dep, d[,-c(1,3)]))
c_na<-filter(c,religions>0, generousity_by_gov>0, bringing_family>0,not_in_fear>0 ,education>0, income>0, occupations>0, religious_level>0 )
cronbach.alpha(c_na)
c_test<- as.data.frame(cbind(dep, data_frequencie[,-c(1,3)]))
c_test_na<-filter(c_test,religions>0, generousity_by_gov>0, bringing_family>0,not_in_fear>0 ,education>0, income>0, occupations>0, religious_level>0, age>0,age_groups>0)
cronbach.alpha(c_test_na) ## for dataset with the 3 questions for dependent var and controled variables


