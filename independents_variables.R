#TODO 
#H1 religion c12: ess_sur 1:christian 0:no christian 
#            c14: 1-10 level of religiosity 1:low 10:high 
#           taking in consideration only christian respondents, and displaying their level ( level is used for the study)

#H2: education 1-8 1:high 8:low (this need to be recoded cause atm 0 is low and 7 is high, and there are 55,77,88,99 which need to be recoded has NA)

#h3 income 1-4 1:living ok ;  4: living not ok------ f42 Which of the descriptions on this card comes closest to how 
#you feel about your household s income nowadays? 

#h4 occupation 1-5 1:job 5:no job
#e39: how likely it is that during the next 12 months you will be unemployed and looking for work for at least four consecutive weeks?
install.packages('expss')
library(tidyverse);library(janitor);library(foreign); library(broom);library(sjlabelled);library(haven);library(sets); library(expss)
ita_swe<-filter(ess_sur, cntry=='SE'| cntry=='IT')

# H2 EDUCATION
edu<-ita_swe$eisced
edu_t<-as.data.frame(edu)
edu_t$edu<-as.integer(edu_t$edu)

edu_t<-case_when(
  edu== 55 ~0,
  edu== 77 ~ 0,
  edu== 88 ~0,
  edu== 99 ~ 0,
  edu== 0 ~8, edu==1~7,
  edu==2~6, edu==3~5, 
  edu==4~4,edu==5~3,
  edu== 6~2, edu==7~1
)#works. Recoding the values 

edu_df<-as.data.frame(edu_t)
edu_t_na<-filter(edu_df, edu_t>0)#erasing values dont know, refusal, other

#tab_dep_ed<-cbind(depend_df,dep_df, edu_df )### df with countries, dependent vars and countries


#######################################################
#h3 income 
inc<-ita_swe$hincfel
income_df<-as.data.frame(inc)#  df with all values income
typeof(income_df$inc)
income_df$inc<-as.integer(income_df$inc)
income_df_na<- filter(income_df, inc>0)#df without na
#income_dummy<-countries(income_df_na)##italy=0, sweden=1
################### FUNCTION

countri<- function(x){
  n=nrow(x)
  y<-x[,2]
  for ( i in 1:n){
    if (y[i]=="IT"){
      it=0
      y[i]<-it
    }else{
      y[i]<-1
    }
  }
  x[,2]<-y
  return(x)
}
dfc<-countri(attitude_towards_refugees)
countries_dummy<-dfc[,2]
##################################
#tail(countries_dummy)
#h4 occupation 1-5 1:job 5:no job
#e39: how likely it is that during the next 12 months you will be unemployed and looking for work for at least four consecutive weeks?

occupation<-ita_swe$lkuemp
occ<-as.data.frame(occupation)
occ$occupation<-as.integer(occ$occupation)##occupation df with na values
typeof(occ$occupation)
occ_df<- as.data.frame(case_when(
  occupation == 55~ 5, occupation==1~1, occupation==2~2, occupation==3~3, occupation==4~4, occupation== 77~0, occupation==88~0, occupation== 99~0
))# column of occupation, substituting 55(= not looking for work) with the value 5 in order to have a scale
occ_df$`case_when(occupation == 55 ~ 5, occupation == 1 ~ 1, occupation == 2 ~ 2, occupation == 3 ~ 3, occupation == 4 ~ 4, occupation == 77 ~ 0, occupation == 88 ~ 0, occupation == 99 ~ 0)`<-as.integer(occ_df$`case_when(occupation == 55 ~ 5, occupation == 1 ~ 1, occupation == 2 ~ 2, occupation == 3 ~ 3, occupation == 4 ~ 4, occupation == 77 ~ 0, occupation == 88 ~ 0, occupation == 99 ~ 0)`)

occ_df<-rename(occ_df, occupations= `case_when(occupation == 55 ~ 5, occupation == 1 ~ 1, occupation == 2 ~ 2, occupation == 3 ~ 3, occupation == 4 ~ 4, occupation == 77 ~ 0, occupation == 88 ~ 0, occupation == 99 ~ 0)`)

#H1 religion c12: dummy 1:christian 0:no christian 
#            c14: 1-10 level of religiosity 1:low 10:high
#c12
relig<-as.data.frame(ita_swe$rlgblg)
reli<- case_when(
  relig==1~1, relig== 2~0 
)
religion<-as.data.frame(reli)

#c14
rel_level<-as.data.frame(ita_swe$rlgdgr)
religions_df<-as.data.frame(cbind(religion, rel_level))#all plus levels
christian<-as.data.frame(filter(religions_df, reli>0))##only christians

#################################################
##table with everything. no controlled
d<-as.data.frame( cbind(attitude_towards_refugees[,1],countries_dummy,religions_df , edu_df, income_df,occ_df ))
sapply(d, class)
###making a df with only usable values
d$occupations<-as.integer(d$occupations)
d$edu_t<-as.integer(d$edu_t)
d$inc<-as.integer(d$inc)
d$reli<-as.integer(d$reli)
d$`ita_swe$rlgdgr`<-as.integer(d$`ita_swe$rlgdgr`)
d$`attitude_towards_refugees[, 1]`<-as.integer(d$`attitude_towards_refugees[, 1]`)
d$countries_dummy<- as.integer(d$countries_dummy)

d<-rename(d, attitude_refugees=`attitude_towards_refugees[, 1]`, countries_dummy=countries_dummy , religions=reli)
d<-rename(d, religious_level= `ita_swe$rlgdgr` ,education=edu_t, income= inc, occupations= occupations )## df with all values md 1

data_nan<-filter(d, religions>0, attitude_refugees>0 ,education>0, income>0, occupations>0, religious_level>0)##!!!!! DF USED IN MODULE 1 WITHOUT NA 

#controlled variables
age<-as.data.frame(as.integer(ita_swe$agea))
gender<-as.data.frame(as.integer(ita_swe$gndr))

data_frequenci<- as.data.frame(cbind(age, gender))
sapply(data_f,class)
colnames(data_frequenci)
data_frequenci<-rename(data_frequenci, age="as.integer(ita_swe$agea)", gender= "as.integer(ita_swe$gndr)" )

### male=1 female=0
data_frequenci$gender<- case_when(
  gender== 1 ~ 1, # male
  gender==2 ~ 0, #female
)
### age grouped
age_groups<- case_when(
  age<=30 ~ 1,
  age>30 & age <= 50 ~2,
  age> 50 & age <=70 ~3,
  age> 70 ~4
)
data_f<-as.data.frame(cbind(d, data_frequenci, age_groups))##df with all the variables and values

data_frequencies<-filter(data_f, religions>0, attitude_refugees>0 ,education>0, income>0, occupations>0, religious_level>0, age>0,age_groups>0 )##filtered df all variables

## FREQUENCIES OF VARIABLES
frequencies_overall<- apply_labels(data_frequencies,
                                   attitude_refugees= num_lab('
                                            1 extremely agree
                                            2 agree
                                            3 neutral
                                            4 disagree
                                            5 extremely agree'),
                                   education=num_lab('
                                   1 high tertiary 
                                   2 lower tertiary
                                   3 sub-degree
                                   4 upper secondary
                                   5 midium secondary
                                   6 lower secondary
                                   7 primary
                                   '),
                                   income= num_lab('
                                 1 confortable
                                 2 coping
                                 3 difficult
                                 4 very difficult'),
                                   occupations=num_lab('
                                     1 not at all unemployed
                                     2 not likely unemployed
                                     3 likely unemployed
                                     4 very likely unemployed
                                     5 not looking for job'),
                                   countries_dummy= num_lab('
                                          0 Italy
                                          1 Sweden'),
                                   gender= num_lab('
                                 1 male
                                 0 female'),
                                   age_groups=num_lab('
                              1 young
                              2 adult
                              3 senior
                              4 old
                             ')
                                   
)


#frequencies for variables ovrall
fre(frequencies_overall$attitude_refugees)
fre(frequencies_overall$religious_level)
fre(frequencies_overall$education)
fre(frequencies_overall$occupations)
fre(frequencies_overall$income)
fre(frequencies_overall$countries_dummy)
fre(frequencies_overall$gender)
fre(frequencies_overall$age_groups)
