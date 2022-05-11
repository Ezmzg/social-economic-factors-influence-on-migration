###ANALYSIS OF THE DATASETS
install.packages('plotly')
install.packages('heatmaply')
library(broom)
library(tidyverse)
library(heatmaply)
library(ggcorrplot)
library(plotly)
library(tidytable)
install.packages('apaTables')
library(apaTables)
library(table1)
install.packages('devtools')
library(devtools)
devtools::install_github("strengejacke/sjPlot")
library(sjPlot)

#####  WITHOUT CONTROLLED VARIABLES   data_nan as dataset
##linear model
lm_ed<-lm(attitude_refugees ~ education ,data = data_nan )
glance(lm_ed) ##attitude vs education 
lm_re<-lm(attitude_refugees ~ religious_level ,data = data_nan )
glance(lm_re)##attitude vs religious level  # p=0.52
lm_occ<-lm(attitude_refugees ~ occupations ,data = data_nan ) 
glance(lm_occ)# attitude vs occupation
lm_inc<-lm(attitude_refugees ~ income+ countries_dummy+ income:countries_dummy ,data = data_nan)
glance(lm_inc)#attitude vs income e countries+ moderation # p income: 2.24e-6  p_country: 2.76e-4  p_moderation:0.288
lm_tot<- lm(attitude_refugees ~ education+religious_level+occupations+ income+ countries_dummy+ income:countries_dummy ,data = data_nan)
tidy(lm_tot, conf.int = TRUE)# linear model overall

##CORRELATIONS single
cor(data_nan$education, data_nan$attitude_refugees, method = 'pearson') # education
cor(data_nan$religious_level, data_nan$attitude_refugees)# religiousity, pearson is the default method 
cor(data_nan$income, data_nan$attitude_refugees)# income
cor(data_nan$occupations, data_nan$attitude_refugees)#occupation
cor(data_nan$countries_dummy, data_nan$attitude_refugees)# countries ita=0, sw=1

###heatmap 
corel<-data.frame(data_nan[,-3] )
sapply(corel, class)
cor.coef<-cor(corel)
# Create the heatmap
heatmaply_cor(
  cor(corel),
  xlab = "Features", 
  ylab = "Features",
  k_col = 2, 
  k_row = 2
)## so cool!!!!  interactive correlation map :D

# Compute a correlation matrix
corr <- cor(corel)
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(corel)
# Visualize the lower triangle of the correlation matrix
# Barring the no significant coefficient
corr.plot <- ggcorrplot(
  corr, hc.order = TRUE, type = "lower", outline.col = "white",
  p.mat = p.mat
)
print(corr.plot)# correlation triangle
ggplotly(corr.plot) # correlation triangle

#visualization for countries income attitude
left_join(data_nan, data_nan%>% 
             group_by(income, countries_dummy)%>%
             summarise(mean= mean(attitude_refugees))
)%>%
  ggplot(aes(x=income, y= attitude_refugees, colour= countries_dummy))+
  geom_jitter(position = position_jitterdodge(jitter.width = 0),
              shape=1,
              alpha=0.6)+
  geom_jitter(aes(y=mean),
              position = position_jitterdodge(jitter.width = 0),
              shape=18,
              size=5) # graph, for the line of sweden (light blue) and of italy( dark blue), seen that in Italy attitude is more negative

###apa style tables correlation 
sub_sets<-subset(data_nan, select = c(attitude_refugees,religious_level,education,income,occupations, countries_dummy))
cor(sub_sets)
apa.cor.table(sub_sets, filename = 'apatable.doc')##correlation table in apa style without controlld variables
############àà HTML
install.packages('devtools')
library(devtools)
devtools::install_github("strengejacke/sjPlot")
library(sjPlot)
sjPlot::tab_model(lm_tot, file = "~/Desktop")
#########################html
apa.reg.table(lm_tot, filename= 'regr.doc')## table regression this is apa style model 1

##### WITH CONTROL VARIABLES AGE E GENDER   TABLE STATISTIC
colnames(data_frequencies)

table1::label(data_frequencies$gender) <- "Gender "
table1::label(data_frequencies$age) <- "Age"
table1::label(data_frequencies$attitude_refugees) <- "Attitude towards refugees"
table1::label(data_frequencies$religious_level) <- "Religious Level"
table1::label(data_frequencies$education) <- "Education"
table1::label(data_frequencies$income) <- "Income"
table1::label(data_frequencies$occupations) <- "Occupation"
table1::table1(~attitude_refugees + religious_level +education + income + occupations +age + gender | countries_dummy, data = data_frequencies)

##apa style correlation
sub_set_cv<-subset(data_frequencies, select = c(attitude_refugees,religious_level,education,income,occupations, countries_dummy, age_groups, gender))
cor(sub_set_cv)
apa.cor.table(sub_set_cv, filename = 'apatable_control.doc')##correlation table in apa style without controlld variables
## linear models with controlled variables
sapply(data_frequencies, class)
lm_all<-lm(attitude_refugees ~ education+religious_level+occupations+ income+ countries_dummy+ income: countries_dummy+ gender + age_groups ,data = data_frequencies)
tidy(lm_all)##overall with gender and age control
lm_all$df.residual## df:1983
apa.reg.table(lm_all, filename= 'regr_control.doc')## apa table regression model 2



##standardised coeff
std_ed<- (sd(data_frequencies$education)/sd(data_frequencies$attitude_refugees))* 0.04964040   ## standardized: 0.11
sd(data_frequencies$countries_dummy*data_frequencies$income)/sd(data_frequencies$attitude_refugees)*(-0.03950140)## standardized: -0.033
sd(data_frequencies$religious_level)/sd(data_frequencies$attitude_refugees)*( -0.02702572)  # standardized: -0.071
sd(data_frequencies$occupations)/sd(data_frequencies$attitude_refugees)*0.01358687  ##standardized: 0.025
sd(data_frequencies$income)/sd(data_frequencies$attitude_refugees)*  0.07891397 ##standardized: 0.083
sd(data_frequencies$countries_dummy)/sd(data_frequencies$attitude_refugees)*(-0.40826266 )##standardized: -0.20
sd(data_frequencies$gender)/sd(data_frequencies$attitude_refugees)*0.06498682  ## standardized: 0.04
sd(data_frequencies$age_groups)/sd(data_frequencies$attitude_refugees)*0.03835002## standardized: 0.04


