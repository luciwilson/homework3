#Question 1
diamonds<-read.csv("diamond.csv")
head(diamonds)
str(diamonds)
#What is the effect of each cut on the price of a typical diamond (in dollars)?
Cut<-diamonds$cut
Price<-diamonds$price
plot(Cut~Price)
Diamond.model<-glm(Price~Cut,family = "poisson")
Diamond.model
boxplot(Price~Cut,data=diamonds,main="diamonds",xlab="Cut of Diamond",ylab="Price of Diamond")
coef(Diamond.model)
confint(Diamond.model)
exp(8.3799424)
# exp(8.3799424) = $4358.758 is the fair cut diamond original price
exp(8.3799424-0.1038367)-exp(8.3799424)
# from a fair to a good diamond cut, the price decreases by $429.73
exp(8.3799424-0.2316292)-exp(8.3799424)
# from a fair to an ideal cut diamond the price decreases by $901.22
#although this does not make intuitive sense
exp(8.3799424+0.0504411)-exp(8.3799424)
# from a fair to a premium cut diamond the price increases by $225.50
exp(8.3799424-0.0904632)-exp(8.3799424)
# from a fair to a very good cut diamond the price decreases by $376.98
#As output, produce a boxplot of original data, parameter estimates, and 95% CI

#Question 2
cuse<-read.csv("contraception.csv")
head(cuse)
str(cuse)
sex=cuse$using/(cuse$notUsing+cuse$using)
boxplot(sex~cuse$education)

#Contraception use and level fo education
cont=cbind(cuse$using, cuse$notUsing)
cuse_model<-glm(cont~cuse$education, family = "binomial")
coef(cuse_model)
confint(cuse_model)
#While this data suggests that there is a 2% increase in contraception use 
#for educated women, the significance level is not high as the confidence interval
# of 95% overlaps 0.

#Question 3
hurricanes<-read.csv("Hurricane dataset.csv")
head(hurricanes)
str(hurricanes)
boxplot(hurricanes$alldeaths~hurricanes$Gender_MF)

hurricanes_model<-glm(hurricanes$alldeaths~hurricanes$Gender_MF, family = "poisson")
coef(hurricanes_model)
confint(hurricanes_model)
exp(3.1679220-0.5123354)-exp(3.1679220)
#-9.524731
library(MASS)
storm<-glm.nb(hurricanes$alldeaths~hurricanes$Gender_MF)
coef(storm)
#(Intercept) hurricanes$Gender_MFM 
#3.1679220            -0.5123354
confint(storm)
#2.5 %    97.5 %
#  (Intercept)            2.816448 3.5640722
#hurricanes$Gender_MFM -1.149166 0.1720959
#Poisson distribution may not be the best decision for this data set
#because Poisson assumes an equal mean and variance so for this dataset
#a negative binomial would work better.
#On average 9.5 more people are killed by hurricanes with a female 
#name than with a male name and this data is significant ast the CI95% 
#does not overlap 0. That is, however, significant according to the Poisson
#distribution. 
# when a negative binomial is run with this dataset we find there are
#no significant differences between average deaths between male
#and female named hurricanes.







#Question 4
#Find one dataset from your own research that could be modelled as normal, binomial, or poisson
##regression (note: http://datadryad.org/ is a great source for existing datasets if you dont
###have your own data yet). Run a glm on your data and interpret results in terms of effect size,
####visualization, and statistical significance.


Androgerard<-read.csv("Y2_AG_Survival.csv")
str(Androgerard)
boxplot(Androgerard$Treatment~Androgerard$Present_Absent)
survival<-glm(Androgerard$Present_Absent~Androgerard$Treatment, family = "poisson")
coef(survival)
confint(survival)
exp(2.99551e-16-1.576289e-01)-exp(2.99551e-16)
#-0.145833 Luci inoculum treatment
exp(2.99551e-16-4.652002e-02)-exp(2.99551e-16)
#-0.04545455 Sterile inoculum treatment
exp(2.99551e-16-1.700383e-15)-exp(2.99551e-16)
#-1.665335e-15 Jim inoculum treatemnt
exp(2.99551e-16-3.413514e-16)-exp(2.99551e-16)
#-2.22046e-16 Whole inoculum treatment

#According to this poisson distribution the whole inoculum treatment
#has a better chance of survival.However, the confint crosses 0
# so it is not considered a significant increase.