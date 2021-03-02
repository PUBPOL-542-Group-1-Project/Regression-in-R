##Load packages and options
install.packages("foreign")
install.packages("psych")
install.packages("plyr")
install.packages("moments")

library(foreign)
library(psych)
library(plyr)
library(moments)

options(scipen=100)
options(digits=2)

##read RDS file from git
df <- readRDS(gzcon(url(
  "https://github.com/PUBPOL-542-Group-1-Project/Merging-Dataframes/raw/main/datafiles/finaldata.RDS")))
summary(df)

##reformatting data for R
library(dplyr)
fromPy %>% rename(LowIncome = "Low-Income")
fromPy %>% rename(AllStudents = "All Students")
fromPy %>% rename(ELL = "English Language Learners")
fromPy3 <- fromPy %>% rename(LowIncome = "Low-Income") %>% 
  rename(AllStudents = "All Students") %>% 
  rename(ELL = "English Language Learners") %>%
  rename(LowIncomeP = "Low-Income.p") %>%
  rename(MaleP = "Male.p") %>%
  rename(WhiteP = "White.p") %>%
  rename(ELLP = ELL.p)
summary(fromPy3)

##regression, correlation, anova, and rsquared
hypo1=formula(Graduate ~ LowIncome + DisciplineInt)
gauss1=glm(hypo1, 
           data=fromPy3,
           family="gaussian")
summary(gauss1)

hypo2=formula(Graduate ~ LowIncome + DisciplineInt + White + Expenditure)
gauss2=glm(hypo2,
           data=fromPy3,
           family="gaussian")
summary(gauss2)

hypo3=formula(Graduate ~ LowIncome + White + Expenditure)
gauss3=glm(hypo3,
           data=fromPy3,
           family="gaussian")
summary(gauss3)

fromPy4 <- fromPy3
fromPy4$DisciplineInt=NULL
fromPy4$SchoolName=NULL
corrmatrix1 <- cor(fromPy4)
corrmatrix1

anova(gauss1, gauss2, gauss3, test="Chisq")

install.packages("rsq")
library(rsq)
rsq(gauss1)
rsq(gauss2)
rsq(gauss3)
##gauss3 had almost exact rsquared as gauss2, without
##disciplineint included, gauss3 will be used going forward

##Plots for my regression
library(dotwhisker)
dwplot(gauss3,by_2sd = F)
cplot(gauss3, 'Expenditure')
cplot(gauss3, 'White')
cplot(gauss3, 'LowIncome')
persp(gauss3)

##marginal effects
library(margins)
(marginsINFO = margins(gauss3))
(marginsSUMM=summary(marginsINFO))

base= ggplot(marginsSUMM,
             aes(x=factor, y=AME))
base= base + geom_point()

plotMargins = base+ theme(axis.text.x = element_text(angle = 80,size = 6,hjust = 1))
plotMargins

plotMargins + geom_errorbar(aes(ymin=lower, 
                                ymax=upper))
