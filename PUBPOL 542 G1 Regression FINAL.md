PubPol542\_Regression
================

###Decompressing & Reading File into Rstudio
First we must read our RDS data file into Rstudio.  
Note that we used "gzcon" to decompress the file. 
Without doing this, the file will not read properly and 
we will receive an error code.  
```{r}
df <- readRDS(gzcon(url(
  "https://github.com/PUBPOL-542-Group-1-Project/Merging-Dataframes/raw/main/datafiles/finaldata.RDS")))
summary(df)
```
###Reformatting for R

Next, we have to call library DPLYR to use the %>% syntax.
These next steps are changing the formatting for our column labels 
so they are more "R-Friendly". Once I've renamed these columns, 
I call the summary to double check that my changes went through. 
Also note that in Line 14, I've created a new fromPy, called fromPy3, 
that will reflect the changes.  This way I can preserve the formatting of the 
original.  
```{r}
#library(dplyr)
fromPy3 <- fromPy %>% rename(LowIncome = "Low-Income") %>% 
  rename(AllStudents = "All Students") %>% 
  rename(ELL = "English Language Learners") %>%
  rename(LowIncomeP = "Low-Income.p") %>%
  rename(MaleP = "Male.p") %>%
  rename(WhiteP = "White.p") %>%
  rename(ELLP = ELL.p)
summary(fromPy3)
```
###Regression


Now onto the actual regression.  I set up three separate hypotheses, 
each involving a slightly different set of predictors.  I ran a Gaussian
regression for all 3, with Low Income and Discipline in the first, 
then the additional White/Expenditure predictors in the second, then 
finally the same regression as step 2 with discipline removed.  
Model 3 had a very similar rsquared to model 2 without the additional predictor
so I opted for model 3. Also note that low-income is only significant without 
white/expenditure included in the model.  Once these two factors are introduced,
that predictor is no longer as relevant.  Also, in order to run rsquared, we 
must install the "rsq" package.
```{r}
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

#install.packages("rsq")
#library(rsq)
rsq(gauss1)
rsq(gauss2)
rsq(gauss3)
```
Now for my plots.  I created a dot and whisker plot for model 3,
then plotted just the linear components (cplot) for "expenditure", "white", and
"LowIncome".  I also did a 3D plot of my model with the "persp" function.  
These are all useful visualizations for my model.  

```{r}
library(dotwhisker)
dwplot(gauss3,by_2sd = F)
library(margins)
cplot(gauss3, 'Expenditure')
cplot(gauss3, 'White')
cplot(gauss3, 'LowIncome')
persp(gauss3)

```

Now that I've plotted those, I will use the code provided by Professor M. 
to confirm the marginal effects of each of my predictors.  
This is an extra step provided for a LOG regression, but it's nice to have 
confirmation nonetheless. Note that the marginal effects match the regression 
values given by Model 3 in prior steps.  
```{r}
(marginsINFO = margins(gauss3))
(marginsSUMM=summary(marginsINFO))

base= ggplot(marginsSUMM,
             aes(x=factor, y=AME))
base= base + geom_point()

plotMargins = base+ theme(axis.text.x = element_text(angle = 80,size = 6,hjust = 1))
plotMargins

plotMargins + geom_errorbar(aes(ymin=lower, 
                                ymax=upper))
```

###THANKS FOR READING!

###-Group 1 PUBPOL 542 WINTER 2021
###(Micaela, Nick, Madelyn, and Wenzhen)

