### Linear Regression of School Data in R
##### Group 1: PUBPOL 542
***
#### Rstudio Prepwork: Decompressing, Reading, and Formatting 

1. First we must read our RDS data file into Rstudio.  
    + Note: We used _gzcon_ to decompress the file. Without doing this, the file __will not read properly__ and we will receive an error code.  

```{r}
df <- readRDS(gzcon(url(
  "https://github.com/PUBPOL-542-Group-1-Project/Merging-Dataframes/raw/main/datafiles/finaldata.RDS")))
summary(df)
```
#### Reformatting for R
***  

2. Next, we have to call library DPLYR to use the %>% syntax.
    + Note: With loading this library (and loading all future libraries) remove the "#" and run if you have not yet loaded this library in your Rstudio.

```{r}
#library(dplyr)
```

3. Now, we will format our column labels so they are more R-Friendly, then call the summary to double-check our changes.
    + Note: I've created a new fromPy dataframe, called fromPy3, that will reflect the changes.  This way I can preserve the formatting of the original.  

```{r}
fromPy3 <- fromPy %>% rename(LowIncome = "Low-Income") %>% 
  rename(AllStudents = "All Students") %>% 
  rename(ELL = "English Language Learners") %>%
  rename(LowIncomeP = "Low-Income.p") %>%
  rename(MaleP = "Male.p") %>%
  rename(WhiteP = "White.p") %>%
  rename(ELLP = ELL.p)
summary(fromPy3)
```
#### Hypotheses, Regression, Correlations, ANOVA/Rsquared
***  

1. Now onto the actual regression.  I set up three separate hypotheses for my three future regressions.

```{r}
hypo1=formula(Graduate ~ LowIncome + DisciplineInt)

hypo2=formula(Graduate ~ LowIncome + DisciplineInt + White + Expenditure)

hypo3=formula(Graduate ~ LowIncome + White + Expenditure)
```

2. Now that we've defined our three hypotheses, let's run our 3 separate regressions. Each of these regressions will be Gaussian and will pull from our "fromPy3" dataframe.
    + Note: I will summarize each regression after it is created so I can evaluate each one.

```{r}
gauss1=glm(hypo1, 
           data=fromPy3,
           family="gaussian")
summary(gauss1)

gauss2=glm(hypo2,
           data=fromPy3,
           family="gaussian")
summary(gauss2)

gauss3=glm(hypo3,
           data=fromPy3,
           family="gaussian")
summary(gauss3)
```

3. We can further evaluate our regressions by looking at our correlation matrix. First we must create a new fromPy, and null out the categorical data.  
    + Note: if we do not null out categorical data, our correlation matrix will return an error.  
    
```{r}
fromPy4 <- fromPy3
fromPy4$DisciplineInt=NULL
fromPy4$SchoolName=NULL
corrmatrix1 <- cor(fromPy4)
corrmatrix1
```

4. So now we can see our correlations.  Let's continue evaluating our models by running an Analysis of Variance (ANOVA)

```{r}
anova(gauss1, gauss2, gauss3, test="Chisq")
```

5. So ANOVA is done, now let's look at our R-squared.
    + Note: we must install the package "rsq" and load the library "rsq" before using it. Please remove "#" and run those steps if you have not downloaded/loaded that package and library.

```{r}
#install.packages("rsq")
#library(rsq)
rsq(gauss1)
rsq(gauss2)
rsq(gauss3)
```

>Comments on Linear Regression: models 2 and 3 had a much higher r-squared than model 1, so our choice will fall between those two.  Model 3 was chosen because it predicts the variance in our outcome similarly to Model 2, and does so without the need for an additional categorical predictor (thus improving our n:p ratio.) Model 3 will be used going forward.  

#### Visualizing Our Model
***

1. Now for my plots.  I created a dot and whisker plot for model 3,
then plotted just the linear components (cplot) for "expenditure", "white", and
"LowIncome".  I also did a 3D plot of my model with the "persp" function.  
These are all useful visualizations for my model.  

```{r}
#library(dotwhisker)
dwplot(gauss3,by_2sd = F)
#library(margins)
cplot(gauss3, 'Expenditure')
cplot(gauss3, 'White')
cplot(gauss3, 'LowIncome')
persp(gauss3)

```

#### Confirmation of Computed Predictors
***

2. Now that I've plotted those, I will use the code provided by Professor M. 
to confirm the marginal effects of each of my predictors.  
    + Note: This is an extra step provided for a LOG regression, but it's nice to have 
confirmation nonetheless. Note that the marginal effects match the regression 
values given by Model 3 in prior steps.  *

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


### THANKS FOR READING!

##### -Group 1 PUBPOL 542 WINTER 2021
##### (Micaela, Nick, Madelyn, and Wenzhen)


