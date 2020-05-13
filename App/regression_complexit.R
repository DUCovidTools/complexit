## Regression modelling


## If Italian proxy available:

# t as independent variable
# cases as dependent.
# Fit regression model, use S.E. to plot uncertainty. 

## If no Italian proxy available
# Fit just to UK data

## Questions:
# - weight UK and Italy data the same?
# - How to show growing uncertainty in future?

setwd( "C:/Users/Rachel Oughton/Dropbox/postdocs/COVID19/ComplexIt/App")
source("global.R")

## 

calderdale = inData[inData$areaName == "Calderdale", 5:47]
Sunderland = inData[inData$areaName == "Sunderland", 5:47]
CoDurham = inData[inData$areaName == "County Durham", 5:47]

pugfog = inData[inData$areaName == "Puglia Foggia", -(1:4)]
pugfogTS = ts(as.numeric(pugfog))
pugfog_diff_ts = sapply(2:length(pugfogTS),
                            function(i){
                              pugfogTS[i] - pugfogTS[i-1]
                            })


calderdaleTS = ts(as.numeric(calderdale[1,]))
SunderlandTS = ts(as.numeric(Sunderland[1,]))
CoDurhamTS = ts(as.numeric(CoDurham[1,]))


calderdale_diff_ts = sapply(2:length(calderdaleTS),
                            function(i){
                              calderdaleTS[i] - calderdaleTS[i-1]
                            })
Sunderland_diff_ts = sapply(2:length(SunderlandTS),
                            function(i){
                              SunderlandTS[i] - SunderlandTS[i-1]
                            })
CoDurham_diff_ts = sapply(2:length(CoDurhamTS),
                            function(i){
                              CoDurhamTS[i] - CoDurhamTS[i-1]
                            })

glm_codur = data.frame(t = 1:length(CoDurhamTS[-(1:30)]), Cases = as.numeric(CoDurhamTS[-(1:30)]))
glmfit_codur = glm(Cases~t, family = gaussian(link = "log"), data=glm_codur)
n.codur = length(glm_codur)
newt= n.codur:(n.codur+9)
pred_glmfit = predict(glmfit_codur, newdata = data.frame(t=newt, type= "link"), se.fit = TRUE)

lmlogfit_codur = lm(log(Cases)~t,  data=glm_codur)
pred_lmfit = predict(lmlogfit_codur, newdata = data.frame(t=newt), se.fit = TRUE, interval = "prediction")


pred1 = rnorm(n=1000, mean = 8.56695, sd = 0.12516)
## WORK OUT HOW TO RE-TRANSFORM THIS!

## Trying bias corrected thing from
# https://stats.stackexchange.com/questions/69613/bias-correction-of-logarithmic-transformations
res_fit = resid(glmfit_codur)
n_glmfit = length(res_fit)
corr_term = (1/n_glmfit)*sum(exp(res_fit))


