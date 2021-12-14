# Bootstrap in lavaan

library(lavaan)
library(haven)

gssdata <- read_dta("C:/Users/joliu/OneDrive/Desktop/Structural Equation Modeling Part1-Spring 2021/Stata-Data-Sets/gss2014.dta")
gssmod <- ' prochoice ~ a*educ + paeduc
            educ ~ b*paeduc
            ind := a*b '

gssfit <- sem(gssmod,data=gssdata, se ="bootstrap", bootstrap = 1000)
parameterEstimates(gssfit, boot.ci.type = "bca.simple")