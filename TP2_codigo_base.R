# HEADER --------------------------------------------
#
# UNIVERSIDAD DE SAN ANDRES
# Class: 
#
# Authors: Carlos J. Daboin, Denys Casiano, Anzony Quispe
#
# Date: 2022-04-11
# Last Update: 
#
# Script Name: Codigo basico para el TP n2
#
#
# 0.1 Load Libraries -----------------------------
library(readr)
library('plm')
library(lmtest)
library('stargazer')


# 0.2 Load Data ----------------------------------
cornwell <- read_csv("datos/cornwell.csv")


#-------------------------------------------#
# Between Model (Table 3 Cornwell) ---------#


BE_model <- plm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+lwcon+lwtuc+lwtrd+
                  lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+west+central+urban+lpctmin,
                data = cornwell, index=c("county", "year"), model="between")

summary(BE_model)


#-------------------------------------------#
# FE Model (Table 3 Cornwell) --------------#

FE_model <- plm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+lwcon+lwtuc+lwtrd+
                  lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+west+central+urban+lpctmin,
                data = cornwell, index=c("county", "year"), model="within")

summary(FE_model)


# OLS:
ols <-lm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+
           lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc, data = cornwell)

# Testing for fixed effects, null: OLS better than fixed
pFtest(FE_model, ols)

# Testing for fixed effects, null: Between better than fixed

t_hausman <- phtest(BE_model, FE_model, data=cornwell, model = c("between", "within"), method = c("chisq", "aux"), index = NULL, vcov = NULL)

#-------------------------------------------#
# Table of results -------------------------#

stargazer(BE_model, FE_model, title="Resultados", dep.var.labels=c("Crime Rate"),
          covariate.labels=c('$P_{A}$','$P_{C}$','$P_{P}$','S','Police','Density','Percent of young male','WCON','WTUC',
                             'WTRD','WFIR','WSER','WMFG','WFED','WSTA','WLOC','West','Central','Urban','Percent minority'),
          notes.align = "l",align=TRUE, out = "Table1.tex")

#-------------------------------------------#
# RE Model (Table 3 Cornwell) --------------#

RE_model <- plm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+lwcon+lwtuc+lwtrd+
                  lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+west+central+urban+lpctmin,
                data = cornwell, index=c("county", "year"), model="random")

summary(RE_model)

# Hausman Test: 
phtest(RE_model, FE_model, data=cornwell, model = c("random", "within"), method = c("chisq", "aux"), index = NULL, vcov = NULL)

  
#-------------------------------------------#
# Table of results -------------------------#


stargazer(BE_model, FE_model, RE_model, title="Resultados", dep.var.labels=c("Crime Rate"),
          covariate.labels=c('$P_{A}$','$P_{C}$','$P_{P}$','S','Police','Density','Percent of young male','WCON','WTUC',
                             'WTRD','WFIR','WSER','WMFG','WFED','WSTA','WLOC','West','Central','Urban','Percent minority'),
          notes.align = "l",align=TRUE, out = "Table2.tex")



## TEST BPG 
bptest(FE_model)

