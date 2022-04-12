#-------------------------------------------#
#   T R A B A J O   P R Á C T I C O   N° 2  #
#-------------------------------------------#
# Autor: Carla Srebot & Agustin Sotelo      
# Fecha: Abril 2019                         


# Set directory:
setwd("C:/Users/Hp Support/Videos/03 - Cursos/02 - Econometría Avanzada/03 - Econometría Avanzada - Udesa (2022)/Trabajos prácticos/Trabajo práctico 2")

# Open dataset:

library('haven')

cornwell <- read_csv("C:/Users/Hp Support/Videos/03 - Cursos/02 - Econometría Avanzada/03 - Econometría Avanzada - Udesa (2022)/Trabajos prácticos/Trabajo práctico 2/cornwell.csv")


#-------------------------------------------#
# Between Model (Table 3 Cornwell) ---------#

library('plm')

BE_model <- plm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+lwcon+lwtuc+lwtrd+
                  lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+west+central+urban+lpctmin,
                data = cornwell, index=c("county", "year"), model="between")

summary(BE_model)


#-------------------------------------------#
# FE Model (Table 3 Cornwell) --------------#

FE_model <- plm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+lwcon+lwtuc+lwtrd+
                  lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+west+central+urban+lpctmin,
                data = tp2_data, index=c("county", "year"), model="within")

summary(FE_model)


# OLS:
ols <-lm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+
           lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc, data = tp2_data)

# Testing for fixed effects, null: OLS better than fixed
pFtest(FE_model, ols)

# Testing for fixed effects, null: Between better than fixed
phtest(BE_model, FE_model, data=tp2_data, model = c("between", "within"), method = c("chisq", "aux"), index = NULL, vcov = NULL)


#-------------------------------------------#
# RE Model (Table 3 Cornwell) --------------#

RE_model <- plm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+lwcon+lwtuc+lwtrd+
                  lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+west+central+urban+lpctmin,
                data = tp2_data, index=c("county", "year"), model="random")

summary(RE_model)

# Hausman Test: 
phtest(RE_model, FE_model, data=tp2_data, model = c("random", "within"), method = c("chisq", "aux"), index = NULL, vcov = NULL)

  
#-------------------------------------------#
# Table of results -------------------------#

library('stargazer')
stargazer(BE_model, FE_model, RE_model, title="Resultados", dep.var.labels=c("Crime Rate"),
          covariate.labels=c('$P_{A}$','$P_{C}$','$P_{P}$','S','Police','Density','Percent of young male','WCON','WTUC',
                             'WTRD','WFIR','WSER','WMFG','WFED','WSTA','WLOC','West','Central','Urban','Percent minority'),
          notes.align = "l",align=TRUE, out = "Table.tex")