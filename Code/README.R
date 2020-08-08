library(data.table)
library(foreign)
library(xts)
library(zoo)
library(pastecs)
library(ggplot2)
library(readxl)
library(imputeTS)
library(dynlm)
library(plotly)
library(sandwich)
library(forecast)
library(urca)
library(dyn)
library(dynlm)
library(orcutt)
library(lmtest)
library(plm)
library(tseries)
library (forecast)
library(rugarch)
library(ggcorrplot)

# Part 1 : ______________________________________________________________


Data_short_term=read_xlsx("C:/Users/dell/Desktop/data_Econometrics/Data_effect_Policy_2020.xlsx",sheet="sheet1",col_types = c("date",rep("numeric",8)))
ECB_FED_Dates=read_xlsx("C:/Users/dell/Desktop/data_Econometrics/Data_effect_Policy_2020.xlsx",sheet="sheet2",col_types = c("text","text"))
Data_short_term$dates=as.Date(Data_short_term$dates, origin = "1899-12-30")   # origin from Windows Excel

#Create a data frame in which we store the date
ECB_FED_Date=as.data.frame(ECB_FED_Dates)
# Convert the dates to "YY-MM-DD" Format !! we have to remove na and change it as numerical one
ECB_Date=as.Date(as.numeric(na.omit(ECB_FED_Date$ECB_Dates)), origin = "1899-12-30")
FED_Date=as.Date(as.numeric(na.omit(ECB_FED_Date$FED_Dates)), origin = "1899-12-30")



#_______________________________________________________________________________________________
#Create the Dummy variables:
D=Data_short_term$dates   #D are the dates of our work that are from ""2012-06-18" until "2017-12-14"

# ___________Ecb dates
dum1=c()
for ( i in 1:length(ECB_Date)){
  k=which(D == ECB_Date[i])
  if (length(k)!=0){
    dum1=rbind(dum1,which(D == ECB_Date[i]))
  }
}

dummy_plus=rep(0,length(D))
dummy_plus[dum1]=1
dummy_ecb=zoo(dummy_plus,D)

#____________Fed dates
dum2=c()
for ( i in 1:length(FED_Date)){
  k=which(D ==FED_Date[i])
  if (length(k)!=0){
    dum2=rbind(dum2,which(D == FED_Date[i]))
  }
}

dummy_minus=rep(0,length(D))
dummy_minus[dum2]=1
dummy_fed=zoo(dummy_minus,D)


#_________________________________ Interpolate the missing values and create zoo series
# just on some days in year// it won't effect much the study !! 

Data_short_term$Euribor3=na.interp(Data_short_term$Euribor3) 
Euribor3=zoo(Data_short_term$Euribor3,D)  # Create a zoo object

Data_short_term$Euribor6=na.interp(Data_short_term$Euribor6) 
Euribor6=zoo(Data_short_term$Euribor6,D)  # Create a zoo object

Data_short_term$EuriborY=na.interp(Data_short_term$EuriborY) 
EuriborY=zoo(Data_short_term$EuriborY,D)  # Create a zoo object

Data_short_term$Euriborw=na.interp(Data_short_term$Euriborw)
Euriborw=zoo(Data_short_term$Euriborw,D)  # Create a zoo object

Data_short_term$Srvix=na.interp(Data_short_term$Srvix) 
Srvix=zoo(Data_short_term$Srvix,D)        # Create a zoo object

Data_short_term$ShadowR=na.interp(Data_short_term$ShadowR)
ShadowR=zoo(Data_short_term$ShadowR,D)      # Create a zoo object

Data_short_term$Eonia=na.interp(Data_short_term$Eonia)  
Eonia=zoo(Data_short_term$Eonia,D)        # Create a zoo object

Data_short_term$Stoxx=na.interp(Data_short_term$Stoxx)  
Stoxx=zoo(Data_short_term$Stoxx,D)        # Create a zoo object

Data=cbind(Data_short_term$EuriborY,Data_short_term$Srvix,Data_short_term$ShadowR,Data_short_term$Eonia,Data_short_term$Stoxx)
#Check corroletation
x11()
ggcorrplot(cor(Data), p.mat = cor_pmat(Data), hc.order=TRUE, type='lower')

#plot the euribor 3M
autoplot(Euribor3,colour = 'red') +ggtitle("Euribor 3 months") +ylab("Euribor") +xlab("Year")

#________ ____________________ Performing regression


log_vol=diff(Srvix,1,log=TRUE)      
deltaShadowR=diff(ShadowR,1,log=FALSE) 
change_rates=diff(Eonia,1)             
 
change_Euribor3=diff(lag(Euribor3,1))        # E_(t+1)-E_(t)
change_Euribor6=diff(lag(Euribor6,1))        # E_(t+1)-E_(t)
change_EuriborY=diff(lag(EuriborY,1))        # E_(t+1)-E_(t)
change_Euriborw=diff(lag(Euriborw,1))        # E_(t+1)-E_(t)

dummy_change_fed=dummy_fed*log_vol               
dummy_change_Ecb=dummy_ecb*deltaShadowR

#Effect on EONIA rates:
fit=dynlm(change_rates~dummy_fed+dummy_ecb+dummy_change_fed+dummy_change_Ecb+Stoxx)
# Effect on Euribor rates:
fit1w=dynlm(change_Euriborw~dummy_fed+dummy_ecb+dummy_change_fed+dummy_change_Ecb+Stoxx)
fit3M=dynlm(change_Euribor3~dummy_fed+dummy_ecb+dummy_change_fed+dummy_change_Ecb+Stoxx)
fit6M=dynlm(change_Euribor6~dummy_fed+dummy_ecb+dummy_change_fed+dummy_change_Ecb+Stoxx)
fit1Y=dynlm(change_EuriborY~dummy_fed+dummy_ecb+dummy_change_fed+dummy_change_Ecb+Stoxx)

summary(fit1w)

#plot ACF 
x11()
par(mfrow=c(2,2))
acf(residuals(fit1w)) 
acf(residuals(fit3M)) 
acf(residuals(fit6M)) 
acf(residuals(fit1Y)) 

#Test for normality
res1=as.data.frame(residuals(fit3M))
shapiro.test(res1$`residuals(fit3M)`)


ggtsdisplay(res,
            plot.type = c("partial", "histogram", "scatter", "spectrum"),
            points = TRUE,
            smooth = FALSE)

# Confidence intervals
Confidence_intervals=confint(fit)
Confidence_intervals1w=confint(fit1w)
Confidence_intervals3M=confint(fit3M)
Confidence_intervals6M=confint(fit6M)
Confidence_intervals1Y=confint(fit1Y)

Conf=cbind(Confidence_intervals1w,Confidence_intervals3M,Confidence_intervals6M,Confidence_intervals1Y)
stargazer(Conf,title="Confidence intervals from 1 week till 1 year",float.env = "sidewaystable")
# test in order to check the presence of heteroscedasticity.
# homoscedasticity

bptest(fit1w,studentize = FALSE)
bptest(fit3M,studentize = FALSE)
bptest(fit6M,studentize = FALSE)
bptest(fit1Y,studentize = FALSE)


# Test in order to check the presence of auto correlation. (order for autocorrelation 6)
#H0:The null hypothesis is no autocorrelation.

bgtest(fit1w, order = 6)
bgtest(fit3M, order = 6)
bgtest(fit6M, order = 6)
bgtest(fit1Y, order = 6)

#Heteroskedasticity and autocorrelation consistent (HAC)
coeftest=coeftest(fit,vcov=vcovHAC) 
coeftest1w=coeftest(fit1w,vcov=vcovHAC) 
coeftest3M=coeftest(fit3M,vcov=vcovHAC) 
coeftest6M=coeftest(fit6M,vcov=vcovHAC) 
coeftest1Y=coeftest(fit1Y,vcov=vcovHAC) 


#BIC criteria 
fit_c1=dynlm(change_EuriborY~L(change_EuriborY,1)+dummy_fed+dummy_ecb+dummy_change_fed+dummy_change_Ecb+Stoxx)
fit_c1=dynlm(change_EuriborY~L(change_EuriborY,1)+L(change_EuriborY,2)+dummy_fed+dummy_ecb+dummy_change_fed+dummy_change_Ecb+Stoxx)
fit_c1=dynlm(change_EuriborY~L(change_EuriborY,1)+L(change_EuriborY,2)+L(change_EuriborY,2)+dummy_fed+dummy_ecb+dummy_change_fed+dummy_change_Ecb+Stoxx)

i1=BIC(fit_c1)
i2=BIC(fit_c2)
i3=BIC(fit_c3)

# We take the minimum

# Arch model (1,0)

spec <- ugarchspec(variance.model = list(garchOrder=c(1,0)),
                   mean.model = list(armaOrder=c(0,0), include.mean = TRUE,
                                     arfima=FALSE, archm = FALSE))
archFit <- ugarchfit(spec=spec, data=change_EuriborY,typeof="latex")
coef(archFit)
x11()
plot(archFit, which = "all")
plot(archFit)
archFit

# compute deviations of the percentage changes from their mean
dev_mean_EuriborY  <- change_EuriborY - archFit@fit$coef[1]

# plot deviation of percentage changes from mean
plot(dev_mean_EuriborY, 
     type = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "Estimated Bands of +- One Conditional Standard Deviation",
     lwd = 0.2)

# add horizontal line at y = 0
abline(0, 0)

# add ARCH(0,1) confidence bands (one standard deviation) to the plot
lines( 
      archFit@fit$coef[1] + archFit@fit$sigma, 
      col = "darkred", 
      lwd = 0.5)

lines(
      archFit@fit$coef[1] - archFit@fit$sigma, 
      col = "darkred", 
      lwd = 0.5)

rob_se <- list(sqrt(diag(vcovHAC(fit, type = "HAC"))),
               sqrt(diag(vcovHAC(fit1w, type = "HAC"))),
               sqrt(diag(vcovHAC(fit3M, type = "HAC"))),
               sqrt(diag(vcovHAC(fit6M, type = "HAC"))),
               sqrt(diag(vcovHAC(fit1Y, type = "HAC"))))

# generate a LaTeX table using stargazer
stargazer(coeftest,coeftest1w,coeftest3M, coeftest6M,coeftest1Y,
          title = "Dynamic Effects of the news on the interest rates",
          se = rob_se,
          type="latex",
          no.space = T,
          align = T,
          header = F,
          column.labels = c("EONIA","EURIBOR1W","EURIBOR3M","EURIBOR6M","EURIBOR1Y"),float.env = "sidewaystable")

require("magrittr")

stargazer::stargazer(archFit@fit$matcoef, 
                     title = "Parameter Estimates of the ARCH(1, 0) for Euribor 1Y") %>% 
  gsub("Std. Error", "Rob. Std. Error", .) %>%  
  gsub("t value", "Rob. t value", .) %>%  
  gsub("mu", "$\\\\mu$", .) %>%
  gsub("alpha1", "$\\\\alpha$", .) %>%
  gsub("omega", "$\\\\omega$", .) %>%  
  writeLines("arch_output.tex")



# Part 2 : ______________________________________________________________
Macro_variables=read_xlsx("C:/Users/dell/Desktop/data_Econometrics/Data_effect_Policy_2020.xlsx",sheet="sheet3",col_types = c("date",rep("numeric",6)))
#Labour_productivity_growth_rate=read_xlsx("C:/Users/dell/Desktop/data_Econometrics/Data_effect_Policy_2020.xlsx",sheet="sheet4",col_types = c("date","numeric"))
#GDP_at_market_prices=read_xlsx("C:/Users/dell/Desktop/data_Econometrics/Data_effect_Policy_2020.xlsx",sheet="sheet5",col_types = c("date","numeric"))


attach(Macro_variables)
Labourproductivity=Macro_variables$Labourproductivity
  
MoneyandInflation <- lm(Inflationincrease ~ Monetaryaggregate)
summary(MoneyandInflation) 
#post Covid Crisis.




MoneyandInflation <- lm(Inflationincrease[2:93,] ~ Monetaryaggregate[2:93,])
summary(MoneyandInflation)#post Covid Crisis, which does not seem to have affected this
#relation in a very significant way. Therefore, for panel data analysis we will always 
#consider post-COVID period with no regrets.



FixedMoneyandInflation <- plm(Inflationincrease ~ Monetaryaggregate, 
                              data = Macro_variables, model = 'within', index = 'years')
#does not seem to be performing too well, individual 'within' differences might not play
#a major role.

RandomMoneyandInflation <- plm(Inflationincrease ~ Monetaryaggregate, 
                               data = Macro_variables, model = 'random', index = 'years')
summary(RandomMoneyandInflation)
phtest(FixedMoneyandInflation, RandomMoneyandInflation) #interesting to see how in this case
# endogeneity does not seem to occur (p-value of the Hausman test appears to be high)
# This is an interesting case, indeed it there is actually no correlation between
# the individual features that might characterize every single year and the monetary 
# aggregate. Though, Panel data seem in both cases to be performing quite badly, with
# an Rsquare even lower than in the OLS analysis. This might be due to the fact that the the macroeconomic
#  relation between both Inflation's M3's changes from period to period is pretty stable, regrdless
# of other variables' movements.

GDPexplained <- lm(GDPgrowth ~ Labourproductivity + Monetaryaggregate + Inflationincrease)
summary(GDPexplained)
library(lmtest)
bptest(GDPexplained, ~ Labourproductivity + Monetaryaggregate + Inflationincrease, studentize = FALSE)


#The model seems to be performing quite well, with an adjusted Rsquare of 0.8618, 
#the p-value coming up after the BPtest is not small, neither big.

bptest(GDPexplained, ~ Labourproductivity + Monetaryaggregate + Inflationincrease -1, studentize = FALSE)
#to obtain a more acceptable confidence level to rule out the possibility of heteroschedasticity, it is
#enough to exclude the intercept from the model, and this is perfectly feasible.
#Indeed, the intercept (performing a t-test on the original model), is the less significant variable.

jarque.bera.test(GDPgrowth - fitted(GDPexplained))
#looking at the output of the Jarque-Brera test it is possible to confirm the 
#good performance of the model also in terms of normality of the errors.
#Normality of residuals is proved true by the high p-value of the test.




GDPexplained <- lm(GDPgrowth ~ Labourproductivity + Monetaryaggregate + Inflationincrease-1)
summary(GDPexplained)
IC1 <- confint(GDPexplained)

#Taking a look at the Ftest, it seems that in overall the linear relation holds pretty
#well, and this is confirmed by T-tests which measure variables' significance one by one.
#as expected, the Rsquare has grown even further and it is clear that the
#inflationincrease has a negative impact on the growth rate, monetary aggregate seems to
#have a positive coefficient in the linear relation, and this might be due to the fact
#that a decent level of liquidity is needed by the system in order to reach good levels
#of production. Last but not least, Labourproductivity has (obviously) a positive
#impact, and this is also clear just checking the fact that considering only 
#Labourproductivity and GDPgrowth we have:

Growthandproductivity <- lm(GDPgrowth ~ Labourproductivity)
summary(Growthandproductivity)
#and therefore a strong linear relation among the two holds.
#We can also check if panel data might be helpful:

bptest(Growthandproductivity, ~ Labourproductivity, studentize = FALSE)
jarque.bera.test(GDPgrowth - fitted(Growthandproductivity))

#no heteroschedasticity and high Rsquare. In contrast with the former OLS regression, 
#In this case residuals seem to lack normality, but we will just ignore this taking into 
#account that the evidence of a strong significance of the productivity factor for
#high level of growth simply confirms what we had stated in the former regression.
#We also keep in mind that OLS estimators are still BLUE even without the normality assumption,
#since we have already excluded the possibility that heteroschedasticity might actually occur.





FixedGrowthandproductivity <- plm(GDPgrowth ~ Labourproductivity, data = Macro_variables,
                                  model = 'within', index = 'years')
summary(FixedGrowthandproductivity)

RandomGrowthandproductivity <- plm(GDPgrowth ~ Labourproductivity, data =Macro_variables,
                                   model = 'random', index = 'years')

summary(RandomGrowthandproductivity)

phtest(FixedGrowthandproductivity, RandomGrowthandproductivity)
#the p-value seems to be low, but not low enough to exclude the possibility of performing 
#a RE analysis. Anyway, in this case Panel data seem to be more useful than previously,
#and this can be interpreted by the fact that, although strong and linear, the relation among 
#Labourproductivity and GDPgrowth seems to be influenced by unique characteristics that
#belong to a specifical period time.

OlsPhilips <- lm(Inflationincrease[1:88] ~ Unemploymentrate[1:88])
summary(OlsPhilips)
bptest(OlsPhilips, ~ Unemploymentrate[1:88], studentize = FALSE)
jarque.bera.test(Inflationincrease[1:88] - fitted(OlsPhilips))
plot(Inflationincrease[1:88], fitted(OlsPhilips), type = 'l', col = 'blue', lwd = 1)
#no heteroschedasticity, high p-value, normality of residuals


#The representation is raw, and this is why we can see a quite low Rsquare.
# Results though, seem to agree with the Philips curve's one. A higher level of Unemployment
# seems to affect Inflation in a negative way. This might look a little strange,
# since as we saw before Inflation is not good for Growth. And so it seems that higher
# levels of unemployment may facilitate the rise of GDP. But this is not the correct 
# explanation. Indeed, economists tend to agree that a moderate level of unemployment
# is an indicator of a good level of health for the economic system (it helps
# labour market' s mobility just to state an example)


Growthandunemployment <- lm(GDPgrowth[1:88]~ Unemploymentrate[1:88])
bptest(Growthandunemployment, ~ Unemploymentrate[1:88], studentize = FALSE)
#NATURAL LEVEL OF UNEMPLOYMENT: up until late 1960's, economists had the idea that it 
#was possible to keep unemeployment at a low level, tolerating some levels of positive
#inflation. However, at the end of 1960's Milton Friedman and Edmund Phelps stated that
#it would not be possible to lower unemployment forever using the inflation tradeoff,
#predicting that the unemployment level would have reached a minimum rate which would
#have been impossible to lower exploiting the trade-off inflation argument further.
#Their prediction ultimately proved true since this minimum level would be ultimately reached
#in the upcoming years. We commonly refer to this minimum rate of unemployment as to
#the natural level of unemployment.


plot(years, Longtermrates, col = 'blue', type = 'l')
#Taking a look at Italian long term interest rate, it is pretty clear that the overall
#trend is decreasing. Anyway it is interesting to see that it is not so low if compared
#to some of other countries related to the Euro area, especially with respect to some
#of the most powerful economies in the aggregate such as France and Germany.


#According to Peersman and Smets results ('The Monetary Transmission Mechanism in the Euro Area:'
#More evidence from Var Analysis'), a sudden rise in the interest rate might negatively a
#affect production in the short term. Looking the overall trend, this seems exactly to be
#what happened with Italy: the graph actually displays some big positive shocks to 
#the interest rate even though the trend is decreasing.



MPolicyeffectiveness2 <- lm(Longtermrates ~ Monetaryaggregate -1 + Inflationincrease)
summary(MPolicyeffectiveness2)

bptest(MPolicyeffectiveness2, ~ Monetaryaggregate -1)

jarque.bera.test(Longtermrates - fitted(MPolicyeffectiveness2))
plot(Monetaryaggregate, fitted(MPolicyeffectiveness2))





# It is straightforward that the level of monetary aggregate depends much on the decisions made by the ECB to inject
#liquidity into the economic system, and this has been done massively since the 2008 crisis, with 
#the QE, which is the main reason why IR have been lowering that much.

plot(years, Monetaryaggregate, type = 'l', col = 'blue', lwd = 1)

#Monetary policy's main ideas are well known: the ECB controls the money supply
#and the equilibrium at higher levels of money supply will be at a lower level in terms 
#of interest rate. But if deeply analyzed, it turns out that things are much more complicated
#than this, because the way monetary policy acts is a lot sophisticated and we will not fully
#describe it here (a lot more stuff would be needed such as the reserve market etc.).
#Anyway, using M3 to resume the mass of money in the economic system and the long term Italian interest
#rates as a proxy for the European Interest rate (as we have seen the Italian one is slightly
#above the French one and the German one, but it's very low anyway) and idea of this complexity
#is given since a straight OLS analysis ultimately fails in finding a significant linear relation 
#between the two. During the recent years a lot of economists have predicted that the post-crisis
#quantitative easing massively employed by the ECB will not manage (alone) to bring states back
#to pre-crisis levels of economic growth, especially those (such as Italy) which have suffered more its
#consequences. It might be at this point national goverments' duty to improve their reforms.
#as stated in this paper, there are some indicators (such as labour productivity) that affect
#GDP growth a lot, and monetary policy has very little control over such variables.



ancmts <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
yearsreduced <- years[10:25]
Longtermratesreduced <- Longtermrates[10:25]
Monetaryaggregatereduced <- Monetaryaggregate[10:25]

LittleDataframe <- data.frame(yearsreduced, Longtermratesreduced, Monetaryaggregatereduced
                              ,ancmts)

lmannouncements <- lm(Longtermratesreduced ~ Monetaryaggregatereduced + ancmts)

lmannouncements2 <- lm(Longtermratesreduced ~ Monetaryaggregatereduced + ancmts:Monetaryaggregatereduced)

summary(lmannouncements)
summary(lmannouncements2)


bptest(lmannouncements, ~ Monetaryaggregatereduced + ancmts, studentize = FALSE)
bptest(lmannouncements2, ~ Monetaryaggregatereduced + ancmts, studentize = FALSE)

#at least for some levels of significance, it is possible to exclude that heteroschedasticity occurs.


jarque.bera.test(Longtermratesreduced - fitted(lmannouncements))

jarque.bera.test(Longtermratesreduced - fitted(lmannouncements2))

#The Long term Interest rate is still the Italian one (proxy).
#The dummy variable has been built considering differences in the announcements within a 
#period of four years: the only year that has 12 announcements (once per month) is described
#in the model giving a value of 1 to the dummy variable. For the other years (where fewer announcements were made)
#the dummy's value is set to 0.
#Checking both the regression's and tests' results it is clear that the announcements (even when
#they are less frequent) are made so often and are so similar between each other in terms of content 
# (they are always about cutting Interest rates) that it becomes very difficult to catch up their real
#marginal effect in terms of interest rates (for which the overall trend is decreasing).
#Even though the coefficient appears to be positive, residuals ultimately fail the normality test,
#and for the reasons explained above, it is less likely that this variable can gain a good level of significance 
#when we are in the long term framework.
#On the other side, analyzing data from this perspective (and with less values) leaves us with the
#interesting result that is definitely possible to catch up the negative relation between 
#the monetary aggregate and the interest rates. Even though we cannot control the significance,
#the result of the bptest leaves hopes that there is some reliability in this relation.



