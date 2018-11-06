library(simts)
library(pageviews)

wiki_mobile = project_pageviews(platform = "mobile-app", granularity = "daily", start = "2017110600", end = "2018110600")
wiki_desktop = project_pageviews(platform = "desktop", granularity = "daily", start = "2018050600", end = "2018110600")
wiki_silvio <- article_pageviews(article = "Silvio_Berlusconi", start = "2018090600", end = "2018110600")
wiki_beyonce <- article_pageviews(article = "Beyonce", start = "2018080600", end = "2018110600")
wiki_chomsky <- article_pageviews(article = "Noam_Chomsky", start = "2018090600", end = "2018110600")
wiki_lazio <- article_pageviews(article = "SS_Lazio", start = "2018050100", end = "2018110600")
wiki_thanks <- article_pageviews(article = "Thanksgiving", start = "2015010100", end = "2018110600")

#MobileViews:

Mt = gts(wiki_mobile$views)
plot(Mt)
modM = estimate(SARIMA(ar = 2, i = 0, ma = 1, sar = 2, si = 0, sma = 2, s = 7), Mt)
check(modM)
predict(modM, n.ahead = 30)

mobile_pred <- predict(modM, n.ahead = 1, level = c(0.95))

#DesktopViews:

Dt = gts(wiki_desktop$views)
plot(Dt)
modD = estimate(SARIMA(ar = 1, i = 0, ma = 1, sar = 2, si = 0, sma = 1, s = 7), Dt, method = "rgmwm")
check(modD)
predict(modD, n.ahead = 30)

#SilvioViews:

St = gts(wiki_silvio$views)
plot(St)
plot(auto_corr(St,pacf=TRUE))
modS = estimate(AR(1),St)
check(modS)
predict(modS, n.ahead = 30)

#BeyonceViews:

Bt = gts(wiki_beyonce$views)
plot(Bt)
plot(auto_corr(Bt,pacf=TRUE))
modS = estimate(AR(9),St)
check(modS)
predict(modS, n.ahead = 30)

#ChomskyViews:

Ct = gts(wiki_chomsky$views)
plot(Ct)
plot(auto_corr(Bt))
modC = estimate(AR(6),Ct)
check(modC)
predict(modC, n.ahead = 10)

#LazioViews:

Lt = gts(wiki_lazio$views)
plot(Lt)
plot(auto_corr(Lt))
modL = estimate(AR(9), Lt)
check(modL)
predict(modL, n.ahead = 30)

#ThanksViews:

Tt = gts(wiki_thanks$views)
Tt = gts(Tt[c(80:150,445:515,810:880,1175:1224),])
Tt = gts(Tt[c(30:71,100:141,170:211,240:263),])
plot(Tt)
plot(auto_corr(Tt,pacf=TRUE))
modT = estimate(SARIMA(ar = 2, i = 0, ma = 1, sar = 1, si = 0, sma = 1, s = 42), Tt, method = "rgmwm")
check(modT)
predict(modT, n.ahead = 30)
