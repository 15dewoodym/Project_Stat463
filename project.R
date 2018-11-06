library(simts)
library(pageviews)

#Submitting:
#define function:

suppressPackageStartupMessages(library(gmailr))

send_prediction = function(group, prediction, to, from, key, date = Sys.Date()){
  send_message(mime(
    To = to,
    From = from,
    Subject = paste("[STAT 463] Group ", group, sep = ""),
    body = paste(key, date, paste(unlist(prediction), collapse = ","), sep = ";")))
}


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

mobile_pred <- predict(modM, n.ahead = 1, level = 0.95)
mobile_object = list(mobile_pred = as.numeric(mobile_forecast$pred), 
                     mobile_ci = as.numeric(mobile_forecast$CI0.95))

#DesktopViews:

Dt = gts(wiki_desktop$views)
plot(Dt)
modD = estimate(SARIMA(ar = 1, i = 0, ma = 1, sar = 2, si = 0, sma = 1, s = 7), Dt, method = "rgmwm")
check(modD)
predict(modD, n.ahead = 30)

desktop_pred <- predict(modD, n.ahead = 1, level = 0.95)
desktop_object = list(desktop_pred = as.numeric(desktop_forecast$pred), 
                     desktop_ci = as.numeric(desktop_forecast$CI0.95))

#SilvioViews:

St = gts(wiki_silvio$views)
plot(St)
plot(auto_corr(St,pacf=TRUE))
modS = estimate(AR(1),St)
check(modS)
predict(modS, n.ahead = 30)


silvio_pred = predict(modS, n.ahead = 1, level = 0.95)
silvio_object = list(silvio_pred = as.numeric(silvio_forecast$pred), 
                     silvio_ci = as.numeric(silvio_forecast$CI0.95))


#BeyonceViews:

Bt = gts(wiki_beyonce$views)
plot(Bt)
plot(auto_corr(Bt,pacf=TRUE))
modB = estimate(AR(9),St)
check(modB)
predict(modB, n.ahead = 30)

beyonce_pred = predict(modB, n.ahead = 1, level = 0.95)
beyonce_object = list(beyonce_pred = as.numeric(beyonce_forecast$pred), 
                      beyonce_ci = as.numeric(beyonce_forecast$CI0.95))

#ChomskyViews:

Ct = gts(wiki_chomsky$views)
plot(Ct)
plot(auto_corr(Bt))
modC = estimate(AR(6),Ct)
check(modC)
predict(modC, n.ahead = 10)

chomsky_pred = predict(modC, n.ahead = 1, level = 0.95)
chomsky_object = list(chomsky_pred = as.numeric(chomsky_forecast$pred), 
                      chomsky_ci = as.numeric(chomsky_forecast$CI0.95))

#LazioViews:

Lt = gts(wiki_lazio$views)
plot(Lt)
plot(auto_corr(Lt))
modL = estimate(AR(9), Lt)
check(modL)
predict(modL, n.ahead = 30)

lazio_pred = predict(modL, n.ahead = 1, level = 0.95)
lazio_object = list(lazio_pred = as.numeric(lazio_forecast$pred), 
                    lazio_ci = as.numeric(lazio_forecast$CI0.95))

#ThanksViews:

Tt = gts(wiki_thanks$views)
Tt = gts(Tt[c(80:150,445:515,810:880,1175:1224),])
Tt = gts(Tt[c(30:71,100:141,170:211,240:263),])
plot(Tt)
plot(auto_corr(Tt,pacf=TRUE))
modT = estimate(SARIMA(ar = 2, i = 0, ma = 1, sar = 1, si = 0, sma = 1, s = 42), Tt, method = "rgmwm")
check(modT)
predict(modT, n.ahead = 30)

thanks_pred = predict(modT, n.ahead = 1, level = 0.95)
thanks_object = list(thanks_pred = as.numeric(thanks_forecast$pred), 
                     thanks_ci = as.numeric(thanks_forecast$CI0.95))


prediction = list(mobile = mobile_object, desktop = desktop_object, 
                  silvio = silvio_object, beyonce = beyonce_object, 
                  chomsky = chomsky_object, lazio = lazio_object, 
                  thanks = thanks_object)


group = 1 # insert group number
from = "psu.forecasting.group.1@gmail.com" # insert group gmail address
key = "aaaaaaaaaaaaaaa" # insert group unique key
credential_OK = check_credentials(group = group, from = from, key = key)

# Check prediction object
prediction_OK = check_prediction(prediction = prediction)



group = 1 #group number
to = "madewoody@gmail.com" # email address that will receive the forecasts
from = "psu.forecasting.group.1@gmail.com" # example gmail address used by group number X to send their forecasts
key_group = "sKCFrcrnXk6623L" # example of unique key identifier for the group
date = Sys.Date() # time at which the forecast is sent

# The function defined earlier to send the predictions
send_prediction(group = group, prediction = prediction, to = to, from = from, 
                key = key_group, date = date)



