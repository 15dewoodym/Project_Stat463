install.packages("gmailr")
library(gmailr)
use_secret_file('stat463.json')

sender = "psu.forecasting.group.1@gmail.com"
receiver = "madewoody@gmail.com"

send_message(mime(
  To = receiver,
  From = sender,
  Subject = "Hi",
  body = "Hello"))

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


#Predictions:

mobile_pred = rnorm(1) # example scalar forecast for the next day
mobile_ci = as.matrix(cbind(-abs(rnorm(1)), abs(rnorm(1)))) # example vector with lower bound (first element) and upper bound (second element) of the confidence intervals for each of the 24 points to be forecast
mob_forecasts = list(mobile_pred = mobile_pred, mobile_ci = mobile_ci) # list to be included in the prediction object

prediction = list(mobile = mob_forecasts)
#, desktop = desk_forecasts, silvio = silvio_forecasts, beyonce = bey_forecasts, chomsky = chom_forecasts, lazio = lazio_forecasts, thanks = thanks_forecasts)


#Actual Submission:

group = 1 #group number
to = "madewoody@gmail.com" # email address that will receive the forecasts
from = "psu.forecasting.group.1@gmail.com" # example gmail address used by group number X to send their forecasts
key_group = "sKCFrcrnXk6623L" # example of unique key identifier for the group
date = Sys.Date() # time at which the forecast is sent

# The function defined earlier to send the predictions
send_prediction(group = group, prediction = prediction, to = to, from = from, 
                key = key_group, date = date)

install.packages("devtools")
devtools::install_github("SMAC-Group/forecast463")

####psu.forecasting.instructors@gmail.com