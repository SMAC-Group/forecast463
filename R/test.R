library(gmailr)
use_secret_file('stat463.json')
library(forecast463)
from = "psu.forecasting.group.00@gmail.com"
receiver = "psu.forecasting.instructors@gmail.com"
group = 1
key = "aaaaaaaaaaaaaaa"

mobile_pred = rnorm(1)
mobile_ci = mobile_pred + c(cbind(-abs(rnorm(1)), abs(rnorm(1)))) #
mobile_forecasts = list(mobile_pred = mobile_pred, mobile_ci = mobile_ci)

desktop_pred = rnorm(1)
desktop_ci = desktop_pred + c(cbind(-abs(rnorm(1)), abs(rnorm(1))))
desktop_forecasts = list(desktop_pred = desktop_pred, desktop_ci = desktop_ci)

silvio_pred = rnorm(1)
silvio_ci = silvio_pred + c(-abs(rnorm(1)), abs(rnorm(1)))
silvio_forecasts = list(silvio_pred = silvio_pred, silvio_ci = silvio_ci)

beyonce_pred = rnorm(1)
beyonce_ci = beyonce_pred + c(-abs(rnorm(1)), abs(rnorm(1)))
beyonce_forecasts = list(beyonce_pred = beyonce_pred, beyonce_ci = beyonce_ci)

chomsky_pred = rnorm(1)
chomsky_ci = chomsky_pred + c(-abs(rnorm(1)), abs(rnorm(1)))
chomsky_forecasts = list(chomsky_pred = chomsky_pred, chomsky_ci = chomsky_ci)

lazio_pred = rnorm(1)
lazio_ci = lazio_pred + c(-abs(rnorm(1)), abs(rnorm(1)))
lazio_forecasts = list(lazio_pred = lazio_pred, lazio_ci = lazio_ci)

thanks_pred = rnorm(1)
thanks_ci = thanks_pred + c(-abs(rnorm(1)), abs(rnorm(1)))
thanks_forecasts = list(thanks_pred = thanks_pred, thanks_ci = thanks_ci)


prediction = list(mobile = mobile_forecasts,
                  desktop = desktop_forecasts,
                  silvio = silvio_forecasts,
                  beyonce = beyonce_forecasts,
                  chomsky = chomsky_forecasts,
                  lazio = lazio_forecasts,
                  thanks = thanks_forecasts)


check_prediction(prediction = prediction)
check_credentials(group = group, key = key, from = from)

send_prediction(group = group, prediction = prediction, from = from, key = key)

send_message(mime(
  To = receiver,
  From = sender,
  Subject = "Hi",
  body = "Hello"))
