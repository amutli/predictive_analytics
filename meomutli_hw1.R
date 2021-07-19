# Homework 1
# Anna Meomutli

library(fpp2)
#Chapter 5
  #1a
daily20 = head(elecdaily, 20)
plot(daily20)
autoplot(daily20)
## It seems like with higher temperatures, there is a higher demand for electricity.
## Possibly this is caused by the heightened use of AC.
  #1b
lm = tslm(Demand ~ Temperature, data = daily20)
plot(Demand~Temperature, data = daily20)
res = resid(lm)
plot(res)
## Residuals do not seem to be correlated with time.
  #1c
forecasted15 = forecast(lm, newdata = data.frame(Temperature=15))
forecasted15
forecasted35 = forecast(lm, newdata = data.frame(Temperature=35))
forecasted35
## This seems like a legitimate and plausible forecast.
  #1d
autoplot(daily20, facets = TRUE)
daily20 %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
fit <- tslm(Demand ~ Temperature, data=daily20)
checkresiduals(fit)
forecast(fit, newdata=data.frame(Temperature=c(15,35)))
  #1e
plot(Demand~Temperature, data=elecdaily)
## This model does not take into account cold temperatures.

  #2a
head(mens400)
autoplot(mens400)
## It seems like the average winning times are significantly smaller now.
  #2b
time = time(mens400)
lm2 = tslm(mens400~time, data=mens400)
lm2
autoplot(mens400, ylab="Time", xlab="Year",main= "AB Line Fit to Time by Year")+
  geom_abline(slope = lm2$coefficients[2],
              intercept = lm2$coefficients[1])
  #2c
checkresiduals(lm2$residuals)
  #2d
forecasted2020 = forecast(lm2, newdata=data.frame(time=2020))
forecasted2020
forecasted2020$lower
forecasted2020$upper