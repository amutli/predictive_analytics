# Homework 1
# Anna Meomutli

library(fpp2)

# Chapter 5 ---------------------------------------------------------------
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

  #3
easter(ausbeer)

  #4
# If elasticity is equal to β1=(dy/dx)*(x/y), we can rearracnge the equation to be
# β1*(dx/x)=(dy/y). And because it is a derivative of y, we can just integrate that equation.

  #5a
autoplot(fancy)
# It seems that this data is seasonal with an increasing magnitude around end of the year
# because this is when Christmas is. It seems that in the later years there are more people coming by that shop
# because of the surfing event.
  #5b
# Because the logarithms of the data will take away the exponential increase compoenent,
# which will make the data much easier to interpret and work with.
  #5c
logfancy = log(fancy)
dummy = rep(0, length(fancy))
dummy[seq_along(dummy)%%12 == 3] = 1
dummy[3] = 0
dummy = ts(dummy, freq = 12, start=c(1987,1))
new = data.frame(logfancy, dummy)
lm3 = tslm(logfancy ~ season + trend + dummy, data=new)
lm3
  #5d
autoplot(lm3$residuals)
# The residuals after 1991 seem to me correlated with time a bit more than before.
  #5e
boxplot(resid(lm3) ~ cycle(resid(lm3)))
# It seems like January, March, August, September and October have large variances.
  #5f
summary(lm3)
# Novermber and December have the highest coefficients which means that this is when
# most of the sales occur.
  #5g
checkresiduals(lm3)
# The residuals are correlated with time, since p-value is less than 0.05.
  #5h
for_data = data.frame(dummy = rep(0,36))
forecastedfancy = forecast(lm3, newdata = for_data)
  #5i
new_fancy = as.data.frame(forecastedfancy)
exp(new_fancy)
  #5j
# It seems that there is still correlation of the residuals, so another type of
# regression should be used to take care of that autocorrelation.

  #6a
gasoline2004 = window(gasoline, end=2005)
autoplot(gasoline2004)
fgas1 = tslm(gasoline2004 ~ trend + fourier(gasoline2004, K=3))
fgas2 = tslm(gasoline2004 ~ trend + fourier(gasoline2004, K=7))
fgas3 = tslm(gasoline2004 ~ trend + fourier(gasoline2004, K=13))
autoplot(gasoline2004, ylab = "Weekly Suuply", main = "Fourier Transformation") +
  autolayer(fitted(fgas1))+
  autolayer(fitted(fgas2))+
  autolayer(fitted(fgas3))
  #6b
AIC(fgas1)
AIC(fgas2)
AIC(fgas3)
# It seems like the last model with K being 13 is the best one, sicne AIC is the lowest there.
  #6c
checkresiduals(fgas3)
# There is correlation.
  #6d
fc <- forecast(fgas3, newdata=data.frame(fourier(gasoline2004,13,52)))
fc
  #6e
gasoline2005 = window(gasoline, start = 2005, end = 2006)
autoplot(gasoline2005, series = "Actual Data") +
  autolayer(fc$mean, series = "Forecasted Data", ylab = "Weekly Supply", main = "Actual vs Forecasted")
# It seems like there are some outliers in the actual data around October or September that 
# we cannot account for.

  #7a
autoplot(huron)
# It seems like there are some changes to the lake level in cold months and overall average change in 
# the lake level (decreasing). 
  #7b
lm4 = tslm(huron ~ trend)
lm4
time1 = time(huron)
minus = ts(pmax(0,time1 - 1915), start = 1875)
piecehuron = tslm(huron ~ time1 + minus)
piecehuron
  #7c
newtime = seq(1973, 1980)
fc1 = forecast(lm4, newdata = data.frame(newtime))
autoplot(fc1, series = "Forecasted") +
  ggtitle({"Level of Lake Huron in feet"})



# Chapter 6 ---------------------------------------------------------------
  #1
# Here we are getting the moving average of five terms and then we extracting a moving average  
# on each of the three series of the original moving average.
# So if we do the math it would be similar to the results listed.

  #2a
autoplot(plastics)
# It seems like there is some seasonal trend with increasing magnitude.
  #2b
decomp1 = decompose(plastics, type = "multiplicative")
autoplot(decomp1)
  #2c
#Yes, they do.
  #2d
seasonal = decomp1$x/decomp1$seasonal
autoplot(seasonal)
  #2e
outlier = plastics
outlier[20] = outlier[20] + 500
decomp2 = decompose(outlier, "multiplicative")
seasonal2 = decomp2$x/decomp2$seasonal
autoplot(seasonal2)
# It added the sharp peak in the data
  #2f
# It does not matter where the peak is located because there is no seasonal trend.

  #3
setwd("/Users/annameomutli/Desktop/school/predictive/hw1/")
library(seasonal)
retail = readxl::read_excel("retail.xlsx", skip = 1)
head(retail)
retailts = ts(retail[,"A3349335T"], frequency = 12, start = c(1982, 4))
X11decomp = seas(retailts, x11 = "")
autoplot(X11decomp)
# It seems like the decomposition shows us outliers more clearly, especially in 1988 and 1989.

  #4a
#Both of these figures show some seasonal changes in the Australian labor force with an overall
#increasing trend. Up until the 1990's there seems to be a sharper upward trend in the labor force,
#and after 90s it seems to flatten out a bit, but nevertheless it goes upwards. If we look at the remainder,
#we can see that the negative trend comes from a shock event like a recession.
  #4b
#Seasonally it is adjusted, however, we can more clearly see it in the remainder and basic data columns.

  #5a
autoplot(cangas)
ggsubseriesplot(cangas)
ggseasonplot(cangas)
# There is most decrease during summer, possibly due to no need for heating. Overall increase
# could be attributed to overall greater use of technology that utilizes electricity.
  #5b
stl = stl(cangas, s.window=12)
autoplot(stl)
# Something in the 1980s really hightened the use of gas. 
  #5c
cangasts = ts(cangas, frequency = 12, start = c(1960, 45))
x11cangas = seas(cangasts, x11="")
autoplot(x11cangas)
# Here we can see more seasonal effects that we previously could not see in the STL.

  #6a
brickstl = stl(bricksq, s.window = "periodic", t.window = 12)
autoplot(brickstl)
brickstl1 = stl(bricksq, s.window = 36, t.window = 12)
autoplot(brickstl1)
brickstl2 = stl(bricksq, s.window = 36)
autoplot(brickstl2)
  #6b
brickseas = seasadj(brickstl)
autoplot(brickseas)
  #6c
naive = naive(brickseas)
autoplot(naive)
  #6d
stlf<-stlf(bricksq,method="naive")
brickfc<-forecast(stlf)
brickfc
  #6e
checkresiduals(stlf$residuals)
# A little bit of correlation.
  #6f
stl1<-stl(bricksq,t.window=12, s.window="periodic", robust=TRUE)
stl2<-seasadj(stl1)
stl3<-naive(stl2)
brickstl_fc<-forecast(stl3)
brickstl_fc
checkresiduals(brickstl_fc$residuals)
# Pretty similar results.
  #6g
train<-window(bricksq, end=c(1992,3))
test<-window(bricksq, start=c(1992,4), end=c(1994,3))
naive1<-snaive(train)
stlf0<-stlf(train)
bricksq.fc_naive<-forecast(naive1,h=8)
bricksq.fc_naive
bricksq.fc_stlf<-forecast(stlf0,h=8)
bricksq.fc_stlf
# I prefer the STLF forecast as it seems to be a bit more accurate.

  #7
autoplot(writing)
hist(writing)
stlfwriting <- stlf(writing, s.window = "periodic", robust = TRUE,method = "naive")
autoplot(stlfwriting)

  #8
stlffancy<-stlf(fancy, s.window = "period", lambda = BoxCox.lambda(fancy),method= "rwdrift", robust = TRUE)
autoplot(stlffancy)
