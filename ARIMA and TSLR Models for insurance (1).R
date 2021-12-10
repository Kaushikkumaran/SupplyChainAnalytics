library(fpp3)
library(tseries)
insurance

autoplot(insurance)

insurance %>% 
  mutate(diff.Q = difference(Quotes),
         diff2.Q = difference(diff.Q)) -> D

# Examine Stationarity Visually
D %>% ACF(Quotes) %>% 
  autoplot() + 
  labs(title = "Quotes")
D %>% ACF(diff.Q) %>% 
  autoplot() + 
  labs(title = "diff.Q")
D %>% ACF(diff2.Q) %>% 
  autoplot() + 
  labs(title = "diff2.Q")

# Unit Root Test
D %>% features(Quotes, unitroot_kpss)
D %>% features(diff.Q, unitroot_kpss)
D %>% features(diff2.Q, unitroot_kpss)

D %>% features(Quotes, unitroot_ndiffs)


# ADF Test
D$Quotes%>% adf.test()

D$diff.Q %>%
  na.omit() %>%
  adf.test()

D$diff2.Q %>%
  na.omit() %>%
  adf.test()

D %>% gg_tsdisplay(Quotes, plot_type = "partial")  
D %>% gg_tsdisplay(diff.Q, plot_type = "partial")


m <- D %>%
  model(ma = ARIMA(Quotes),
        mlr = TSLM(Quotes ~ TVadverts))

m %>% select(ma) %>% report()
m %>% select(mlr) %>% report()

m %>% glance()

# Residual Testing
m %>% augment() %>%
  features(.resid, ljung_box, lag = 10)

m %>% select(ma) %>% gg_tsresiduals()
m %>% select(mlr) %>% gg_tsresiduals()

# Examine Information Criteria
m %>% glance()

# Examine Forecasts
m %>% select(ma) %>% 
  forecast(h=8) %>%
  autoplot(D) 


# Correlation between Quotes and TVadverts
plot(Quotes ~ TVadverts, data = D)
cor(D$Quotes, D$TVadverts)


m <- D %>% model(ma = ARIMA(Quotes),
                 mra = ARIMA(Quotes ~ TVadverts),
                 mrag1 = ARIMA(Quotes ~ TVadverts + pdq(2,1,0)),
                 mrag2 = ARIMA(Quotes ~ TVadverts + pdq(0,0,3)),
                 mrag3 = ARIMA(Quotes ~ TVadverts + pdq(2,1,3)))


m %>% glance() %>%
  select(.model, AIC, AICc, BIC)

m %>% select(mrag1) %>% report()

m %>% select(mrag1) %>%  residuals(type="regression") %>% 
  gg_tsdisplay(difference(.resid), "partial", lag_max = 16)

m %>% select(mrag2) %>%  residuals(type="regression") %>% 
  gg_tsdisplay(.resid, "partial", lag_max = 16)

m %>% 
  augment() %>%
  features(.resid, ljung_box, lag = 8)

# We select the (2,1,0) modelbased on IC

m %>% select(mra) %>% gg_tsresiduals()

# Now we use the (2,1,0) to forecast

new_advert_data <- new_data(D,8) %>%
  mutate(TVadverts = c(9,10,2,2,15,8,8,9))

m %>% select(mrag1) %>%
  forecast(new_data = new_advert_data) %>%
  autoplot() +
  geom_line(D, mapping = aes(y = Quotes))

#
# Adding lags to the advertising variable
#

X <- D %>% mutate(TV1 = lag(TVadverts),
                  TV2 = lag(TV1),
                  TV3 = lag(TV2),
                  TV4 = lag(TV3)) %>%
  filter(Month >= yearmonth("2002 May")) #Trim first rows with missing data

ml <- X %>% model(ma = ARIMA(Quotes),
                  mra = ARIMA(Quotes ~ TVadverts),
                  mra1 = ARIMA(Quotes ~ TVadverts + TV1),
                  mra2 = ARIMA(Quotes ~ TVadverts + TV1 + TV2),
                  mra3 = ARIMA(Quotes ~ TVadverts + TV1 + TV2 + TV3),
                  mra4 = ARIMA(Quotes ~ TVadverts + TV1 + TV2 + TV3 + TV4))

ml %>% glance()

ml %>% 
  augment() %>%
  features(.resid, ljung_box, lag = 8)

ml %>% select(mra) %>% report()
ml %>% select(mra1) %>% report()
ml %>% select(mra2) %>% report()
ml %>% select(mra3) %>% report()
ml %>% select(mra4) %>% report()

# What can we conclude regarding the lagged effects of demand?


