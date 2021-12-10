# We illustrate the workflow using a multi-variate time series data set "tourism" that compiles 
# the quarterly number trips between pairs of destinations and segregate the trips according to multiple pusposes.

# The fable forecasting package relies on tidyverse functions,
# specifically dplyr functions, to manipulate data.  
# If you are not familiar with dplyr please consult "R for Data Science" 
# https://r4ds.had.co.nz/ 

library(fpp3)
D <- tourism %>% 
  group_by(Purpose) %>%
  summarize(Trips = sum(Trips))

DTR <- D %>% filter(Quarter <= yearquarter("2015 Q4"))
DTE <- D %>% filter(Quarter >= yearquarter("2016 Q1"))

autoplot(DTR,Trips) +
  autolayer(DTE, Trips, col="red") +
  labs(title = "Number of Trips",
       x = "Year Quarter") 

# We first fit a model automatically on the training set defined above

m <- DTR %>% 
  model(m.auto = ETS(Trips))

m %>% 
  tidy()

m %>% 
  filter(Purpose == "Holiday") %>% 
  components() %>%
  autoplot()

m %>% filter(Purpose == "Holiday") %>% gg_tsresiduals()



# Suppose that instead of fitting the model automatically, 
# we want to compare the automatic selection with a set of pre-specified 
# or partially sepecified models:
  

m <- DTR %>%  
  model(m.auto = ETS(Trips),
        m.ANA = ETS(Trips ~ error("A") + trend("N") + season("A")),
        m.ANM = ETS(Trips ~ error("A") + trend("N") + season("M")),
        m.MNM = ETS(Trips ~ error("M") + trend("N") + season("M")),
        m.AAA = ETS(Trips ~ error("A") + trend("A") + season("A")),
        m.ZAZ = ETS(Trips ~ trend("A")))

m %>%
  filter(Purpose == "Holiday") %>% 
  glance()    

m %>% 
  accuracy()

m %>% 
  select(m.auto) %>%
  filter(Purpose == "Holiday") %>%
  report()



# Models can be augmented by adding fitted and resifual values 
# for every observation in the training set

mg <- m %>% 
  augment()

mgH <- mg %>% 
  filter(Purpose == "Holiday") %>%
  filter(.model == "m.auto")

mgH %>%
  autoplot(.vars = Trips, col = "black") +
  geom_point(data = mgH, mapping = aes(y = .fitted))


#Creating forecasts and assessing forecasting accuracy

f <- m %>% 
  filter(Purpose == "Holiday") %>%
  forecast(h = 8)

DTE.H <- DTE %>% 
  filter(Purpose == "Holiday")

f %>% filter(Purpose == "Holiday") %>%
filter(.model == "m.auto") %>% autoplot(DTR) +
  geom_point(data = mgH, mapping = aes(y = .fitted), col = "blue") +
  geom_point(data = DTE.H, mapping = aes(y = Trips), col = "red")


# Examining In-Sample and Out-of-Sample Accuracy Statistics

rbind(m %>% filter(Purpose == "Holiday") %>% accuracy(), 
            f %>% accuracy(data = DTE))


# Extract Confidence Intervals

f %>% filter(Purpose == "Holiday") %>%
  filter(.model == "m.auto") %>%
  hilo(level =c(80,90)) %>%
  unpack_hilo("80%") %>%
  select(Quarter,"80%_lower", "80%_upper")
  

