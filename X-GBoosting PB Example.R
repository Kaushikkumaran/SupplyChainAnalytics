library(tidyverse)
library(Matrix)
library(xgboost)


d <- read_csv("PB Sales.csv")
D <- d %>% mutate(IRI_KEY = as.factor(IRI_KEY),
                  F   = as.factor(F),
                  D   = as.factor(D),
                  PR  = as.factor(PR),
                  UPC = as.factor(UPC),
                  TEXTURE = as_factor(TEXTURE),
                  TYPE    = as_factor(TYPE),
                  FLAVOR  = as_factor(FLAVOR),
                  LP  = log1p(PPOZ),
                  LV = log1p(UNITS * VOL_EQ * 16)) %>%
  select(-SY,-GE, -Market_Name, -Open, -Clsd, -PPOZ, -PPU)
                 
unique(D$IRI_KEY)
unique(D$UPC)

#
# This dataset contains retail sales of the 
# peanut butter category in the Chicago market.
#
# The data set includes 35 x 170 = 5,950 
# store x product combinations
#

#
# FEATURE ENGINEERING:
#

#
# Include lagged demand history to 
# account for auto-correlation effects
# a non-linear AR(12) effect in this example
#

LY <- D %>% select(IRI_KEY, WEEK, UPC, LV)
LDEM <- data.frame(IRI_KEY = NULL, WEEK = NULL, UPC = NULL,
                   Y1 = NULL, Y2 = NULL, Y3 = NULL,
                   Y4 = NULL, Y5 = NULL, Y6 = NULL,
                   Y7 = NULL, Y8 = NULL, Y9 = NULL,
                   Y10 = NULL, Y11 = NULL, Y12 = NULL
                   )

U.Stores <- unique(D$IRI_KEY)
U.Prods  <- unique(D$UPC)

for(s in U.Stores){
  for(p in U.Prods){
    Y <- LY %>% filter(IRI_KEY == s, UPC == p)
    X <- data.frame(WEEK = 1635:1686)
    X <- left_join(X,Y, by = "WEEK") %>%
      mutate(Y1 = lag(LV),
             Y2 = lag(LV,2),
             Y3 = lag(LV,3),
             Y4 = lag(LV,4),
             Y5 = lag(LV,5),
             Y6 = lag(LV,6),
             Y7 = lag(LV,7),
             Y8 = lag(LV,8),
             Y9 = lag(LV,9),
             Y10 = lag(LV,10),
             Y11 = lag(LV,11),
             Y12 = lag(LV,12),
      )
    
    LDEM <- rbind(LDEM,X)
  }
}
LDEM <- LDEM %>%
  select(-LV)

DS <- D %>% left_join(LDEM, by =c("WEEK", "IRI_KEY", "UPC")) %>%
  select(-VEND,-ITEM,-UNITS,-DOLLARS,-VOL_EQ)
  

#
# Including Competitive SKU's Parameters
# as Predicitve Model Features
#

#
# Create variables related to the lowest-price
# product in the product category and then
# join to the dataset by WEEK and store
#

DS %>% group_by(WEEK, IRI_KEY) %>%
  summarize(MIN_P = min(LP),
            NUM = n_distinct(UPC),
            IX    = min(which(MIN_P == LP)),
            MIN_UPC = UPC[IX],
            MIN_F   = F[IX],
            MIN_D   = D[IX],
            MIN_PR  = PR[IX]) %>%
  select(-IX) -> DSX1

DS2 <- DS %>% 
  left_join(DSX1, by =c("WEEK", "IRI_KEY"))


#
# Create variables related to the lowest-price
# product in the TEXTURE sub-category and then
# join to the dataset by WEEK, TEXTURE type and store
#

DS2 <- DS2 %>%
  mutate(TXTR = ifelse(TEXTURE == "CRUNCHY" |
                          TEXTURE == "EXTRA CRUNCHY" |
                          TEXTURE == "CHUNKY" |
                          TEXTURE == "EXTRA CHUNKY" |
                          TEXTURE == "SUPER CHUNKY",
                        "CR","NCR"),
         TXTR = as.factor(TXTR)
         )

DS %>% filter(TEXTURE == "CRUNCHY" |
                TEXTURE == "EXTRA CRUNCHY" |
                TEXTURE == "CHUNKY" |
                TEXTURE == "EXTRA CHUNKY" |
                TEXTURE == "SUPER CHUNKY") %>%
  group_by(WEEK, IRI_KEY) %>%
  summarize(TXTR    = as.factor("CR"),
            MIN_P_T = min(LP),
            NUM_T = n_distinct(UPC),
            IX    = min(which(MIN_P_T == LP)),
            MIN_UPC_T = UPC[IX],
            MIN_F_T   = F[IX],
            MIN_D_T   = D[IX],
            MIN_PR_T  = PR[IX]) %>%
  select(-IX) -> DSX2

DS %>% filter(TEXTURE != "CRUNCHY",
                TEXTURE != "EXTRA CRUNCHY",
                TEXTURE != "CHUNKY",
                TEXTURE != "EXTRA CHUNKY",
                TEXTURE != "SUPER CHUNKY") %>%
  group_by(WEEK, IRI_KEY) %>%
  summarize(TXTR    = as.factor("NCR"),
            MIN_P_T = min(LP),
            NUM_T = n_distinct(UPC),
            IX    = min(which(MIN_P_T == LP)),
            MIN_UPC_T = UPC[IX],
            MIN_F_T   = F[IX],
            MIN_D_T   = D[IX],
            MIN_PR_T  = PR[IX]) %>%
  select(-IX) -> DSX3

DS2 <- DS2 %>% 
  left_join(rbind(DSX2,DSX3), by =c("WEEK", "IRI_KEY", "TXTR"))

#
# Create variables related to the lowest-price
# product in the FLAVOR sub-category and then
# join to the dataset by WEEK, FLAVOR type and store
#

DS2 <- DS2 %>%
  mutate(FLVR = ifelse((FLAVOR == "REGULAR" |
                       FLAVOR == "ORIGINAL" |
                       FLAVOR == "MISSING"),
                       "R","NR"),
         FLVR = as.factor(FLVR)
  )

DS %>% filter(FLAVOR == "REGULAR" |
              FLAVOR == "ORIGINAL" |
              FLAVOR == "MISSING") %>%
  group_by(WEEK, IRI_KEY) %>%
  summarize(FLVR    = as.factor("R"),
            MIN_P_F = min(LP),
            NUM_F = n_distinct(UPC),
            IX    = min(which(MIN_P_F == LP)),
            MIN_UPC_F = UPC[IX],
            MIN_F_F   = F[IX],
            MIN_D_F   = D[IX],
            MIN_PR_F  = PR[IX]) %>%
  select(-IX) -> DSX4

DS %>% filter(FLAVOR != "REGULAR",
                FLAVOR != "ORIGINAL",
                FLAVOR != "MISSING") %>%
  group_by(WEEK, IRI_KEY) %>%
  summarize(FLVR    = as.factor("NR"),
            MIN_P_F = min(LP),
            NUM_F = n_distinct(UPC),
            IX    = min(which(MIN_P_F == LP)),
            MIN_UPC_F = UPC[IX],
            MIN_F_F   = F[IX],
            MIN_D_F   = D[IX],
            MIN_PR_F  = PR[IX]) %>%
  select(-IX) -> DSX5

DS2 <- DS2 %>% 
  left_join(rbind(DSX4,DSX5), by =c("WEEK", "IRI_KEY", "FLVR"))
 
#
# Create Learning and Testing sets
#

DS.tr <- DS2 %>% filter(WEEK <= 1682)
DS.te <- DS2 %>% filter(WEEK >= 1683)

#
# xgboost() requires data to be hot-one coded,
# hence we use model.matrix() to process
# factor-valued variables.
#
# NA's present no problem, so we opt to keep
# rows with missing data
#

options(na.action='na.pass')
x.tr <- model.matrix(LV ~ ., data = DS.tr)[,-1]
x.te <- model.matrix(LV ~ ., data = DS.te)[,-1]
y.tr <- DS2 %>% filter(WEEK <= 1682) %>% select(LV) 
y.te <- DS2 %>% filter(WEEK >= 1683) %>% select(LV)

#
# Optimize xgboost() parameters using an 
# accuracy metric (i.e. MAPE) obtained from
# a sequential cross-validation procees on 
# the training data set (NOT SHOWN HERE)
# For expediency in class will play only with 
# training-testing accuracy, which illustrates 
# that xgboost() is very flexible and hence
# it is capable of grossly overfitting the training data
#

set.seed(1)
xb <- xgboost(x.tr, y.tr$LV,
              learning_rate = .05,      # Boosting Parameters
              nround=20,                # Boosting Parameters
              lambda = 0,               # Regularization Parameters
              max_depth = 3,            # Regularization Parameters
              subsample = 1,            # Boosting-like Parameter
              colsample_bytree = 1,     # RF-like Parameter
              colsample_bylevel = 1)    # RF-like Parameter

y_hat <- predict(xb, x.te)
y_fit <- predict(xb, x.tr)
sqrt(mean((y.te$LV - y_hat)^2))
# In-Sample MAPE, Test MAPE
c(mean(abs(y.tr$LV - y_fit)/y.tr$LV),
  mean(abs(y.te$LV - y_hat)/y.te$LV))

plot(y_fit ~ y.tr$LV, xlim=c(0,10), ylim=c(0,10))
abline(0,1)
points(y_hat ~ y.te$LV, col = "red")
