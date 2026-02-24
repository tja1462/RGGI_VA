# Initializing ------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(fixest)

fig_h <- 8
fig_w <- 11

eGRID <- read.csv("data/final_dataset.csv")

# Baseline: Treated v Both Controls ---------------------------------------
## interaction term 
eGRID <- eGRID %>%
  mutate(did = TREATED * POST,
         neigh_post = NEIGHBOR * POST,
         y_co2 = log1p(PLCO2AN),
         y_co2rt = log1p(PLCO2RTA))

m_co2   <- feols(y_co2 ~ did + neigh_post | ORISPL + YEAR, 
                 cluster = ~ORISPL, 
                 data = eGRID)
m_cap   <- feols(CAPFAC ~ did + neigh_post | ORISPL + YEAR, 
                 cluster = ~ORISPL, 
                 data = eGRID)
m_co2rt <- feols(y_co2rt ~ did + neigh_post | ORISPL + YEAR, 
                 cluster = ~ORISPL,
                 data = eGRID)

etable(m_co2, m_cap, m_co2rt)

# Handling Parallel Trend Violation ---------------------------------------

betahat <- summary(m_co2)$coefficients 
sigma <- summary(m_co2)$cov.scaled