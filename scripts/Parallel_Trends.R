## Parallel Trends Assumption

# Initializing ------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(patchwork)
library(ggmap)
library(scales)
library(fixest)

fig_h <- 8
fig_w <- 11

eGRID <- read.csv("data/final_dataset.csv")

# Visualizations -----------------------------------------------------------
treat_c <- eGRID %>%
  mutate(TREATED = factor(TREATED,
                          levels = c(0, 1),
                          labels = c("Control", "Treated")
   )) %>%
  group_by(TREATED, YEAR) %>%
  summarise(mean_co2 = mean(PLCO2AN, na.rm = TRUE),
            mean_co2rt = mean(PLCO2RTA, na.rm = TRUE),
            mean_capfac = mean(CAPFAC, na.rm = TRUE)
            ) %>% 
  ungroup()

co2_em <- ggplot(treat_c, aes(x = YEAR, 
                    y = mean_co2, 
                    color = TREATED)
                 ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2021, 
             linetype = "dashed") +
  labs(title = "Average CO2 Emissions per Plant",
       y = "CO2 Emission (tons)", 
       x = "Year", 
       color = "Group")
co2_em

co2_rate <- ggplot(treat_c, aes(x = YEAR, 
                              y = mean_co2rt, 
                              color = TREATED)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2021, 
             linetype = "dashed") +
  labs(title = "Average Plant Carbon Intensity",
       y = "CO2 Emission (tons)", 
       x = "Year", 
       color = "Group")
co2_rate

co2_capfac <- ggplot(treat_c, aes(x = YEAR, 
                              y = mean_capfac, 
                              color = TREATED)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2021, 
             linetype = "dashed") +
  labs(title = "Plant Mean Capacity Factor",
       y = "Capacity Factor", 
       x = "Year", 
       color = "Group")
co2_capfac 

# Event Study Regressions -------------------------------------------------

ESGRID <- eGRID %>% 
  mutate(treated = POST == 1 & TREATED == 1,
         rel_year = YEAR - 2021 )

es_co2 <- feols(
  log1p(PLCO2AN) ~ i(rel_year, TREATED, ref = -1) | ORISPL + YEAR,
  cluster = ~ORISPL,
  data = ESGRID
)
etable(es_co2)
iplot(
  es_co2,
  ref.line = 0,
  xlab = "Years relative to 2021",
  ylab = "Effect on CO2 Emissions",
  main = "Event-study"
)