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

SHORTTON_TO_METRIC <- 0.90718474

to_mmt <- function(x) {
  x * SHORTTON_TO_METRIC / 1e6
}

eGRID <- read.csv("data/final_dataset.csv")

# Visualizations -----------------------------------------------------------
treat_c <- eGRID %>%
  mutate(group = case_when(
    TREATED == 1 ~ "Treatment",
    NEIGHBOR == 1 ~ "Neighbor Control",
    TRUE ~ "Far Control"
  )) %>%
  mutate(group = factor(group, 
                        levels = c("Far Control","Neighbor Control","Treatment"))
   ) %>%
  group_by(group, YEAR) %>%
  summarise(mean_co2 = to_mmt(mean(PLCO2AN)),
            mean_co2rt = mean(PLCO2RTA),
            mean_capfac = mean(CAPFAC)
            ) %>% 
  ungroup()

co2_em <- ggplot(treat_c, aes(x = YEAR, 
                    y = mean_co2, 
                    color = group)
                 ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2021, 
             linetype = "dashed") +
  labs(title = "Average CO2 Emissions per Plant",
       y = "CO2 Emission (Million Metric Tons)", 
       x = "Year", 
       color = "Group")
co2_em
ggsave('figures/co2em_pt.png', plot = co2_em,
       width = fig_w, height = fig_h)

co2_rate <- ggplot(treat_c, aes(x = YEAR, 
                              y = mean_co2rt, 
                              color = group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2021, 
             linetype = "dashed") +
  labs(title = "Average Plant Carbon Intensity",
       y = "CO2 Emission (lb/MWh)", 
       x = "Year", 
       color = "Group")
co2_rate
ggsave('figures/CO2rate_pt.png', plot = co2_rate,
       width = fig_w, height = fig_h)

co2_capfac <- ggplot(treat_c, aes(x = YEAR, 
                              y = mean_capfac, 
                              color = group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2021, 
             linetype = "dashed") +
  labs(title = "Mean Plant Capacity Factor",
       y = "Capacity Factor", 
       x = "Year", 
       color = "Group")
co2_capfac 
ggsave('figures/capfac_pt.png', plot = co2_capfac,
       width = fig_w, height = fig_h)

# Event Study Regressions -------------------------------------------------

ESGRID <- eGRID %>% 
  mutate(rel_year = YEAR - 2021)

es_co2 <- feols(
  log1p(PLCO2AN) ~ i(rel_year, TREATED, ref = -1) + i(rel_year, NEIGHBOR, ref = -1) | ORISPL + YEAR,
  cluster = ~ORISPL,
  data = ESGRID
)
etable(es_co2)
iplot(
  es_co2,
  ref.line = -0.5,
  xlab = "Years relative to 2021",
  ylab = "Coefficient",
  main = "Event-study: Effect on CO2 Emissions"
)

es_co2rt <- feols(
  log1p(PLCO2RTA) ~ i(rel_year, TREATED, ref = -1) + i(rel_year, NEIGHBOR, ref = -1)| ORISPL + YEAR,
  cluster = ~ORISPL,
  data = ESGRID
)
etable(es_co2rt)
iplot(
  es_co2rt,
  ref.line = -0.5,
  xlab = "Years relative to 2021",
  ylab = "Coefficient",
  main = "Event-study: Effect on CO2 Output Rate"
)

es_cf <- feols(
  CAPFAC ~ i(rel_year, TREATED, ref = -1) + i(rel_year, NEIGHBOR, ref = -1)| ORISPL + YEAR,
  cluster = ~ORISPL,
  data = ESGRID
)
etable(es_cf)
iplot(
  es_cf ,
  ref.line = -0.5,
  xlab = "Years relative to 2021",
  ylab = "Coefficient",
  main = "Event-study: Effect on Capacity Factor"
)

# HonestDiD Violation Analysis -------------------------------------------

