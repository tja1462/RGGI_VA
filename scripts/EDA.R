
# Intro -------------------------------------------------------------------
## Name: Trixie Joy Adlaon
## Project: QSS 82 Virginia and RGGI

# Initializing ------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(patchwork)
library(ggmap)
library(scales)

fig_h <- 8
fig_w <- 11

SHORTTON_TO_METRIC <- 0.90718474

to_mmt <- function(x) {
  x * SHORTTON_TO_METRIC / 1e6
}

# Loading Data ------------------------------------------------------------

eGRID <- read.csv('data/final_dataset.csv')
eGRID

dim(eGRID)
summary(eGRID)

# CO2 Emissions (fuel type)------------------------------------------------------------

co2_yr_fuel = eGRID %>% 
  group_by(YEAR, PLFUELCT) %>% 
  summarize(total_co2_fuel = to_mmt(sum(PLCO2AN)))
co2_yr_fuel 

co2_yr <- eGRID %>% 
  group_by(YEAR) %>%
  summarize(total_co2 = to_mmt(sum(PLCO2AN)))

mean_co2 <- mean(co2_yr$total_co2)

co2_per_year <- ggplot() +
  geom_bar(data= co2_yr_fuel, 
           mapping= aes(x = YEAR, 
                        y = total_co2_fuel,
                        fill = PLFUELCT),
           stat='identity') + 
  geom_hline(yintercept = mean_co2, 
             linetype = 'dashed', 
             color = 'blue') +
  annotate('text',
           y= mean_co2,
           x = 2020,
           label= paste('Mean CO2:',mean_co2),
           color='blue') +
  scale_y_continuous(breaks = breaks_width(100)) +
  scale_x_continuous(breaks = breaks_width(1)) + 
  labs(title = 'Total CO2 Emissions by Fossil Plants (2018 - 2023)',
       x = 'Year',
       y = 'CO2 (million metric tons)',
       fill = 'Fuel Type',
       caption = 'Source: EPA eGRID, plant-level data (2018–2023)') +
  theme_few()
co2_per_year

ggsave('figures/yearly_co2_fuelstacked.png', plot=co2_per_year, 
       width = fig_w, height = fig_h)

# CO2 Emissions (STATE) ------------------------------------------------------

state_co2_yr <- eGRID %>% 
  group_by(YEAR, PSTATABB) %>% 
  summarize(total_co2_mil = to_mmt(sum(PLCO2AN)))

total_state <- ggplot() +
  geom_bar(data= state_co2_yr,
           mapping = aes(x = YEAR,
                         y = total_co2_mil,
                         fill = PSTATABB),
           stat='identity',
           alpha = 0.8
  ) +
  labs(title = 'Total CO2 Emissions by State (2018 - 2023)',
       y= 'CO2 (million metric tons,',
       x = 'Year',
       fill = 'States (abbv)',
       caption = 'Source: EPA eGRID, plant-level data (2018–2023)') +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  theme_few()
total_state

ggsave('figures/yearly_co2_statestacked.png', plot = total_state,
       width = fig_w, height = fig_h)

# Yearly CO2 Emissions (State) ---------------------------------------------

state_co2_graph <- ggplot(
  data = state_co2_yr, 
  mapping = aes(x = YEAR, 
                y = total_co2_mil,
                group = PSTATABB)
  ) +
  geom_line(mapping = aes(color = PSTATABB)) +
  geom_point(mapping = aes(shape = PSTATABB),
           size = 1) +
  labs(title = 'State CO2 Emissions (2018 - 2023)',
       y = 'CO2 in Million Metric Tons',
       x = 'Year',
       color = 'State Abbrv',
      shape = 'State Abbrv',
      caption = 'Source: EPA eGRID, plant-level data (2018–2023)') +
  theme_few()
state_co2_graph

ggsave('figures/totalco2_by_state.png', plot=state_co2_graph, 
       width = fig_w, height = fig_h)

# Fuel Type (2)  --------------------------------------------------------------

fuel_bar <- ggplot(eGRID, aes(x = PLFUELCT)) +
  geom_bar(fill = 'purple',
           color = 'black'
           ) + 
  geom_text( stat = 'count',
             aes(label = ..count..),
             vjust = -1
            )+
  labs(title = 'Fuel Categories Count',
       x = 'Fuel Type',
       y = 'Count',
       caption = 'Source: EPA eGRID, plant-level data (2018–2023)') +
  theme_few()
fuel_bar
ggsave('figures/fuel_count.png', plot = fuel_bar,
       width = fig_w, height = fig_h)

fuel_box <- ggplot(data = eGRID, 
                   mapping = aes(x = PLFUELCT,
                                 y = CAPFAC)) +
  geom_boxplot() + 
  labs(title = 'Capacity Factor of Different Fuel Types',
      x = 'Capacity Factor',
      y = 'Fuel',
      caption = 'Source: EPA eGRID, plant-level data (2018–2023)') +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  coord_flip() +
  theme_few()
fuel_box

ggsave('figures/fuel_boxplot.png', plot = fuel_box,
       width = fig_w, height = fig_h)

# Treatment v Control Pre -------------------------------------------------
treat_c <- eGRID %>%
  mutate(group = case_when(
    TREATED == 1 ~ 'Treatment',
    NEIGHBOR == 1 ~ 'Neighbor Control',
    TRUE ~ 'Far Control'
  )) %>%
  mutate(group = factor(group, 
                        levels = c('Far Control','Neighbor Control','Treatment')))
# treat_c <- eGRID %>%
#   filter(POST == 0) %>% 
#   mutate(TREATED = factor(TREATED, 
#                           levels = c(0, 1), 
#                           labels = c('Control', 'Treated')
#                           )
#          )

co2_rate <- ggplot(treat_c, aes(x = PLFUELCT, y = PLCO2RTA)) +
  geom_boxplot(outlier.shape = 1) +
  stat_summary(
    fun = median,
    geom = 'text',
    aes(label = round(after_stat(y), 0)),
    fontface = 'bold', 
    vjust = -2.5,
    color = 'red',
    size = 4
  ) +
  coord_flip() +
  facet_grid(group ~ .) +
  labs(
    title = 'Carbon Intensity of Fuel Types Pre-Treatment',
    x = 'Fuel',
    y = 'CO2 emission rate (lb/MWh)',
    caption = 'Source: EPA eGRID, plant-level data (2018–2021)'
  ) +
  scale_y_log10() +
  theme_few()
co2_rate
ggsave('figures/CarbonIntensity_TvC.png', plot = co2_rate,
       width = fig_w, height = fig_h)

cf <- ggplot(treat_c, aes(x = PLFUELCT,
                          y = CAPFAC)) +
  geom_boxplot(outlier.shape = 1) + 
  labs(title = 'Capacity Factor of Fuel Types Pre Treatment',
       x = 'Capacity Factor',
       y = 'Fuel',
       caption = 'Source: EPA eGRID, plant-level data (2018–2021)') +
  facet_grid(group ~ .) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  coord_flip() +
  theme_few()
cf
ggsave('figures/CapFac_TvC.png', plot = cf,
       width = fig_w, height = fig_h)
