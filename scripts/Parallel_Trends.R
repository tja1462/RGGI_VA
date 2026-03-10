## Parallel Trends Assumption

# Initializing ------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(patchwork)
library(ggmap)
library(scales)
library(fixest)
library(HonestDiD)
library(purrr)

fig_h <- 8
fig_w <- 11

SHORTTON_TO_METRIC <- 0.90718474

to_mmt <- function(x) {
  x * SHORTTON_TO_METRIC / 1e6
}

eGRID <- read.csv('data/final_dataset.csv')

# Visualizations -----------------------------------------------------------
treat_c <- eGRID %>%
  mutate(group = case_when(
    TREATED == 1 ~ 'Treatment',
    NEIGHBOR == 1 ~ 'Neighbor Control',
    TRUE ~ 'Far Control'
  )) %>%
  mutate(group = factor(group, 
                        levels = c('Far Control','Neighbor Control','Treatment'))
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
             linetype = 'dashed') +
  labs(title = 'Average CO2 Emissions per Plant',
       y = 'CO2 Emission (Million Metric Tons)', 
       x = 'Year', 
       color = 'Group') +
  theme_bw(base_size = 12) +
  theme(axis.text = element_text(size = 12),
        text = element_text(face = "bold")) 
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
             linetype = 'dashed') +
  labs(title = 'Average Plant Carbon Intensity',
       y = 'CO2 Emission (lb/MWh)', 
       x = 'Year', 
       color = 'Group') + 
  theme_bw(base_size = 12) +
  theme(axis.text = element_text(size = 12),
        text = element_text(face = "bold")) 
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
             linetype = 'dashed') +
  labs(title = 'Mean Plant Capacity Factor',
       y = 'Capacity Factor', 
       x = 'Year', 
       color = 'Group') +
  theme_bw(base_size = 12) +
  theme(axis.text = element_text(size = 12),
        text = element_text(face = "bold")) 
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
png('figures/es_co2.png', width = fig_w, height = fig_h, 
    units = 'in', res = 300, pointsize = 14)
iplot(
  es_co2,
  ref.line = -0.5,
  xlab = 'Years relative to 2021',
  ylab = 'Coefficient',
  main = 'Yearly Effect on CO2 Emissions',
  pt.cex = 1.8,
  ci.lwd = 2,
  plot_prms = list(cex.axis = 1, font.axis = 2)
)
dev.off()

es_co2rt <- feols(
  log1p(PLCO2RTA) ~ i(rel_year, TREATED, ref = -1) + i(rel_year, NEIGHBOR, ref = -1)| ORISPL + YEAR,
  cluster = ~ORISPL,
  data = ESGRID
)
png('figures/es_co2rt.png', width = fig_w, height = fig_h,
    units = 'in', res = 300, pointsize = 14)
iplot(
  es_co2rt,
  ref.line = -0.5,
  xlab = 'Years relative to 2021',
  ylab = 'Coefficient',
  main = 'Yearly Effect on CO2 Output Rate',
  pt.pch = 1,
  ci.lwd = 2,
  plot_prms = list(cex.axis = 1, font.axis = 2)
)
dev.off()

es_cf <- feols(
  CAPFAC ~ i(rel_year, TREATED, ref = -1) + i(rel_year, NEIGHBOR, ref = -1)| ORISPL + YEAR,
  cluster = ~ORISPL,
  data = ESGRID
)
png('figures/es_cf.png', width = fig_w, height = fig_h, 
    units = 'in', res = 300, pointsize = 14)
iplot(
  es_cf ,
  ref.line = -0.5,
  xlab = 'Years relative to 2021',
  ylab = 'Coefficient',
  main = 'Event-study: Effect on Capacity Factor',
  pt.cex = 1.8,
  ci.lwd = 2,
  plot_prms = list(cex.axis = 1, font.axis = 2)
)
dev.off()
etable(es_co2, es_co2rt, es_cf)

# HonestDiD Violation Analysis -------------------------------------------
extract_treated_event_study <- function(model, treated_suffix = ':TREATED$') {
  betahat_all <- coef(model)
  sigma_all <- vcov(model)

  keep <- grepl(treated_suffix, names(betahat_all))
  if (!any(keep)) {
    stop('No treated event-study coefficients found. Check model terms.')
  }

  betahat <- betahat_all[keep]
  sigma <- sigma_all[keep, keep, drop = FALSE]

  event_time <- as.integer(
    sub(treated_suffix, '', sub('.*::', '', names(betahat)))
  )

  ord <- order(event_time)
  betahat <- betahat[ord]
  sigma <- sigma[ord, ord, drop = FALSE]
  event_time <- event_time[ord]

  numPrePeriods <- sum(event_time < 0)
  numPostPeriods <- sum(event_time >= 0)

  if (numPrePeriods < 1 || numPostPeriods < 1) {
    stop('HonestDiD needs at least one pre period and one post period.')
  }

  list(
    betahat = betahat,
    sigma = sigma,
    event_time = event_time,
    numPrePeriods = numPrePeriods,
    numPostPeriods = numPostPeriods
  )
}

run_honest_did <- function(model, mbar_grid = seq(0, 2, by = 0.25)) {
  es <- extract_treated_event_study(model)

  # Target estimand: treated effect in the first post-treatment period.
  l_vec <- c(1, rep(0, es$numPostPeriods - 1))

  original <- constructOriginalCS(
    betahat = es$betahat,
    sigma = es$sigma,
    numPrePeriods = es$numPrePeriods,
    numPostPeriods = es$numPostPeriods,
    l_vec = l_vec
  )

  sensitivity <- createSensitivityResults_relativeMagnitudes(
    betahat = es$betahat,
    sigma = es$sigma,
    numPrePeriods = es$numPrePeriods,
    numPostPeriods = es$numPostPeriods,
    l_vec = l_vec,
    Mbarvec = mbar_grid
  )

  original <- mutate(
    original,
    lb = as.numeric(lb),
    ub = as.numeric(ub)
  )

  sensitivity <- mutate(
    sensitivity,
    lb = as.numeric(lb),
    ub = as.numeric(ub)
  )

  list(original = original, sensitivity = sensitivity)
}

honest_models <- list(
  co2_emissions = es_co2,
  co2_rate = es_co2rt,
  capacity_factor = es_cf
)

honest_model_titles <- c(
  co2_emissions = 'CO2 emissions',
  co2_rate = 'CO2 rate',
  capacity_factor = 'Capacity Factor'
)

honest_results <- imap(honest_models, function(model, model_name) {
  res <- run_honest_did(model)
  display_name <- honest_model_titles[model_name]
  if (is.na(display_name)) {
    display_name <- model_name
  }

  p <- createSensitivityPlot_relativeMagnitudes(
    robustResults = res$sensitivity,
    originalResults = res$original
  ) +
    labs(
      title = paste(display_name, 'sensitivity analysis for first post period:'),
      x = 'Mbar',
      y = '95% confidence'
    ) +
    theme_bw(base_size = 12) +
    theme(axis.text = element_text(size = 12),
          legend.position = "none",
          text = element_text(face = "bold")) 
  

  ggsave( paste0('figures/honestdid_', model_name, '.png'),
    plot = p,
    width = fig_w,
    height = fig_h
  )

  res
})
