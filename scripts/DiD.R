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

did_table <- etable(m_co2, m_cap, m_co2rt)
did_table
did_lines <- capture.output(print(did_table))
n <- length(did_lines)
y_pos <- seq(0.985, 0.02, length.out = n)

# PDF
grDevices::pdf("figures/did_table.pdf", width = 13, height = max(4, 0.25 * n))
graphics::par(mar = c(0.4, 0.4, 0.4, 0.4), family = "mono")
graphics::plot.new()
for (i in seq_along(did_lines)) {
  graphics::text(0.01, y_pos[i], did_lines[i], adj = c(0, 1), cex = 0.82)
}
grDevices::dev.off()
