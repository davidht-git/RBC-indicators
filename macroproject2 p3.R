#part 3 macro
#packages
library(tidyverse)
library(lubridate)
library(ggplot2)

#winsor function for cleaning data
winsor <- function(x, lo = 0.01, hi = 0.99) {
  ql <- quantile(x, lo, na.rm = TRUE)
  qh <- quantile(x, hi, na.rm = TRUE)
  x  <- pmin(pmax(x, ql), qh)
  x
}

#read in data
comp_raw <- read_csv("hi2t1mtxhdtwgfpt.csv", show_col_types = FALSE)

comp <- comp_raw %>%
#keeps only actives
  filter(costat == "A") %>%
# make time vars
  mutate(
    datadate = ymd(datadate),
    year     = year(datadate),
    qtr      = quarter(datadate)
  ) %>%
  arrange(gvkey, datadate)

#quarterly for capxy
comp <- comp %>%
  group_by(gvkey, year) %>%
  arrange(datadate, .by_group = TRUE) %>%
  mutate(
    capxy_lag = lag(capxy),
    capxq     = if_else(is.na(capxy_lag), capxy, capxy - capxy_lag),
    capxq     = if_else(capxq < 0, 0, capxq)
  ) %>%
  ungroup()

#variable creation
comp <- comp %>%
  group_by(gvkey) %>%
  arrange(datadate, .by_group = TRUE) %>%
  mutate(
# revenue growth y/y
    revtq_lag4      = lag(revtq, 4),
    rev_growth_yoy  = (revtq - revtq_lag4) / revtq_lag4,
    
# leverage (current + LT) / assets
    total_debt      = coalesce(dlcq, 0) + coalesce(dlttq, 0),
    leverage        = total_debt / atq,
    
# investment rate
    inv_rate        = capxq / atq,
    
# profitability variables
    roa             = niq / atq,
    roe             = niq / seqq
  ) %>%
  ungroup()

#make periods
comp <- comp %>%
  mutate(
    gfc_period = case_when(
      year <= 2007 ~ "pre",
      year %in% 2008:2009 ~ "gfc",
      year %in% 2010:2013 ~ "post",
      TRUE ~ "other"
    )
  )
#utilize winsorize
comp <- comp %>%
  mutate(
#drops outliers
    rev_growth_yoy = if_else(is.infinite(rev_growth_yoy), NA_real_, rev_growth_yoy),
    leverage       = if_else(is.infinite(leverage) | leverage < 0, NA_real_, leverage),
    inv_rate       = if_else(is.infinite(inv_rate) | inv_rate < 0, 0, inv_rate),
    inv_rate       = pmin(inv_rate, 1),  
# 0–100% of assets
    rev_growth_yoy_w = winsor(rev_growth_yoy, 0.05, 0.95),
    # normal winsor for the rest
    inv_rate_w       = winsor(inv_rate, 0.01, 0.99),
    leverage_w       = winsor(leverage, 0.01, 0.99),
    roa_w            = winsor(roa, 0.01, 0.99),
    roe_w            = winsor(roe, 0.01, 0.99)
  )

#summaries
period_print <- comp %>%
  summarise(
    n_obs             = n(),
    n_firms           = n_distinct(gvkey),
    rev_growth_yoy_pct = 100 * mean(rev_growth_yoy_w, na.rm = TRUE),
    inv_rate_pct       = 100 * mean(inv_rate_w,       na.rm = TRUE),
    leverage_pct       = 100 * mean(leverage_w,       na.rm = TRUE),
    roa_pct            = 100 * mean(roa_w,            na.rm = TRUE),
    roe_pct            = 100 * mean(roe_w,            na.rm = TRUE),
    .by = gfc_period
  ) %>%
  mutate(
    gfc_period = factor(gfc_period, levels = c("pre", "gfc", "post", "other"))
  ) %>%
  arrange(gfc_period)

# see it
period_print

# save it
write_csv(period_print, "period_summary_printable.csv")

#sectors
sector_print <- comp %>%
  summarise(
    n_firms            = n_distinct(gvkey),
    rev_growth_yoy_pct = 100 * mean(rev_growth_yoy_w, na.rm = TRUE),
    inv_rate_pct       = 100 * mean(inv_rate_w,       na.rm = TRUE),
    leverage_pct       = 100 * mean(leverage_w,       na.rm = TRUE),
    roa_pct            = 100 * mean(roa_w,            na.rm = TRUE),
    .by = c(gfc_period, gsector)
  ) %>%
  arrange(gfc_period, gsector)

write_csv(sector_print, "sector_period_printable.csv")

#print plots

# Investment
p_inv <- ggplot(period_print,
                aes(x = gfc_period, y = inv_rate_pct)) +
  geom_col() +
  labs(
    title = "Investment-to-Assets by Period",
    x = "Period",
    y = "Investment / Assets (%)"
  ) +
  theme_bw(base_size = 12)

ggsave("fig_inv_rate.png", p_inv, width = 6, height = 4, dpi = 300)

#Revenue growth
p_rev <- ggplot(period_print,
                aes(x = gfc_period, y = rev_growth_yoy_pct)) +
  geom_col() +
  labs(
    title = "Revenue Growth (y/y) by Period",
    x = "Period",
    y = "Revenue growth (%)"
  ) +
  theme_bw(base_size = 12)

ggsave("fig_rev_growth.png", p_rev, width = 6, height = 4, dpi = 300)

# Leverage
p_lev <- ggplot(period_print,
                aes(x = gfc_period, y = leverage_pct)) +
  geom_col() +
  labs(
    title = "Leverage by Period",
    x = "Period",
    y = "Debt / Assets (%)"
  ) +
  theme_bw(base_size = 12)

ggsave("fig_leverage.png", p_lev, width = 6, height = 4, dpi = 300)

period_roa_med <- comp %>%
  summarise(roa_med_pct = 100 * median(roa_w, na.rm = TRUE), .by = gfc_period)


period_ro <- period_print %>%   
  dplyr::select(gfc_period, roa_pct, roe_pct)
#unnused section
p_ro <- ggplot2::ggplot(period_ro, aes(x=gfc_period, y=roa_pct)) +
  ggplot2::geom_col() +
  ggplot2::labs(title="ROA by Period", x="Period", y="ROA (%)") +
  ggplot2::theme_bw(base_size = 12)
ggplot2::ggsave("fig_roa_by_period.png", p_ro, width=6, height=4, dpi=300)





# name sectors (edit labels if you prefer)
gics_names <- tibble::tribble(
  ~gsector, ~gics_name,
  10, "Energy/Materials", 15, "Materials/Industrials", 20, "Industrials",
  25, "Consumer Disc.",   30, "Consumer Staples",      35, "Health Care",
  40, "Financials",       45, "Info Tech",             50, "Comm",
  55, "Utilities",        60, "Real Estate",           NA, "Unclassified"
)

sector_named <- sector_print %>%
  dplyr::left_join(gics_names, by = "gsector") %>%
  dplyr::mutate(
    gics_name = dplyr::if_else(is.na(gics_name), "Unclassified", gics_name),
    gfc_period = factor(gfc_period, levels = c("pre","gfc"))
  )

p_sector_inv <- sector_named %>%
  dplyr::filter(gfc_period %in% c("pre","gfc")) %>%
  ggplot2::ggplot(aes(x = gics_name, y = inv_rate_pct, fill = gfc_period)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::labs(title = "Investment-to-Assets by Sector: Pre vs GFC",
                x = "Sector", y = "Investment / Assets (%)", fill = "Period") +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot2::ggsave("fig_sector_investment_pre_gfc.png", p_sector_inv, width = 8, height = 5, dpi = 300)








period_ro <- period_print %>%
  dplyr::select(gfc_period, roa_pct, roe_pct)

p_ro <- ggplot2::ggplot(period_ro, aes(x=gfc_period, y=roa_pct)) +
  ggplot2::geom_col() +
  ggplot2::labs(title="ROA by Period", x="Period", y="ROA (%)") +
  ggplot2::theme_bw(base_size = 12)
ggplot2::ggsave("fig_roa_by_period.png", p_ro, width=6, height=4, dpi=300)
