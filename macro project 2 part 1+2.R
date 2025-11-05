#data is from 2000 onward
#packages
rm(list = ls())

pkgs <- c("tidyverse","lubridate","tsibble","zoo","janitor","gt","scales","stringr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))
#read data
JPNRGDPEXP      <- read_csv("JPNRGDPEXP.csv",      show_col_types = FALSE)
NGDPRSAXDCDEQ   <- read_csv("NGDPRSAXDCDEQ.csv",   show_col_types = FALSE)
GDPC1           <- read_csv("GDPC1.csv",           show_col_types = FALSE)
LMJVTTNVJPQ647S <- read_csv("LMJVTTNVJPQ647S.csv", show_col_types = FALSE)
LMJVTTUVDEQ647S <- read_csv("LMJVTTUVDEQ647S.csv", show_col_types = FALSE)
LMJVTTUVUSM647S <- read_csv("LMJVTTUVUSM647S.csv", show_col_types = FALSE)
SPASTT01DEQ661N <- read_csv("SPASTT01DEQ661N.csv", show_col_types = FALSE)
SPASTT01JPQ661N <- read_csv("SPASTT01JPQ661N.csv", show_col_types = FALSE)
SPASTT01USQ661N <- read_csv("SPASTT01USQ661N.csv", show_col_types = FALSE)
CPALTT01USM659N <- read_csv("CPALTT01USM659N.csv", show_col_types = FALSE)
CPALTT01DEM659N <- read_csv("CPALTT01DEM659N.csv", show_col_types = FALSE)
CPALTT01JPM659N <- read_csv("CPALTT01JPM659N.csv", show_col_types = FALSE)

#reader function
read_std <- function(df, sid) {
  nm <- names(df)
  date_col  <- nm[tolower(nm) %in% c("date","observation_date","time","period")]
  if (!length(date_col)) date_col <- nm[1]
  value_col <- nm[tolower(nm) %in% c("value","values","index","observation_value", tolower(sid))]
  if (!length(value_col)) value_col <- nm[length(nm)]
  tibble(
    series_id = sid,
    date      = as_date(df[[date_col]]),
    value     = readr::parse_number(as.character(df[[value_col]]))
  ) |> filter(!is.na(date))
}

#setupdata table
data_long <- bind_rows(
  read_std(JPNRGDPEXP,      "JPNRGDPEXP"),
  read_std(NGDPRSAXDCDEQ,   "NGDPRSAXDCDEQ"),
  read_std(GDPC1,           "GDPC1"),
  read_std(LMJVTTNVJPQ647S, "LMJVTTNVJPQ647S"),
  read_std(LMJVTTUVDEQ647S, "LMJVTTUVDEQ647S"),
  read_std(LMJVTTUVUSM647S, "LMJVTTUVUSM647S"),
  read_std(SPASTT01DEQ661N, "SPASTT01DEQ661N"),
  read_std(SPASTT01JPQ661N, "SPASTT01JPQ661N"),
  read_std(SPASTT01USQ661N, "SPASTT01USQ661N"),
  read_std(CPALTT01USM659N, "CPALTT01USM659N"),
  read_std(CPALTT01DEM659N, "CPALTT01DEM659N"),
  read_std(CPALTT01JPM659N, "CPALTT01JPM659N")
) |> arrange(series_id, date)

#meta table
meta <- tibble::tribble(
  ~series_id,        ~country,        ~group,      ~nice_name,                          ~type,
  "JPNRGDPEXP",      "Japan",         "Output",    "Real GDP",                          "logdiff",
  "NGDPRSAXDCDEQ",   "Germany",       "Output",    "Real GDP",                          "logdiff",
  "GDPC1",           "United States", "Output",    "Real GDP",                          "logdiff",
  "LMJVTTNVJPQ647S", "Japan",         "Labor",     "Job Vacancies",                     "logdiff",
  "LMJVTTUVDEQ647S", "Germany",       "Labor",     "Job Vacancies",                     "logdiff",
  "LMJVTTUVUSM647S", "United States", "Labor",     "Job Vacancies",                     "logdiff",
  "SPASTT01DEQ661N", "Germany",       "Financial", "Stock Price Index (2015=100, NSA)", "logdiff",
  "SPASTT01JPQ661N", "Japan",         "Financial", "Stock Price Index (2015=100, NSA)", "logdiff",
  "SPASTT01USQ661N", "United States", "Financial", "Stock Price Index (2015=100, NSA)", "logdiff",
  "CPALTT01USM659N", "United States", "Prices",    "CPI inflation (y/y, NSA)",          "diff",
  "CPALTT01DEM659N", "Germany",       "Prices",    "CPI inflation (y/y, NSA)",          "diff",
  "CPALTT01JPM659N", "Japan",         "Prices",    "CPI inflation (y/y, NSA)",          "diff"
)

#build quarterly if not quarterly
start_q <- yearquarter("2000 Q1")

data_q <- data_long %>%
  mutate(fq = yearquarter(date)) %>%                 # quarter index
  group_by(series_id, fq) %>%                        # one row per series×quarter
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%  # monthly→quarterly
  filter(fq >= start_q) %>%                          # CLAMP HERE
  arrange(series_id, fq)

#check
data_q %>%
  group_by(series_id) %>%
  summarise(start = min(fq), end = max(fq), n_quarters = n(), .groups = "drop") %>%
  print(n = 100)

#join tables
data_q_meta <- data_q %>%
  inner_join(meta, by = "series_id") %>%
  arrange(country, group, nice_name, fq)

#detrending
cycles <- data_q_meta %>%
  group_by(country, group, nice_name, type) %>%
  arrange(fq, .by_group = TRUE) %>%
  mutate(
    cyc_log  = if_else(value > 0 & lag(value) > 0, log(value) - log(lag(value)), NA_real_),
    cyc_diff = value - lag(value),
    cyc      = case_when(
      type == "logdiff" ~ cyc_log,   # GDP, Stocks, Vacancies
      type == "diff"    ~ cyc_diff,  # CPI y/y
      TRUE              ~ value
    )
  ) %>%
  ungroup() %>%
  filter(!(type %in% c("logdiff","diff") & is.na(cyc))) %>%  # drop first-diff NAs
  select(country, group, nice_name, type, fq, cyc)

#confirm it worked
stopifnot(nrow(cycles) > 0)
dplyr::count(cycles, country, nice_name) %>% print(n = 50)

#summary statistics
wide_cyc <- cycles %>%
  select(country, fq, nice_name, cyc) %>%
  tidyr::pivot_wider(names_from = nice_name, values_from = cyc)

sd_tbl <- wide_cyc %>%
  group_by(country) %>%
  summarise(across(-fq, ~ sd(.x, na.rm = TRUE), .names = "sd_{.col}"), .groups = "drop")

post_cor <- wide_cyc %>%
  group_by(country) %>%
  summarise(across(-c(fq, `Real GDP`),
                   ~ cor(.x, `Real GDP`, use = "pairwise.complete.obs"),
                   .names = "corr_with_output_{.col}"),
            .groups = "drop")

summary_long <- sd_tbl %>%
  tidyr::pivot_longer(-country, names_to = "metric", values_to = "sd_q") %>%
  mutate(variable = sub("^sd_", "", metric)) %>%
  select(country, variable, sd_q) %>%
  left_join(
    post_cor %>%
      tidyr::pivot_longer(-country, names_to = "metric", values_to = "corr") %>%
      mutate(variable = sub("^corr_with_output_", "", metric)) %>%
      select(country, variable, corr),
    by = c("country","variable")
  ) %>%
  group_by(country) %>%
  mutate(rel_vol = sd_q / sd_q[variable == "Real GDP"]) %>%
  ungroup() %>%
  arrange(country, desc(sd_q))
#print them
print(sd_tbl)
print(post_cor)
print(summary_long, n = 50)

#save function
out_dir <- file.path(getwd(), "BC_Plots"); dir.create(out_dir, showWarnings = FALSE)
readr::write_csv(summary_long, file.path(out_dir, "summary_sd_corr_relvol.csv"))
cat("Wrote: ", file.path(out_dir, "summary_sd_corr_relvol.csv"), "\n")

if (requireNamespace("gt", quietly = TRUE)) {
  gt_tbl <- summary_long %>%
    mutate(across(c(sd_q, corr, rel_vol), ~ round(.x, 4))) %>%
    gt::gt(groupname_col = "country") %>%
    gt::tab_header(title = gt::md("**Volatility & Comovement with Output**")) %>%
    gt::cols_label(variable = "Variable",
                   sd_q = "SD (quarterly)",
                   corr = "Corr with Δlog GDP",
                   rel_vol = "Relative Vol (vs GDP)")
  gt::gtsave(gt_tbl, file.path(out_dir, "summary_sd_corr_relvol.html"))
  cat("Wrote: ", file.path(out_dir, "summary_sd_corr_relvol.html"), "\n")
}

#correlations
library(ggplot2)
library(stringr)
library(scales)

#long scatters
scatter_df <- wide_cyc %>%
  pivot_longer(
    cols = -c(country, fq, `Real GDP`),
    names_to = "variable",
    values_to = "x"
  ) %>%
  rename(y = `Real GDP`) %>%
  filter(!is.na(x), !is.na(y)) %>%
  filter(fq >= start_q)

# additional stats
panel_stats <- scatter_df %>%
  group_by(country, variable) %>%
  summarise(r = cor(x, y, use = "complete.obs"),
            n = dplyr::n(), .groups = "drop") %>%
  mutate(facet_label = paste0(variable, "  (ρ = ", number(r, accuracy = 0.01), ", n = ", n, ")"))

scatter_df <- scatter_df %>%
  left_join(panel_stats, by = c("country","variable"))

safe <- function(s) s %>%
  str_replace_all("[^A-Za-z0-9]+","_") %>%
  str_replace("^_|_$","")

base_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

# 1) charts per country function
for (cty in unique(scatter_df$country)) {
  p_cty <- scatter_df %>%
    filter(country == cty) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(alpha = 0.55, size = 1.4) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.6) +
    facet_wrap(vars(facet_label), scales = "free", ncol = 2) +
    labs(
      title = paste0(cty, ": Detrended correlations with Δlog Real GDP"),
      x = "Detrended variable (Δlog or Δ y/y)",
      y = "Δlog Real GDP"
    ) + base_theme
  f_out <- file.path(out_dir, paste0("scatter_", safe(cty), ".png"))
  ggsave(f_out, p_cty, width = 11, height = 8.5, dpi = 300)
  message("Wrote ", f_out)
}

#Single chart per country + variable
scatter_df %>%
  group_by(country, variable, facet_label) %>%
  group_walk(~{
    cty <- .y$country; var <- .y$variable
    p <- ggplot(.x, aes(x = x, y = y)) +
      geom_point(alpha = 0.6, size = 1.6) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
      labs(
        title = paste0(cty, " — ", .y$facet_label),
        x = paste0(var, " (detrended)"),
        y = "Δlog Real GDP"
      ) + base_theme
    f_out <- file.path(out_dir, paste0("scatter_", safe(cty), "_", safe(var), ".png"))
    ggsave(f_out, p, width = 7.5, height = 6, dpi = 300)
    message("Wrote ", f_out)
  })

# List outputs
print(list.files(out_dir, pattern = "summary_sd_corr_relvol|^scatter_.*\\.png$", full.names = TRUE))

#timeframes
us_gfc   <- list(name="GFC",   start=yearquarter("2007 Q4"), end=yearquarter("2009 Q2"))
us_covid <- list(name="COVID", start=yearquarter("2020 Q1"), end=yearquarter("2020 Q2"))

# order variables
var_order <- c("Real GDP","Job Vacancies","Stock Price Index (2015=100, NSA)","CPI inflation (y/y, NSA)")

#packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# index function
to_us_levels <- function(data_q_meta){
  data_q_meta %>%
    filter(country=="United States", nice_name %in% var_order) %>%
    arrange(nice_name, fq) %>%
    group_by(nice_name, type) %>%
    mutate(
      # for logdiff variables (GDP, vacancies, stocks): use log level
      lvl        = if_else(type=="logdiff", log(value), value),
      # pretty label
      var_short  = case_when(
        nice_name=="Real GDP" ~ "GDP",
        nice_name=="Job Vacancies" ~ "Vacancies",
        grepl("^Stock Price", nice_name) ~ "Stocks",
        grepl("^CPI inflation", nice_name) ~ "Inflation (y/y)",
        TRUE ~ nice_name
      )
    ) %>%
    ungroup()
}

#stats peak for different periods
measure_downturn <- function(df, start_q, end_q){
#before recession
  pre_df <- df %>% filter(fq < start_q)
  rec_df <- df %>% filter(fq >= start_q & fq <= end_q)
  post_df<- df %>% filter(fq > end_q)
  
  if(nrow(pre_df)==0 || nrow(rec_df)==0) return(tibble())
  
  pre_peak_q <- pre_df$fq[which.max(pre_df$lvl)]
  pre_peak_v <- max(pre_df$lvl, na.rm=TRUE)
  trough_q   <- rec_df$fq[which.min(rec_df$lvl)]
  trough_v   <- min(rec_df$lvl, na.rm=TRUE)
  
#recovery timing
  recov_q <- NA
  if(nrow(post_df)>0){
    hit <- post_df %>% filter(lvl >= pre_peak_v)
    if(nrow(hit)>0) recov_q <- min(hit$fq)
  }
  
  tibble(
    start_q = start_q, end_q=end_q,
    pre_peak_q=pre_peak_q, trough_q=trough_q, recov_q=recov_q,
    pre_peak_v=pre_peak_v, trough_v=trough_v
  )
}

# percent change helper
pretty_delta <- function(nm, v0, v1, type){
  if(type=="logdiff"){
    100*(v1 - v0) # % change
  } else {
    (v1 - v0)     # percentage points for inflation rate
  }
}






us_levels <- to_us_levels(data_q_meta)

one_recession_metrics <- function(rec){
  us_levels %>%
    group_by(nice_name, var_short, type) %>%
    group_modify(\(df, key){
      m <- measure_downturn(df, rec$start, rec$end)
      if(nrow(m)==0) return(tibble())
      # durations
      dur_q      <- as.integer(m$end_q - m$start_q) + 1
      ttr_q      <- ifelse(is.na(m$recov_q), NA_integer_, as.integer(m$recov_q - m$end_q))
      # severity
      sev        <- pretty_delta(key$nice_name, m$pre_peak_v, m$trough_v, key$type)
      tibble(
        event = rec$name,
        start_q=m$start_q, end_q=m$end_q,
        pre_peak_q=m$pre_peak_q, trough_q=m$trough_q, recov_q=m$recov_q,
        duration_q=dur_q, time_to_recover_q=ttr_q,
        severity = sev
      )
    }) %>% ungroup()
}

metrics_gfc   <- one_recession_metrics(us_gfc)
metrics_covid <- one_recession_metrics(us_covid)

metrics_us <- bind_rows(metrics_gfc, metrics_covid) %>%
  mutate(
    severity_label = ifelse(type=="logdiff",
                            sprintf("%+.1f%%", severity),
                            sprintf("%+.2f pp", severity))
  ) %>%
  select(event, var_short, start_q, end_q, pre_peak_q, trough_q, recov_q,
         duration_q, time_to_recover_q, severity_label)

# Save table
readr::write_csv(metrics_us, file.path(out_dir, "US_downturn_metrics_GFC_COVID.csv"))
print(metrics_us, n = 20)












event_plot <- function(rec, fname){
  base <- us_levels %>%
    filter(nice_name %in% var_order) %>%
    mutate(h = as.integer(fq - rec$start)) %>%
    filter(h >= -12 & h <= 12)
#this is for log-level to fix
  base_norm <- base %>%
    group_by(nice_name, type) %>%
    mutate(anchor = lvl[h==-1][1],
           y = if_else(type=="logdiff", 100*(lvl - anchor),  (lvl - lvl[h==-1][1]))) %>% # % for log, pp for CPI
    ungroup()
  
  p <- ggplot(base_norm, aes(h, y, color = nice_name)) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_hline(yintercept = 0, linewidth = 0.2) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~nice_name, scales="free_y", ncol = 2) +
    labs(title = paste0("United States — Event study around ", rec$name, " start (h=0)"),
         x = "quarters relative to recession start", 
         y = "Δ from h=-1 (%, or pp for CPI)") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none", panel.grid.minor = element_blank())
  
  ggsave(file.path(out_dir, fname), p, width = 10.5, height = 8, dpi = 300)
  message("Wrote ", file.path(out_dir, fname))
}

event_plot(us_gfc,   "US_event_study_GFC.png")
event_plot(us_covid, "US_event_study_COVID.png")





















#printing new plots


bdaf_plot <- function(rec, fname){
  df <- us_levels %>% filter(fq >= rec$start - 12 & fq <= rec$end + 12)
  
  p <- ggplot() +
# shades the recession
    annotate("rect",
             xmin = rec$start, xmax = rec$end, ymin = -Inf, ymax = Inf,
             fill = "grey50", alpha = 0.12) +
    geom_line(data = df %>% filter(type=="logdiff"),
              aes(fq, 100*lvl, color = var_short), linewidth=0.8) +
    geom_line(data = df %>% filter(type!="logdiff"),
              aes(fq, lvl, color = var_short), linewidth=0.8) +
    facet_wrap(~var_short, scales="free_y", ncol = 2) +
    labs(title = paste0("United States — Before / During / After: ", rec$name),
         x = NULL, y = "Level (log vars in %) / CPI y/y (pp)") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none", panel.grid.minor = element_blank())
  
  ggsave(file.path(out_dir, fname), p, width = 11, height = 8.5, dpi = 300)
}

bdaf_plot(us_gfc,   "US_BDAF_GFC.png")
bdaf_plot(us_covid, "US_BDAF_COVID.png")



















# (a) Side-by-side bars: severity & time-to-recover
cmp_tbl <- metrics_us %>%
  mutate(var_short = factor(var_short, levels = c("GDP","Vacancies","Stocks","Inflation (y/y)")))

p_sev <- ggplot(cmp_tbl, aes(var_short, as.numeric(gsub("[^-.0-9]","", severity_label)), fill = event)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Peak-to-trough severity: GFC vs COVID (US)",
       x = NULL, y = "Severity (%, CPI in pp)") +
  theme_minimal(base_size = 11)
ggsave(file.path(out_dir, "US_compare_severity.png"), p_sev, width = 9, height = 4.8, dpi = 300)

p_ttr <- ggplot(cmp_tbl, aes(var_short, time_to_recover_q, fill = event)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Time to recover (quarters): GFC vs COVID (US)",
       x = NULL, y = "Quarters after recession end") +
  theme_minimal(base_size = 11)
ggsave(file.path(out_dir, "US_compare_ttr.png"), p_ttr, width = 9, height = 4.8, dpi = 300)


