# =============================================================================
# NZ fuel price analysis: crude oil vs retail petrol/diesel across 6 periods
#
# Source: MBIE Weekly Fuel Price Monitoring, full series 2004-2026
# (weekly-table.csv, supplied directly by MBIE)
#
# Six periods, chosen from actual crude price movement (not news dates),
# alternating crisis / calm, split by New Zealand's supply-chain era:
#
#   1. 2020 COVID crash          2020-03-06 - 2020-06-05  (crisis, own refinery)
#   2. Calm, own-refinery era    2020-06-05 - 2022-02-18  (calm,   own refinery)
#   3. 2022 Russia/Ukraine war   2022-02-24 - 2022-08-31  (crisis, refinery closes mid-window)
#   4. 2025 US-China tariff shock 2025-04-02 - 2025-06-30 (crisis, import era)
#   5. Calm, import era          2025-07-10 - 2026-02-27  (calm,   import era)
#   6. 2026 Iran/US conflict     2026-02-28 - [latest date in file] (crisis, import era)
#
# NZ's only oil refinery (Marsden Point) supplied ~65-70% of national fuel
# demand until it closed -- last crude shipment 8 March 2022, refining ended
# 31 March 2022. Periods 1-2 are the "own refinery" era; periods 4-6 are the
# "100% imported refined product" era. Period 3 straddles the actual closure,
# so it's a structurally mixed period, not a clean comparison point.
#
# No sub-splitting into "first wave / second wave" or "rise / decline"
# phases within a period -- earlier drafts tried this and ran into too few
# data points per sub-phase to trust the result. Each of the 6 periods is
# treated as one window.
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# ---------------------------------------------------------------------------
# 1. LOAD
# ---------------------------------------------------------------------------
raw <- read.csv("weekly-table.csv", stringsAsFactors = FALSE)
raw$Date <- as.Date(raw$Date)
cat("Full file loaded:", nrow(raw), "rows,", as.character(min(raw$Date)),
    "to", as.character(max(raw$Date)), "\n\n")

# Period 6 (the ongoing crisis) ends at whichever week is most recent in the
# data file, not a hardcoded date -- so re-running this script after MBIE
# publishes a new week automatically extends period 6 instead of silently
# ignoring the new data (or worse, needing a manual date edit every time).
latest_date <- as.character(max(raw$Date))

periods <- list(
  list(id = "01_covid_2020",        name = "2020 COVID crash",
       start = "2020-03-06", end = "2020-06-05"),
  list(id = "02_calm_own_refinery", name = "Calm period (own-refinery era)",
       start = "2020-06-05", end = "2022-02-18"),
  list(id = "03_ukraine_2022",      name = "2022 Russia/Ukraine war",
       start = "2022-02-24", end = "2022-08-31"),
  list(id = "04_tariff_2025",       name = "2025 US-China tariff shock",
       start = "2025-04-02", end = "2025-06-30"),
  list(id = "05_calm_import_era",   name = "Calm period (import era)",
       start = "2025-07-10", end = "2026-02-27"),
  list(id = "06_iranus_2026",       name = "2026 Iran/US conflict",
       start = "2026-02-28", end = latest_date)
)

# ---------------------------------------------------------------------------
# 2. HELPER FUNCTIONS
# ---------------------------------------------------------------------------

# get_series(): crude (native NZD/bbl column -- no manual currency
# conversion needed, MBIE publishes it directly) + retail for ONE fuel.
get_series <- function(start, end, fuel = "Regular Petrol") {
  crude <- raw %>%
    filter(is.na(Fuel), Variable == "Dubai crude price", Unit == "NZD/bbl",
           Date >= as.Date(start), Date <= as.Date(end)) %>%
    select(Date, crude = Value)
  retail <- raw %>%
    filter(Fuel == fuel, Variable == "Adjusted retail price",
           Date >= as.Date(start), Date <= as.Date(end)) %>%
    select(Date, retail = Value)
  inner_join(crude, retail, by = "Date") %>% arrange(Date)
}

# get_series_both(): crude + BOTH petrol and diesel retail, long format,
# used for the "illustrate the data" price charts (point 3).
get_series_both <- function(start, end) {
  crude <- raw %>%
    filter(is.na(Fuel), Variable == "Dubai crude price", Unit == "NZD/bbl",
           Date >= as.Date(start), Date <= as.Date(end)) %>%
    select(Date, crude = Value)
  retail <- raw %>%
    filter(Fuel %in% c("Regular Petrol", "Diesel"), Variable == "Adjusted retail price",
           Date >= as.Date(start), Date <= as.Date(end)) %>%
    select(Date, Fuel, retail = Value)
  inner_join(retail, crude, by = "Date") %>% arrange(Date)
}

# get_margin(): weekly Importer cost + Importer margin for one fuel.
get_margin <- function(start, end, fuel) {
  raw %>%
    filter(Fuel == fuel, Date >= as.Date(start), Date <= as.Date(end),
           Variable %in% c("Importer cost", "Importer margin")) %>%
    pivot_wider(id_cols = Date, names_from = Variable, values_from = Value) %>%
    arrange(Date) %>%
    rename(importer_cost = `Importer cost`, importer_margin = `Importer margin`)
}

# lag_profile(): correlation between crude (shifted back k weeks) and
# retail (now), for EVERY tested lag k = 0..maxlag -- not just the winner.
# We show the full profile everywhere in this script rather than a single
# "best lag" number: a single number invites the objection that it's
# cherry-picked, while the full shape (sharp peak vs flat vs noisy) lets
# the reader judge how convincing the result actually is.
#
# This does NOT use R's built-in ccf(). ccf() centers/scales using the
# FULL series mean and variance regardless of lag, and divides by n (not by
# the number of pairs actually overlapping at that lag). On the short
# windows used here (13-90 weeks, testing lags that can be a meaningful
# fraction of that), this shrinks correlations at higher lags in a way that
# doesn't match a plain "correlate these specific pairs" calculation -- we
# hit a real case where ccf() and the direct method disagreed on which lag
# won. The direct method below is the standard Pearson correlation on
# whichever pairs are actually being compared at each lag.
#
# maxlag defaults to floor(n/3), capped at 10 weeks regardless of how large
# n is: testing a lag that eats too much of a short window leaves too few
# pairs to trust (guarded directly below too), and on the long calm windows
# (up to 90 weeks) floor(n/3) alone would test physically implausible lags
# like 25-30 weeks, which nobody would defend as a real pass-through delay.
lag_profile <- function(both, maxlag = NULL) {
  n <- nrow(both)
  if (is.null(maxlag)) maxlag <- min(10, max(1, floor(n / 3)))
  results <- data.frame(lag = 0:maxlag, r = NA_real_)
  for (k in 0:maxlag) {
    if (n - k < 5) next
    c_lag <- head(both$crude, n - k)
    r_now <- tail(both$retail, n - k)
    results$r[results$lag == k] <- round(cor(c_lag, r_now), 3)
  }
  na.omit(results)
}

best_lag <- function(profile) profile[which.max(profile$r), ]

# If the winning lag sits right at the edge of what was tested, and the
# correlation was climbing right up to that edge with no internal peak,
# that's not a real result -- it's the same artifact we hit earlier
# (short window, correlation keeps rising simply because we ran out of
# room to test further, not because that lag is genuinely best). In that
# case we fall back to lag=0 for reporting/plotting rather than present a
# misleading "best fit" at the boundary.
resolve_lag <- function(profile) {
  b <- best_lag(profile)
  if (b$lag == max(profile$lag) && nrow(profile) > 3) {
    cat(sprintf("  [no internal peak found -- correlation still rising at lag=%d, the edge of what was tested; falling back to lag=0 rather than report a boundary artifact]\n", b$lag))
    return(profile[profile$lag == 0, ])
  }
  b
}

# ---------------------------------------------------------------------------
# 3 & 4. FOR EACH PERIOD: lag/correlation tables (point 4) -- computed first
#         because the scatter charts (point 5) need each fuel's best lag.
# ---------------------------------------------------------------------------
period_results <- list()

for (p in periods) {
  cat(sprintf("\n================ %s (%s to %s) ================\n",
              p$name, p$start, p$end))

  petrol <- get_series(p$start, p$end, "Regular Petrol")
  diesel <- get_series(p$start, p$end, "Diesel")

  petrol_profile <- lag_profile(petrol)
  diesel_profile <- lag_profile(diesel)

  cat(sprintf("\nn = %d weeks\n", nrow(petrol)))
  cat("\n-- Regular Petrol: correlation at each lag (weeks) --\n")
  print(petrol_profile, row.names = FALSE)
  cat("\n-- Diesel: correlation at each lag (weeks) --\n")
  print(diesel_profile, row.names = FALSE)

  pb <- resolve_lag(petrol_profile)
  db <- resolve_lag(diesel_profile)
  cat(sprintf("\nBest fit -> Petrol: lag=%d (r=%.3f)   Diesel: lag=%d (r=%.3f)\n",
              pb$lag, pb$r, db$lag, db$r))

  period_results[[p$id]] <- list(
    petrol_lag = pb$lag, petrol_r = pb$r,
    diesel_lag = db$lag, diesel_r = db$r
  )
}

# ---------------------------------------------------------------------------
# 5. PRICE CHARTS -- one file per period, crude + petrol + diesel together
#    (point 3: just illustrating the MBIE data, no lag applied here)
# ---------------------------------------------------------------------------
for (p in periods) {
  d <- get_series_both(p$start, p$end) %>%
    group_by(Fuel) %>%
    mutate(retail_idx = retail / retail[1] * 100) %>%
    ungroup()

  crude_idx <- raw %>%
    filter(is.na(Fuel), Variable == "Dubai crude price", Unit == "NZD/bbl",
           Date >= as.Date(p$start), Date <= as.Date(p$end)) %>%
    arrange(Date) %>%
    mutate(Fuel = "Dubai Crude", retail_idx = Value / Value[1] * 100) %>%
    select(Date, Fuel, retail_idx)

  plot_data <- bind_rows(d %>% select(Date, Fuel, retail_idx), crude_idx)

  p_chart <- ggplot(plot_data, aes(x = Date, y = retail_idx, color = Fuel)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.3) +
    scale_color_manual(values = c("Dubai Crude" = "#D55E00", "Regular Petrol" = "#0072B2", "Diesel" = "#009E73")) +
    labs(title = paste0("Crude oil vs NZ retail: ", p$name),
         subtitle = paste0("Indexed to 100 at ", p$start, ". MBIE weekly data -- illustrative, no lag applied."),
         x = NULL, y = "Index (start = 100)", color = NULL) +
    theme_minimal(base_size = 12) + theme(legend.position = "top")

  fname <- paste0("charts/prices_", p$id, ".png")
  ggsave(fname, p_chart, width = 9, height = 5.5, dpi = 150)
  cat("Saved", fname, "\n")
}

# ---------------------------------------------------------------------------
# 6. SCATTER CHARTS -- one file per period, crude vs retail AT EACH FUEL'S
#    OWN best-fit lag (point 5). Petrol and diesel use their own lag from
#    Section 3/4 above -- these numbers match the printed tables exactly.
# ---------------------------------------------------------------------------
for (p in periods) {
  res <- period_results[[p$id]]

  petrol <- get_series(p$start, p$end, "Regular Petrol") %>%
    mutate(crude = lag(crude, n = res$petrol_lag), Fuel = "Regular Petrol") %>%
    filter(!is.na(crude))
  diesel <- get_series(p$start, p$end, "Diesel") %>%
    mutate(crude = lag(crude, n = res$diesel_lag), Fuel = "Diesel") %>%
    filter(!is.na(crude))

  scatter_data <- bind_rows(petrol, diesel)

  p_scatter <- ggplot(scatter_data, aes(x = crude, y = retail, color = Fuel)) +
    geom_point(size = 2.2, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
    scale_color_manual(values = c("Regular Petrol" = "#0072B2", "Diesel" = "#009E73")) +
    labs(
      title = paste0("Crude price vs retail price, at each fuel's own best-fit lag: ", p$name),
      subtitle = sprintf("Petrol: lag=%d weeks (r=%.2f)   |   Diesel: lag=%d weeks (r=%.2f)",
                          res$petrol_lag, res$petrol_r, res$diesel_lag, res$diesel_r),
      x = "Dubai Crude (NZD/bbl), lagged", y = "Retail price (NZ cents/litre)", color = NULL
    ) +
    theme_minimal(base_size = 12) + theme(legend.position = "top")

  fname <- paste0("charts/scatter_", p$id, ".png")
  ggsave(fname, p_scatter, width = 8, height = 5.5, dpi = 150)
  cat("Saved", fname, "\n")
}

# ---------------------------------------------------------------------------
# 7. MARGIN CHART -- last crisis only (period 6), petrol vs diesel together
# ---------------------------------------------------------------------------
# "Importer margin" = Adjusted Retail Price - Taxes - Importer cost: what's
# left for the retailer after the fuel itself and government taxes/levies.
p6 <- periods[[6]]
margin_data <- bind_rows(
  get_margin(p6$start, p6$end, "Regular Petrol") %>% mutate(Fuel = "Regular Petrol"),
  get_margin(p6$start, p6$end, "Diesel") %>% mutate(Fuel = "Diesel")
)

p_margin <- ggplot(margin_data, aes(x = Date, y = importer_margin, color = Fuel)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("Regular Petrol" = "#0072B2", "Diesel" = "#009E73")) +
  labs(
    title = paste0("Importer margin during ", p6$name, ": petrol vs diesel"),
    subtitle = "Both dip to ~zero at the sharpest cost spike; diesel later overshoots to ~2x normal",
    x = NULL, y = "Importer margin (NZ cents/litre)", color = NULL
  ) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")

ggsave("charts/margin_06_iranus_2026.png", p_margin, width = 9, height = 5.5, dpi = 150)
cat("Saved charts/margin_06_iranus_2026.png\n")

cat("\n=== Done. All tables printed above; all charts in ./charts/ ===\n")
